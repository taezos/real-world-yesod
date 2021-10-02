{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Api.User where

-- real-world-yesod
import           Api.Common
import           Api.Model.User
import           Database.Model.User
import           Database.Model.UserFollowing
import           Handler.Common
import           Handler.Internal.Email
import           Handler.Internal.Password
import           Import.NoFoundation

-- esqueleto
import qualified Database.Esqueleto.Legacy    as E

insertUserFollowing
  :: MonadIO m
  => UserId
  -> Text
  -> UTCTime
  -> SqlPersistT m ( Either Text UserProfile )
insertUserFollowing userId username createdAt = do
  mUserEntity <- selectFirst [ UserUsername ==. username ] []
  case mUserEntity of
    Nothing -> pure $ Left "User not found"
    Just userFollowingEntity@( Entity followingUserId _ ) -> do
      followings <- selectList
        [ UserFollowingUserId ==. userId
        , UserFollowingFollowingUserId ==. followingUserId
        ] []
      if length followings > 0
        then pure $ Left "User already following this username"
        else do
          insert_ $ UserFollowing userId followingUserId createdAt
          Right <$> toUserProfile userFollowingEntity

removeUserFollowing
  :: MonadIO m
  => UserId
  -> Text
  -> SqlPersistT m ( Either Text UserProfile )
removeUserFollowing userId username = do
  mUserEntity <- selectFirst [ UserUsername ==. username ] []
  case mUserEntity of
    Nothing -> pure $ Left "User not found"
    Just userFollowingEntity@( Entity followingUserId _ ) -> do
      followings <- selectList
        [ UserFollowingUserId ==. userId
        , UserFollowingFollowingUserId ==. followingUserId
        ] []
      if length followings == 0
        then pure $ Left "User not following this username"
        else do
          deleteUserFollowing followingUserId
          Right <$> toUserProfile userFollowingEntity

deleteUserFollowing :: MonadIO m => UserId -> SqlPersistT m ()
deleteUserFollowing uid = E.delete $ E.from $ \userFollowing ->
  E.where_ $ userFollowing E.^. UserFollowingFollowingUserId E.==. E.val uid

updateUserIO
  :: MonadIO m
  => UserId
  -> UserUpdate
  -> UTCTime
  -> SqlPersistT m ( Maybe UserProfile )
updateUserIO userId UserUpdate{..} now = do
  let mNewEmail = mkEmail =<< userUpdateEmail
  mNewPassword <- join <$> traverse mkPassword userUpdatePassword

  updatedUser  <- updateGet userId $ catMaybes
    [ maybeUpdate UserEmail mNewEmail
    , maybeUpdate UserUsername userUpdateUsername
    , maybeUpdate UserPassword mNewPassword
    , maybeUpdate UserImageLink ( Just userUpdateImage )
    , maybeUpdate UserBio ( Just userUpdateBio )
    , maybeUpdate UserUpdatedAt ( Just now )
    ]

  traverse toUserProfile ( Just $ Entity userId updatedUser )

selectUserProfileByUsernameIO
  :: MonadIO m
  => Text
  -> SqlPersistT m ( Maybe UserProfile )
selectUserProfileByUsernameIO username =
  selectUserProfileIO [ UserUsername ==. username ]

selectUserByIdIO
  :: MonadIO m
  => UserId
  -> SqlPersistT m ( Maybe UserProfile )
selectUserByIdIO userId = selectUserProfileIO [ UserId ==. userId ]

selectUserProfileIO
  :: MonadIO m
  => [ Filter User ]
  -> SqlPersistT m ( Maybe UserProfile )
selectUserProfileIO dbFilter =
  traverse toUserProfile =<< selectFirst dbFilter []

selectUserLoginIO
  :: MonadIO m
  => ( Key User -> m Text )
  -> UserLogin
  -> SqlPersistT m ( Either Text UserAuth )
selectUserLoginIO userIdToText UserLogin{..} = do
  mUserEntity <- maybe
    ( pure Nothing ) (\email -> selectFirst [ UserEmail ==. email ] [])
    $ mkEmail userLoginEmail
  case mUserEntity of
    Nothing -> pure $ Left invalidEmailPassMsg
    Just userEntity -> lift
      $ toUserAuth userIdToText userLoginPassword userEntity

insertUserIO
  :: MonadIO m
  => CreateUser
  -> UTCTime
  -> SqlPersistT m ( Either Text DbRecordKey )
insertUserIO cUser@CreateUser{..} createdOn = do
  maybePass <- mkPassword createUserPassword
  case toUserRecord cUser maybePass createdOn of
    Left errMsg -> pure $ Left errMsg
    Right user  -> Right . toRecordId <$> insert user
  where
    toRecordId :: Key User -> DbRecordKey
    toRecordId kUser = DbRecordKey ( unUserKey kUser )

toUserAuth
  :: Monad m
  => ( Key User -> m Text )
  -> Text
  -> Entity User
  -> m ( Either Text UserAuth )
toUserAuth userIdToText rawPassword ( Entity gKey User {..} ) = do
  token <- userIdToText gKey
  if verifyPassword rawPassword userPassword
    then pure $ Right UserAuth
      { userAuthEmail    = emailToText userEmail
      , userAuthToken    = token
      , userAuthUsername = userUsername
      , userAuthBio      = userBio
      , userAuthImage    = userImageLink
      }
    else pure $ Left invalidEmailPassMsg

toUserProfile
  :: Monad m
  => Entity User
  -> m UserProfile
toUserProfile ( Entity gKey User{..} ) = pure
  $ UserProfile
  { userProfileId = unUserKey gKey
  , userProfileEmail = Just $ emailToText userEmail
  , userProfileFirstName = userFirstName
  , userProfileLastName = userLastName
  , userProfileUsername = userUsername
  , userProfileBio = userBio
  , userProfileImageLink = userImageLink
  }

toUserRecord :: CreateUser -> Maybe Password -> UTCTime -> Either Text User
toUserRecord CreateUser{..} maybePass now = do
  email <- maybe ( Left invalidEmailPassMsg ) Right $ mkEmail createUserEmail
  case mkCreds maybePass of
    Nothing -> Left invalidEmailPassMsg
    Just password ->
      Right $ User
        { userFirstName = Nothing
        , userLastName = Nothing
        , userEmail = email
        , userUsername = createUserUsername
        , userPassword = password
        , userCreatedAt = now
        , userBio = createUserBio
        , userImageLink = createUserImageLink
        , userUpdatedAt = now
        }
  where
    mkCreds :: Maybe Password -> Maybe Password
    mkCreds mPass = do
      password <- mPass
      pure password

invalidEmailPassMsg :: Text
invalidEmailPassMsg = "Invalid Email and/or Password"
