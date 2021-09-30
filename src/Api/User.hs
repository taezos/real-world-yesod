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
import           Handler.Common
import           Handler.Internal.Email
import           Handler.Internal.Password
import           Import.NoFoundation

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

  toUserProfile ( Just $ Entity userId updatedUser )

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
selectUserProfileIO dbFilter = toUserProfile =<< selectFirst dbFilter []

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
  => Maybe ( Entity User )
  -> m ( Maybe UserProfile )
toUserProfile mUserEntity =
  pure $ mUserEntity
    <&> \( Entity gKey User{..}) -> UserProfile
      { userProfileId = unUserKey gKey
      , userProfileEmail = Just $ emailToText userEmail
      , userProfileFirstName = userFirstName
      , userProfileLastName = userLastName
      , userProfileUsername = userUsername
      , userProfileBio = userBio
      , userProfileImageLink = userImageLink
      }

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
