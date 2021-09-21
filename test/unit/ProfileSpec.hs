{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ProfileSpec where

-- real-world-yesod
import           Api.User
import           Api.Model.User
import qualified Auth.JWT                  as JWT
import           Database.Model.User
import           Handler.Internal.Email
import           Handler.Internal.Password
import           TestImport

-- persistent
import           Database.Persist

-- uuid
import qualified Data.UUID                 as UUID

-- text
import qualified Data.Text                 as T

-- aeson
import qualified Data.Aeson                as JSON

spec :: Spec
spec = do
  let userPasswordTxt = "password"
  let usernameTxt = "user"
  let userEmailTxt = "user@test.com"
  let userBioTxt = "I am a user"
  let txtUUID = "123e4567-e89b-12d3-a456-426614174000"

  describe "UserSpec" $ do
    it "will build a user profile from a user entity" $ do
      maybePassword <- mkPassword userPasswordTxt
      let userEntity = do
            password <- maybePassword
            userKey <- UUID.fromText txtUUID
            email <- mkEmail userEmailTxt
            pure $ Entity ( UserKey userKey ) $ User
              { userFirstName = Nothing
              , userLastName = Nothing
              , userEmail = email
              , userPassword = password
              , userCreatedAt = currentTestTime
              , userUsername = usernameTxt
              , userBio = Just userBioTxt
              , userImageLink = Nothing
              }
      mUserProfile <- toUserProfile userEntity
      case mUserProfile of
        Nothing -> error "no user profile found"
        Just userProfile -> do
          shouldBe ( UUID.toText $ userProfileId userProfile ) txtUUID

    it "will create a user record from CreateUser input" $ do
      let cUser = CreateUser
            { createUserFirstName = Nothing
            , createUserLastName  = Nothing
            , createUserEmail     = userEmailTxt
            , createUserUsername  = usernameTxt
            , createUserPassword  = userPasswordTxt
            , createUserBio       = Just userBioTxt
            , createUserImageLink = Nothing
            }
      password <- mkPassword userPasswordTxt
      let userRecordRes = toUserRecord cUser password currentTestTime
      case userRecordRes of
        Left err -> error $ T.unpack err
        Right User{..} -> do
          shouldBe ( emailToText userEmail ) "user@test.com"
          shouldBe userUsername usernameTxt

    it "will create a user with token" $ do
      maybePassword <- mkPassword userPasswordTxt
      let
        userIdToToken :: Monad m => UserId -> m Text
        userIdToToken userId = pure
          $ JWT.jsonToToken "secret" $ JSON.toJSON userId

      let userEntity = do
            password <- maybePassword
            userKey <- UUID.fromText txtUUID
            email <- mkEmail userEmailTxt
            pure $ Entity ( UserKey userKey ) $ User
              { userFirstName = Nothing
              , userLastName = Nothing
              , userEmail = email
              , userPassword = password
              , userCreatedAt = currentTestTime
              , userUsername = usernameTxt
              , userBio = Just userBioTxt
              , userImageLink = Nothing
              }

      mRes <- traverse ( toUserAuth userIdToToken "password" ) userEntity
      case mRes of
        Nothing -> error "no user found"
        Just res ->
          case res of
            Left err -> error $ T.unpack err
            Right user -> do
              shouldBe ( ( == 0 ) . length $ userAuthToken user ) False
              shouldBe ( JWT.tokenToJson "secret" ( userAuthToken user ) )
                $ Just ( JSON.String txtUUID )
