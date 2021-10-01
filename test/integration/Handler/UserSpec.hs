{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.UserSpec (spec) where

import           Api.Model.User
import           Database.Model.User
import           TestImport

-- aeson
import qualified Data.Aeson          as JSON

-- http-types
import           Network.HTTP.Types

-- unordered-containers
import qualified Data.HashMap.Strict as HS

spec :: Spec
spec = withApp $ do
  let userPasswordTxt = "password"
  let usernameTxt = "username"
  describe "UserSpec" $ do
    it "asserts no access to my-account for anonymous users" $ do
      get $ ProfileR ""
      statusIs 403

    it "will return a user with an authenticated request" $ do
      currentTime <- liftIO getCurrentTime
      void $ createUser usernameTxt userPasswordTxt currentTime
      mRes <- runDB $ selectFirst [ UserUsername ==. usernameTxt ] []
      case mRes of
        Nothing -> error "user does not exist"
        Just ( Entity key _ ) -> do
          authenticatedRequest key $ do
            setUrl $ ProfileR usernameTxt
          res <- getJsonResponse @( HashMap Text UserProfile )
          statusIs 200
          assertEq "response username should be equal to username input"
            ( fmap userProfileUsername $ HS.lookup "profile" res )
            $ Just usernameTxt

    it "will create a user" $ do
      let createUserInput = CreateUser
            { createUserFirstName = Nothing
            , createUserLastName  = Nothing
            , createUserEmail     = "test@test.com"
            , createUserUsername  = "username"
            , createUserPassword  = "password"
            , createUserBio       = Nothing
            , createUserImageLink = Nothing
            }

      request $ do
        setMethod "POST"
        setUrl UserRegisterR
        setRequestBody $ JSON.encode ( UserWrapper createUserInput )
        addRequestHeader (hContentType, "application/json")
      statusIs 200

    it "will login a user" $ do
      currentTime <- liftIO getCurrentTime
      void $ createUser usernameTxt userPasswordTxt currentTime
      let userLogin = UserLogin
            { userLoginEmail = "test@test.com"
            , userLoginPassword = userPasswordTxt
            }
      request $ do
        setMethod "POST"
        setUrl UserLoginR
        setRequestBody $ JSON.encode ( UserWrapper userLogin )
        addRequestHeader (hContentType, "application/json")
      statusIs 200

    it "will return the current user" $ do
      currentTime <- liftIO getCurrentTime
      userEntity <- createUser usernameTxt userPasswordTxt currentTime
      authenticatedRequest ( entityKey userEntity ) $ do
        setMethod "GET"
        setUrl CurrentUserR
        addRequestHeader (hContentType, "application/json")
      statusIs 200
      res <- getJsonResponse @( UserWrapper UserProfile )
      assertEq "user email" ( userProfileEmail . userWrapperUser $ res )
        $ Just "test@test.com"

    it "will update a user" $ do
      currentTime <- liftIO getCurrentTime
      userEntity <- createUser usernameTxt userPasswordTxt currentTime
      let defaultUser = UserUpdate
            { userUpdateEmail     = Nothing
            , userUpdateUsername  = Nothing
            , userUpdatePassword  = Nothing
            , userUpdateImage     = Nothing
            , userUpdateBio       = Nothing
            }

      authenticatedRequest ( entityKey userEntity ) $ do
        setMethod "PUT"
        setUrl CurrentUserR
        setRequestBody $ JSON.encode
          $ UserWrapper
          $ defaultUser { userUpdateEmail = Just "new-email@test.com" }
        addRequestHeader (hContentType, "application/json")

      statusIs 200

      res <- getJsonResponse @( UserWrapper UserProfile )

      assertEq "update email" ( userProfileEmail . userWrapperUser $ res )
        $ Just "new-email@test.com"

      assertEq  "only update email" ( userWrapperUser res )
        $ UserProfile
          { userProfileId         = unUserKey $ entityKey userEntity
          , userProfileEmail      = Just "new-email@test.com"
          , userProfileFirstName  = Nothing
          , userProfileLastName   = Nothing
          , userProfileUsername   = usernameTxt
          , userProfileBio        = Nothing
          , userProfileImageLink  = Nothing
          }

    it "will update user password" $ do
      currentTime <- liftIO getCurrentTime
      userEntity <- createUser usernameTxt userPasswordTxt currentTime
      let defaultUser = UserUpdate
            { userUpdateEmail     = Nothing
            , userUpdateUsername  = Nothing
            , userUpdatePassword  = Nothing
            , userUpdateImage     = Nothing
            , userUpdateBio       = Nothing
            }

      authenticatedRequest ( entityKey userEntity ) $ do
        setMethod "PUT"
        setUrl CurrentUserR
        setRequestBody $ JSON.encode
          $ UserWrapper
          $ defaultUser { userUpdatePassword = Just "new-password" }
        addRequestHeader (hContentType, "application/json")

      statusIs 200

      let userLogin = UserLogin
            { userLoginEmail = "test@test.com"
            , userLoginPassword = "new-password"
            }

      request $ do
        setMethod "POST"
        setUrl UserLoginR
        setRequestBody $ JSON.encode ( UserWrapper userLogin )
        addRequestHeader (hContentType, "application/json")
      statusIs 200
