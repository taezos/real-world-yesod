{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.UserSpec (spec) where

import           Api.Model.User
import           Database.Model.User
import           TestImport

-- aeson
import qualified Data.Aeson           as JSON

import           Network.HTTP.Types

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
          statusIs 200
          response <- getJsonResponse @UserProfile
          assertEq "response username should be equal to username input"
            ( userProfileUsername response )
            usernameTxt

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
        setRequestBody $ JSON.encode createUserInput
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
        setRequestBody $ JSON.encode userLogin
        addRequestHeader (hContentType, "application/json")
      statusIs 200
