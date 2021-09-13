{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProfileSpec (spec) where

import           Api.Model.Guest
import           Database.Model.Guest
import           TestImport

spec :: Spec
spec = withApp $ do
  let guestPasswordTxt = "password"
  let guestUsernameTxt = "username"
  describe "ProfileSpec" $ do
    it "asserts no access to my-account for anonymous users" $ do
      get $ ProfileR ""
      statusIs 403

    it "will return a guest with an authenticated request" $ do
      currentTime <- liftIO getCurrentTime
      void $ createGuest guestUsernameTxt guestPasswordTxt currentTime
      mRes <- runDB $ selectFirst [ GuestUsername ==. guestUsernameTxt ] []
      case mRes of
        Nothing -> error "guest does not exist"
        Just ( Entity key _ ) -> do
          authenticatedRequest key $ do
            setUrl $ ProfileR guestUsernameTxt
          statusIs 200
          response :: GuestProfile <- getJsonResponse
          assertEq "response username should be equal to username input"
            ( guestProfileUsername response )
            guestUsernameTxt
