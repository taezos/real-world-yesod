{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.Guest where

-- real-world-yesod
import           Api.Guest
import           Api.Model.Guest
import           Import

postGuestRegisterR :: Handler Value
postGuestRegisterR = do
  createGuest <- requireCheckJsonBody @Handler @CreateGuest
  currentTime <- liftIO getCurrentTime
  res <- runDB $ insertGuestIO createGuest currentTime
  case res of
    Left errMsg    -> sendResponseStatus status404 errMsg
    Right guestKey -> pure $ toJSON guestKey

postGuestLoginR :: Handler Value
postGuestLoginR = do
  guestLogin <- requireCheckJsonBody @Handler @GuestLogin
  pure $ toJSON guestLogin
  -- res <- runDB $ selectGuestLoginIO guestIdToToken guestLogin
  -- case res of
  --   Left errMsg -> sendResponseStatus status404 errMsg
  --   Right guestAuth -> pure $ toJSON guestAuth
