{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.Profile where

-- real-world-yesod
import           Api.User
import           Import

getProfileR :: Text -> Handler Value
getProfileR username = do
  mProfile <- runDB $ selectUserProfileByUsernameIO username
  pure $ object [ "profile" .= mProfile ]
