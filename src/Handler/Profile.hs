{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Handler.Profile where

-- real-world-yesod
import           Api.Guest
import           Import

getProfileR :: Text -> Handler Value
getProfileR username =
  fmap toJSON <$> runDB $ selectGuestProfileByUsernameIO username
