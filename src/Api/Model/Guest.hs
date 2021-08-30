module Api.Model.Guest where

-- real-world-yesod
import           Import.NoFoundation

data GuestLogin = GuestLogin
  { guestLoginEmail    :: Text
  , guestLoginPassword :: Text
  } deriving ( Eq, Show )

data CreateGuest = CreateGuest
  { createGuestFirstName :: Maybe Text
  , createGuestLastName  :: Maybe Text
  , createGuestEmail     :: Text
  , createGuestPassword  :: Text
  } deriving ( Eq, Show )
