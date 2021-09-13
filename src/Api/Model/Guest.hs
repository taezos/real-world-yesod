{-# LANGUAGE TemplateHaskell #-}
module Api.Model.Guest where

-- real-world-yesod
import           Import.NoFoundation

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- base
import           Data.List           ( genericLength )

-- casing
import           Text.Casing         ( camel )

-- uuid
import           Data.UUID           ( UUID )

data GuestLogin = GuestLogin
  { guestLoginEmail    :: Text
  , guestLoginPassword :: Text
  } deriving ( Eq, Show )

data CreateGuest = CreateGuest
  { createGuestFirstName :: Maybe Text
  , createGuestLastName  :: Maybe Text
  , createGuestEmail     :: Maybe Text
  , createGuestUsername  :: Text
  , createGuestPassword  :: Text
  , createGuestBio       :: Maybe Text
  , createGuestImageLink :: Maybe Text
  } deriving ( Eq, Show )

data GuestProfile = GuestProfile
  { guestProfileGuestId   :: UUID
  , guestProfileEmail     :: Maybe Text
  , guestProfileFirstName :: Maybe Text
  , guestProfileLastName  :: Maybe Text
  , guestProfileUsername  :: Text
  , guestProfileBio       :: Maybe Text
  , guestProfileImageLink :: Maybe Text
  } deriving ( Eq, Show )

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "GuestLogin" )
  } ''GuestLogin)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "GuestProfile" )
  } ''GuestProfile)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "CreateGuest" )
  } ''CreateGuest)
