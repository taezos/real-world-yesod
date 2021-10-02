{-# LANGUAGE TemplateHaskell #-}
module Api.Model.User where

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

data UserLogin = UserLogin
  { userLoginEmail    :: Text
  , userLoginPassword :: Text
  } deriving ( Eq, Show )

data CreateUser = CreateUser
  { createUserFirstName :: Maybe Text
  , createUserLastName  :: Maybe Text
  , createUserEmail     :: Text
  , createUserUsername  :: Text
  , createUserPassword  :: Text
  , createUserBio       :: Maybe Text
  , createUserImageLink :: Maybe Text
  } deriving ( Eq, Show )

data UserProfile = UserProfile
  { userProfileId        :: UUID
  , userProfileEmail     :: Maybe Text
  , userProfileFirstName :: Maybe Text
  , userProfileLastName  :: Maybe Text
  , userProfileUsername  :: Text
  , userProfileBio       :: Maybe Text
  , userProfileImageLink :: Maybe Text
  } deriving ( Eq, Show )

data UserAuth = UserAuth
  { userAuthEmail    :: Text
  , userAuthToken    :: Text
  , userAuthUsername :: Text
  , userAuthBio      :: Maybe Text
  , userAuthImage    :: Maybe Text
  } deriving ( Eq, Show )

data UserUpdate = UserUpdate
  { userUpdateEmail    :: Maybe Text
  , userUpdateUsername :: Maybe Text
  , userUpdatePassword :: Maybe Text
  , userUpdateImage    :: Maybe Text
  , userUpdateBio      :: Maybe Text
  } deriving ( Eq, Show )

data UserWrapper a = UserWrapper
  { userWrapperUser :: a
  } deriving ( Eq, Show )

data ProfileWrapper a =  ProfileWrapper
  { profileWrapperProfile :: a
  } deriving ( Eq, Show )

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "ProfileWrapper" )
  } ''ProfileWrapper)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "UserUpdate" )
  } ''UserUpdate)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "UserWrapper" )
  } ''UserWrapper)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "UserLogin" )
  } ''UserLogin)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "UserProfile" )
  } ''UserProfile)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "CreateUser" )
  } ''CreateUser)

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "UserAuth" )
  } ''UserAuth)
