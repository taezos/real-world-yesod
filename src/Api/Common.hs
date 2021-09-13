{-# LANGUAGE TemplateHaskell #-}
module Api.Common where

-- real-world-yesod
import           Import.NoFoundation

-- uuid
import           Data.UUID           ( UUID )

-- aeson
import           Data.Aeson.TH

-- base
import           Data.List           ( genericLength )

-- casing
import           Text.Casing         ( camel )

data DbRecordKey = DbRecordKey
  { dbRecordKeyId :: UUID
  } deriving ( Eq, Show )

$(deriveJSON defaultOptions
  { fieldLabelModifier = camel . drop ( genericLength "DbRecordKey" )
  } ''DbRecordKey)
