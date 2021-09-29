{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

-- classy-prelude
import           Import.NoFoundation

-- persistent
import           Database.Persist.Sql  ( PersistFieldSql (..) )

-- uuid
import           Data.UUID             ( UUID )
import qualified Data.UUID             as UUID

-- bytestring
import qualified Data.ByteString.Char8 as B8

instance PersistField UUID where
  toPersistValue = toPersistValue . UUID.toString
  fromPersistValue ( PersistLiteral t ) =
    case UUID.fromString $ B8.unpack t of
      Nothing   -> Left "Invalid UUID"
      Just uuid -> Right uuid
  fromPersistValue _ = Left "Not PersistLiteral"

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  fromPathPiece = UUID.fromText
  toPathPiece = UUID.toText
