{-# LANGUAGE RecordWildCards #-}
module Handler.Internal.Email where

-- case-insensitive
import           Data.CaseInsensitive ( CI )
import qualified Data.CaseInsensitive as CI

-- email-validate
import qualified Text.Email.Validate  as Email

-- real-world-yesod
import           Import.NoFoundation

-- persistent
import           Database.Persist.Sql
    ( PersistFieldSql (..)
    )

-- text
import qualified Data.Text            as T

newtype Email = Email
  { unEmail :: CI Text
  } deriving ( Eq, Show )

mkEmail :: Text -> Maybe Email
mkEmail email =
  if Email.isValid  $ encodeUtf8 email
  then Just $ Email $ CI.mk email
  else Nothing

instance PersistField Email where
  toPersistValue Email {..} = PersistText $ CI.original unEmail
  fromPersistValue ( PersistText txt ) =
    case mkEmail txt of
      Nothing    -> Left $ "Deserialized invalid email: " <> txt
      Just email -> Right email
  fromPersistValue v = Left
    $ "When trying to deserialize Email: expected PersistText, received: "
    <> ( T.pack $ show v )

instance PersistFieldSql Email where
  sqlType _ = SqlString

