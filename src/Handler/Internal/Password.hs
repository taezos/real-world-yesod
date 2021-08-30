{-# LANGUAGE RecordWildCards #-}
module Handler.Internal.Password where

-- real-world-yesod
import           Import.NoFoundation

-- persistent
import           Database.Persist.Sql ( PersistFieldSql (..) )

-- text
import qualified Data.Text            as T

newtype Password = Password
  { unPassword :: Text
  } deriving ( Eq, Show )

instance PersistField Password where
  toPersistValue Password {..} = PersistText unPassword
  fromPersistValue ( PersistText txt ) = Right $ Password txt
  fromPersistValue p = Left
    $ "When trying to deserialize Password: expected PersistText, received: "
    <> ( T.pack $ show p )

instance PersistFieldSql Password where
  sqlType _ = SqlString
