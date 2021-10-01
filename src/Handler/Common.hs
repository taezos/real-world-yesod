module Handler.Common where

import           Import

-- aeson
import qualified Data.Aeson as JSON

postLoginDestR :: Handler Value
postLoginDestR = pure JSON.Null

getLogoutDestR :: Handler Value
getLogoutDestR = pure JSON.Null

maybeUpdate
  :: PersistField t
  => EntityField v t
  -> Maybe t
  -> Maybe ( Update v )
maybeUpdate label = fmap ( label =. )
