module Handler.Common where

import           Import

-- aeson
import qualified Data.Aeson as JSON

postLoginDestR :: Handler Value
postLoginDestR = pure JSON.Null

getLogoutDestR :: Handler Value
getLogoutDestR = pure JSON.Null
