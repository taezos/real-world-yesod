module Handler.Logout where

-- real-world-yesod
import           Import

-- aeson
import qualified Data.Aeson as JSON

getLogoutDestR :: Handler Value
getLogoutDestR = pure JSON.Null
