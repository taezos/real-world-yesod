module Auth.JWT where

-- classy-prelude
import           ClassyPrelude.Yesod

-- base
import           Data.Char           ( isSpace )

--
import           Web.JWT             as JWT

import qualified Data.Map            as Map

lookupToken :: MonadHandler m => m ( Maybe Text )
lookupToken = do
  mAuth <- lookupHeader "Authorization"
  pure $ extractToken . decodeUtf8 =<< mAuth

jsonToToken :: Text -> Value -> Text
jsonToToken jwtSecret userId =
  encodeSigned (JWT.hmacSecret jwtSecret) joseHeader
    mempty {unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, userId)]}
  where
    joseHeader :: JOSEHeader
    joseHeader = JOSEHeader
      { typ = Just "JWT"
      , cty = Nothing
      , alg = Just HS256
      , kid = Nothing
      }

tokenToJson :: Text -> Text -> Maybe Value
tokenToJson jwtSecret token = do
  jwt <- JWT.decodeAndVerifySignature ( JWT.hmacSecret jwtSecret ) token
  unClaimsMap ( JWT.unregisteredClaims ( JWT.claims jwt )) Map.!? jwtKey

jwtKey :: Text
jwtKey = "jwt"

extractToken :: Text -> Maybe Text
extractToken auth
  | toLower x == "token" = Just $ dropWhile isSpace y
  | otherwise = Nothing
  where ( x, y ) = break isSpace auth
