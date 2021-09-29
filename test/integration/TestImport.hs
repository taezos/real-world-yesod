{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module TestImport
    ( module TestImport
    , module X
    ) where

-- real-world-yesod
import           Application
    ( makeFoundation
    , makeLogWare
    )
import           Database.Model.User
import           Foundation
import           Handler.Internal.Email
import           Handler.Internal.Password

-- classy-prelude
import           ClassyPrelude                        as X hiding
    ( Handler
    , delete
    , deleteBy
    )

import           Database.Persist                     as X hiding ( get )
import           Database.Persist.Sql
    ( SqlPersistM
    , rawExecute
    , rawSql
    , runSqlPersistMPool
    , unSingle
    )

import           Database.Persist.SqlBackend.Internal ( connEscapeFieldName )

import           Foundation                           as X

import           Test.Hspec                           as X

import           Text.Shakespeare.Text                ( st )

-- yesod
import           Yesod.Auth                           as X
import           Yesod.Default.Config2
    ( loadYamlSettings
    , useEnv
    )
import           Yesod.Test                           as X

-- aeson
import           Data.Aeson

-- wai-test
import           Network.Wai.Test                     ( SResponse (..) )

-- bytestring
import qualified Data.ByteString.Lazy.Char8           as BL8

-- HUnit
import           Test.HUnit                           ( assertFailure )

-- yesod-core
import           Yesod.Core.Unsafe                    ( fakeHandlerGetLogger )

-- http-types
import           Network.HTTP.Types                   ( hAuthorization )

-- base
import           System.Environment

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  app <- getTestYesod
  liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query ( appConnPool app )

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  setEnv "JWT_SECRET" "secret"
  settings <- loadYamlSettings
    ["config/test-settings.yml", "config/settings.yml"]
    []
    useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  pure (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables = fmap ( connEscapeFieldName sqlBackend . FieldNameDB ) tables
  let query = "TRUNCATE TABLE " <> intercalate ", " escapedTables
  rawExecute query []

getTables :: DB [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |] []

    return $ map unSingle tables

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> Text -> UTCTime -> YesodExample App (Entity User)
createUser username password createdAt = runDB $ do
  mPass <- mkPassword password
  let pass = maybe ( error "password error" ) id mPass
  let email = maybe ( error "email error" ) id $ mkEmail "test@test.com"
  insertEntity User
    { userFirstName = Nothing
    , userLastName = Nothing
    , userEmail = email
    , userPassword = pass
    , userUsername = username
    , userCreatedAt = createdAt
    , userBio = Nothing
    , userImageLink = Nothing
    }

getJsonResponse :: FromJSON a => YesodExample App a
getJsonResponse =
  withResponse $ \SResponse{..} ->
  case fromJSON <$> decode simpleBody of
    Just ( Success a ) -> pure a
    _ -> do
      liftIO $ assertFailure $ "cannot decode JSON: " <> BL8.unpack simpleBody

authenticatedRequest :: UserId -> RequestBuilder App () -> YesodExample App ()
authenticatedRequest userId reqBuilder = do
  token <- runHandler $ userIdToToken userId
  request $ do
    addRequestHeader (hAuthorization, "Token " <> encodeUtf8 token)
    reqBuilder

testUserIdToToken :: UserId -> YesodExample App Text
testUserIdToToken userId = runHandler $ userIdToToken userId

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  fakeHandlerGetLogger appLogger app handler
