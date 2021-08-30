{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module TestImport
    ( module TestImport
    , module X
    ) where

-- real-world-yesod
import           Application           ( makeFoundation, makeLogWare )
import           Model.Guest

-- classy-prelude
import           ClassyPrelude         as X hiding ( Handler, delete, deleteBy )

import           Database.Persist      as X hiding ( get )
import           Database.Persist.Sql
    ( SqlPersistM
    , connEscapeName
    , rawExecute
    , rawSql
    , runSqlPersistMPool
    , unSingle
    )
import           Foundation            as X

import           Test.Hspec            as X

import           Text.Shakespeare.Text ( st )

-- yesod
import           Yesod.Auth            as X
import           Yesod.Core.Unsafe     ( fakeHandlerGetLogger )
import           Yesod.Default.Config2 ( loadYamlSettings, useEnv )
import           Yesod.Test            as X


runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  app <- getTestYesod
  liftIO $ runDBWithApp app query

runDbWithApp :: App -> SqlPersistM a -> IO a
runDbWithApp app query = runSqlPersistMPool query ( appConnPool app )

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
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
wipeDB app = runDbWithApp app $ do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables = fmap ( connEscapeName sqlBackend . DBName ) tables
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

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity Guest -> YesodExample App ()
authenticateAs (Entity _ u) = do
  request $ do
    setMethod "POST"
    addPostParam "email" $ guestEmail u
    setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
createUser :: Text -> YesodExample App (Entity Guest)
createUser ident = runDB $ do
  user <- insertEntity Guest {}
  return user
