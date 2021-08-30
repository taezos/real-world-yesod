{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Foundation where

-- real-world-yesod
import qualified Auth.JWT             as JWT
import           Import.NoFoundation
import           Model.Guest

-- persistent
import           Database.Persist.Sql ( ConnectionPool, runSqlPool )

-- monad-logger
import           Control.Monad.Logger ( LogSource )

-- yesod
import           Yesod.Auth.Dummy
import qualified Yesod.Auth.Message   as AuthMsg
import           Yesod.Auth.OpenId    ( IdentifierType (Claimed), authOpenId )
import           Yesod.Core.Types     ( Logger )
import qualified Yesod.Core.Unsafe    as Unsafe

-- aeson
import           Data.Aeson

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
      case appRoot $ appSettings app of
          Nothing   -> getApprootText guessApproot app req
          Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
      120    -- timeout in minutes
      "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler
  -- function.
  -- The defaultYesodMiddleware adds the response header
  -- "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either
  -- a header or POST parameter.
  -- To add it, chain it together with the
  -- defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module
  -- of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  isAuthorized
      :: Route App  -- ^ The route the user is visiting.
      -> Bool       -- ^ Whether or not this is a "write" request.
      -> Handler AuthResult

  -- the profile route requires that the user is authenticated, so we
  -- delegate to that function
  isAuthorized ( AuthR _ ) _ = pure Authorized
  isAuthorized LogoutDestR _ = pure Authorized
  isAuthorized LoginDestR _  = pure Authorized
  isAuthorized ProfileR _    = isAuthenticated

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
      return $
      appShouldLogAll (appSettings app)
          || level == LevelWarn
          || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = GuestId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = LoginDestR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = LogoutDestR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate
    :: (MonadHandler m, HandlerSite m ~ App)
    => Creds App
    -> m ( AuthenticationResult App )
  authenticate _ =
    maybe ( UserError AuthMsg.InvalidLogin ) Authenticated <$> maybeAuthId

  maybeAuthId = do
    mToken <- JWT.lookupToken
    liftHandler $ maybe ( pure Nothing ) tokenToGuestId mToken

    -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
      -- Enable authDummy login if enabled.
    where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _  -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

tokenToGuestId :: Text -> Handler ( Maybe GuestId )
tokenToGuestId token = do
  jwtSecret <- getJwtSecret
  let mGuestId = fromJSON <$> JWT.tokenToJson jwtSecret token
  case mGuestId of
    Just ( Success guestId ) -> pure $ Just guestId
    _                        -> pure Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret =
  getsYesod $ appJwtSecret . appSettings
