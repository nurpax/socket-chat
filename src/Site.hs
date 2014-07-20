{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Prelude hiding (mapM_)
import           Control.Concurrent
import qualified Control.Concurrent.STM as STM
import           Control.Applicative
import           Control.Monad.Trans (liftIO, lift)
import           Control.Monad.Trans.State hiding (state)
import           Control.Monad.Trans.Either
import           Control.Error.Safe (tryJust)
import           Control.Lens hiding ((.=), un)
import           Data.Aeson
import           Data.Foldable (mapM_)
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import qualified Network.EngineIO.Snap as EIOSnap
import qualified Network.SocketIO as SocketIO
import qualified Snap.CORS as CORS

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import qualified Db
import           Util

type H = Handler App App

maybeWhen :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeWhen Nothing _  = return ()
maybeWhen (Just a) f = f a

-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError =
  heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err

-- | Handle login submit.  Either redirect to '/' on success or give
-- an error.  We deliberately do NOT show the AuthFailure on the login
-- error, as we don't want to reveal to visitors whether or not the
-- login exists in the user database.
handleLoginSubmit :: H ()
handleLoginSubmit =
  with auth $ loginUser "login" "password" Nothing
    (\_ -> handleLogin . Just $ "Unknown login or incorrect password")
    (redirect "/")

-- | Logs out and redirects the user to the site index.
handleLogout :: H ()
handleLogout = with auth logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: H ()
handleNewUser =
  method GET (renderNewUserForm Nothing) <|> method POST handleFormSubmit
  where
    handleFormSubmit = do
      authUser <- with auth $ registerUser "login" "password"
      either (renderNewUserForm . Just) login authUser

    renderNewUserForm (err :: Maybe AuthFailure) =
      heistLocal (I.bindSplices errs) $ render "new_user"
      where
        errs = maybe noSplices splice err
        splice e = "newUserError" ## I.textSplice . T.pack . show $ e

    login user =
      logRunEitherT $
        lift (with auth (forceLogin user) >> redirect "/")

-- | Run actions with a logged in user or go back to the login screen
withLoggedInUser :: (Db.User -> H ()) -> H ()
withLoggedInUser action =
  with auth currentUser >>= go
  where
    go Nothing  =
      with auth $ handleLogin (Just "Must be logged in to view the main page")
    go (Just u) = logRunEitherT $ do
      uid  <- tryJust "withLoggedInUser: missing uid" (userId u)
      uid' <- hoistEither (reader T.decimal (unUid uid))
      return $ action (Db.User uid' (userLogin u))

handleCommentSubmit :: H ()
handleCommentSubmit = method POST (withLoggedInUser go)
  where
    go user = do
      c <- getParam "comment"
      maybeWhen c (withTop db . Db.saveComment user . T.decodeUtf8)
      redirect "/"

renderComment :: Monad m => Db.Comment -> I.Splice m
renderComment (Db.Comment _ saved text) =
  I.runChildrenWithText splices
  where
    splices = do
      "savedOn" ## T.pack . show $ saved
      "comment" ## text

-- | Render main page
mainPage :: H ()
mainPage = withLoggedInUser go
  where
    go :: Db.User -> H ()
    go user = do
      comments <- withTop db $ Db.listComments user
      heistLocal (splices comments) $ render "/index"
    splices cs =
      I.bindSplices ("comments" ## I.mapSplices renderComment cs)

------------------------------------------------------------

--------------------------------------------------------------------------------
data AddUser = AddUser T.Text

instance FromJSON AddUser where
  parseJSON = withText "AddUser" $ pure . AddUser


data NumConnected = NumConnected !Int

instance ToJSON NumConnected where
  toJSON (NumConnected n) = object [ "numUsers" .= n]


data NewMessage = NewMessage T.Text

instance FromJSON NewMessage where
  parseJSON = withText "NewMessage" $ pure . NewMessage


data Said = Said T.Text T.Text

instance ToJSON Said where
  toJSON (Said username message) = object
    [ "username" .= username
    , "message" .= message
    ]

data UserName = UserName T.Text

instance ToJSON UserName where
  toJSON (UserName un) = object [ "username" .= un ]


data UserJoined = UserJoined T.Text Int

instance ToJSON UserJoined where
  toJSON (UserJoined un n) = object
    [ "username" .= un
    , "numUsers" .= n
    ]

data ServerState = ServerState { ssNConnected :: STM.TVar Int }

server :: ServerState -> StateT SocketIO.RoutingTable IO ()
server state = do
  liftIO $ putStrLn "*** SERVER IO ***"
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let forUserName m = do
        u <- liftIO (STM.atomically (STM.tryReadTMVar userNameMVar))
        mapM_ m u

  SocketIO.on "new message" $ \(NewMessage message) -> do
    liftIO $ putStrLn "*** SERVER IO ***"
    forUserName $ \userName ->
      SocketIO.broadcast "new message" (Said userName message)

  SocketIO.on "add user" $ \(AddUser userName) -> do
    liftIO $ putStrLn "*** SERVER IO ***"
    n <- liftIO $ STM.atomically $ do
      n <- (+ 1) <$> STM.readTVar (ssNConnected state)
      STM.putTMVar userNameMVar userName
      STM.writeTVar (ssNConnected state) n
      return n

    SocketIO.emit "login" (NumConnected n)
    SocketIO.broadcast "user joined" (UserJoined userName n)

  SocketIO.on_ "typing" $ do
    liftIO $ putStrLn "*** SERVER IO ***"
    forUserName $ \userName ->
      SocketIO.broadcast "typing" (UserName userName)

  SocketIO.on_ "stop typing" $ do
    liftIO $ putStrLn "*** SERVER IO ***"
    forUserName $ \userName ->
      SocketIO.broadcast "stop typing" (UserName userName)



-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",        handleLoginSubmit)
         , ("/logout",       handleLogout)
         , ("/new_user",     handleNewUser)
         , ("/save_comment", handleCommentSubmit)
--         , ("/",             mainPage)
         , ("/chat.html",    serveFile "static/chat.html")
         , ("/static",       serveDirectory "static")
         ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  -- addRoutes must be called before heistInit - heist wants to
  -- serve "" itself which means our mainPage handler never gets a
  -- chance to get called.
  liftIO $ putStrLn "*** INIT SERVER IO ***"
  state <- liftIO $ ServerState <$> STM.newTVarIO 0
  xx <- liftIO $ SocketIO.initialize EIOSnap.snapAPI (return $ server state)
  addRoutes [("/socket.io/", CORS.applyCORS CORS.defaultOptions xx)]
  addRoutes routes
  h <- nestSnaplet "" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sess $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)

  -- Initialize auth that's backed by an sqlite database
  d <- nestSnaplet "db" db sqliteInit
  a <- nestSnaplet "auth" auth $ initSqliteAuth sess d

  -- Grab the DB connection pool from the sqlite snaplet and call
  -- into the Model to create all the DB tables if necessary.
  let c = sqliteConn $ d ^# snapletValue
  liftIO $ withMVar c $ \conn -> Db.createTables conn

  addAuthSplices h auth
  return $ App h s d a

