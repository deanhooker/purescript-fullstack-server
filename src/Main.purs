module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array (head)
import Data.Either (Either(..), hush)
import Data.JSDate (getTime, now, toUTCString)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (oneOf, (:|))
import Data.Posix.Signal (Signal(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Account as AccountHandler
import Handler.Api.Logon (Logon)
import Handler.Class.ApiHandler (HandlerEnv, handle)
import Manager.Account as AccountManager
import Manager.Session as SessionManager
import Node.Process (onSignal)
import Record (delete)
import Type.Proxy (Proxy(..))

router :: HandlerEnv -> Request -> ResponseM
router env { body, method }
  | method == HTTPure.Post =
    case hush =<< (head $ oneOf $ ( handle (Proxy :: _ Logon) body ) :| []) of
      Nothing -> HTTPure.badRequest body
      Just reader -> runReaderT reader env
  | otherwise = HTTPure.methodNotAllowed

loggingRouter :: HandlerEnv -> Request -> ResponseM
loggingRouter env req = do
  startDate <- liftEffect now
  id <- liftEffect genUUID
  let idStr = " (" <> show id <> ")"
      ts dt = "(" <> toUTCString dt <> ") "
  log $ ts startDate <> "REQUEST: " <> show req <> idStr
  res <- router env req
  endDate <- liftEffect now
  let duration = getTime endDate - getTime startDate
  log $ ts endDate <> "RESPONSE: "
    <> (show $ delete (Proxy :: _ "writeBody") res)
    <> " [" <> show duration <> " ms]"
    <> idStr
  pure res

port :: Int
port = 3000

main :: Effect Unit
main = launchAff_ do
  loadResults <- AccountHandler.loadAccounts
  case loadResults of
    Left err -> log $ "Cannot load accounts: " <> show err
    Right accounts -> do
      accountsAVar <- AccountManager.startUp accounts
      sessionsAVar <- SessionManager.startUp
      liftEffect $ do
        shutdown <- HTTPure.serve port (loggingRouter { accountsAVar, sessionsAVar })
                    $ log $ "Server up and running on port: " <> show port
        let shutdownServer = launchAff_ do
              log "Shutting down server..."
              SessionManager.shutdown sessionsAVar
              AccountManager.shutdown accountsAVar
              liftEffect $ shutdown $ log "Server shutdown."
        onSignal SIGINT shutdownServer
        onSignal SIGTERM shutdownServer
  pure unit
