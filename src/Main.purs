module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (runReaderT)
import Data.Array (last, null)
import Data.Either (Either(..), either, hush)
import Data.Foldable (class Foldable, foldl)
import Data.JSDate (getTime, now, toUTCString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType)
import Data.MediaType.Common as MIME
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Posix.Signal (Signal(..))
import Data.String (Pattern(..), joinWith, length)
import Data.String.Common as String
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Account as AccountHandler
import Handler.Api.CreateUser (CreateUser)
import Handler.Api.Logoff (Logoff)
import Handler.Api.Logon (Logon)
import Handler.Api.QueryUsers (QueryUsers)
import Handler.Class.ApiHandler (HandlerEnv, Handler, handle)
import Manager.Account as AccountManager
import Manager.Session as SessionManager
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (onSignal)
import Record (delete)
import Type.Proxy (Proxy(..))

oneOf :: forall a e f. Foldable f => NonEmpty f (Either e a) -> Either e a
oneOf (x :| xs) = foldl (<|>) x xs

apiHandlers ∷ NonEmpty Array (String → Either MultipleErrors Handler)
apiHandlers =
            handle (Proxy :: _ Logon) :| [
            handle (Proxy :: _ Logoff)
          , handle (Proxy :: _ CreateUser)
          , handle (Proxy :: _ QueryUsers)
          ]

router :: HandlerEnv -> Request -> ResponseM
router env req@{ body, method }
  | method == HTTPure.Post =
    let handlers = apiHandlers <#> (_ $ body) in
    case hush $ oneOf handlers of
      Nothing -> HTTPure.badRequest body
      Just reader -> runReaderT reader env
  | method == HTTPure.Get = staticFiles req
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

staticRoot :: String
staticRoot = "../client/dist/"

staticFiles :: Request -> ResponseM
staticFiles { path } =
  let fileName = if null path then "index.html" else joinWith "/" path
      in
   mimeType fileName # maybe HTTPure.forbidden \mime -> do
     fileData <- try $ readTextFile ASCII $ staticRoot <> fileName
     fileData # either (const HTTPure.notFound) \fileData' ->
       let headers = HTTPure.headers
             [ Tuple "Content-Length" (show $ length fileData')
             , Tuple "Content-Type" $ unwrap mime
             ] in
       HTTPure.ok' headers fileData'

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

mimeTypes :: Map String MediaType
mimeTypes = Map.fromFoldable [
    Tuple "js" MIME.applicationJavascript
  , Tuple "html" MIME.textHTML
  , Tuple "jpg" MIME.imageJPEG
  , Tuple "png" MIME.imagePNG
  , Tuple "map" MIME.textPlain
  ]

mimeType :: String -> Maybe MediaType
mimeType fileName = do
  extension <- String.split (Pattern ".") fileName # last
  Map.lookup (String.toLower extension) mimeTypes
