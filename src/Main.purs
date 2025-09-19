module Main where

import Prelude

import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Class.Console (log)
import HTTPure (Method(..))
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Node.Process (onSignal)

-- import HTTPure.Server (ServerM)


-- import Api.Logon (LogonRequest, LogonResponse(..), LogonResults(..))
-- import Data.Array ((!!))
-- import Data.Maybe (fromMaybe)
-- import Data.UUID (UUID, emptyUUID)


-- import Foreign.Generic (encodeJSON)


-- import HTTPure.Lookup ((!@))



-- import Test (test)

-- postRouter :: Request -> ResponseM
-- postRouter { path }
--   | path !@ 0 == "this" = HTTPure.ok $ fromMaybe "missing path[1]" $ path !! 1
--   | path !@ 0 == "that" = HTTPure.ok $ fromMaybe "missing path[2]" $ path !! 2
--   | otherwise           = HTTPure.notFound

router :: Request -> ResponseM
-- router { path: [ "goodbye" ] } = HTTPure.ok "goodbye"
-- router request@{ method }
--   | method == Get              = HTTPure.methodNotAllowed
--   | method == Post             = postRouter request
router _                       = HTTPure.notFound

port :: Int
port = 3000

main :: Effect Unit
main = do
  shutdown <- HTTPure.serve port router $ log $ "Server up and running on port: " <> show port

  let shutdownServer = do
        log "Shutting down server..."
        shutdown $ log "Server shutdown."
  onSignal SIGINT shutdownServer
  onSignal SIGTERM shutdownServer
  pure unit
-- main :: Effect Unit
-- main = test
