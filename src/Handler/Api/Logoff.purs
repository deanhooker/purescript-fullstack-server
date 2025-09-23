module Handler.Api.Logoff where

import Prelude

import Control.Monad.Reader.Trans (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.Logoff (LogoffRequest(..), LogoffResponse(..), LogoffResults(..))
import Data.Maybe (Maybe(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Session (removeSession, verifySession)

data Logoff = Logoff

instance apiHandlerLogoff :: ApiHandler Logoff where
  handle _ = handleApi handler

handler :: LogoffRequest -> Handler
handler (LogoffRequest { authToken }) = do
  { sessionsAVar } <- ask
  verifiedSession <- lift $ verifySession sessionsAVar authToken
  response <- case verifiedSession of
    Nothing -> pure $ LogoffResponse LogoffResultsFailure
    Just _ -> do
      _ <- lift $ removeSession sessionsAVar authToken
      pure $ LogoffResponse LogoffResultsSuccess
  HTTPure.ok $ encodeJSON response
