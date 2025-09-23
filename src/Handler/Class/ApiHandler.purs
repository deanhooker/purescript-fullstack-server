module Handler.Class.ApiHandler where

import Control.Monad.Reader.Trans (ReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Foreign (MultipleErrors)
import HTTPure.Response (Response)
import Manager.Account (Accounts)
import Manager.Session (Sessions)
import Type.Proxy (Proxy)

type Handler = ReaderT HandlerEnv Aff Response

class ApiHandler :: forall k. k -> Constraint
class ApiHandler a where
  handle :: Proxy a -> String -> Either MultipleErrors Handler

type HandlerEnv =
  { accountsAVar :: AVar Accounts
  , sessionsAVar :: AVar Sessions
  }
