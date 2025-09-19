module Handler.Class.ApiHandler where

import Data.Either (Either)
import Foreign (MultipleErrors)
import HTTPure.Response (ResponseM)
import Type.Proxy (Proxy)

class ApiHandler :: forall k. k -> Constraint
class ApiHandler a where
  handle :: String -> Proxy a -> Either MultipleErrors ResponseM
