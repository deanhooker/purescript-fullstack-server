module Handler.Api.QueryUsers where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.QueryUsers (QueryUsersFailureReason(..), QueryUsersRequest(..), QueryUsersResponse(..), QueryUsersResults(..))
import Data.Either (either, note)
import Entity.Account (Account(..))
import Entity.Session (Session(..))
import Entity.User (User(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Account (findAccount, getAccounts)
import Manager.Session (verifySession)
import Record (delete)
import Type.Prelude (Proxy(..))
import Utils (liftSuccess)

data QueryUsers = QueryUsers

instance apiHandlerQueryUsers :: ApiHandler QueryUsers where
  handle _ = handleApi handler

handler :: QueryUsersRequest -> Handler
handler (QueryUsersRequest { authToken }) = do
  { accountsAVar, sessionsAVar } <- ask
  result <- lift $ runExceptT do
    (Session { userName }) <- verifySession sessionsAVar authToken
      <#> note NotAuthenticated # liftSuccess
    (Account { admin }) <- findAccount accountsAVar userName
      <#> note NotAuthorized # liftSuccess
    unless admin $ throwError NotAuthorized
    accounts <- lift $ getAccounts accountsAVar
    let users = (\(Account account) -> User $ delete (Proxy :: _ "passwordHash") account) <$> accounts
    pure $ QueryUsersResultsSuccess { users }
  let ok = HTTPure.ok <<< encodeJSON <<< QueryUsersResponse
  result # either
    (ok <<< \reason -> QueryUsersResultsFailure { reason })
    ok
