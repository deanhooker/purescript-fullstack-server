module Manager.Account where

import Prelude

import Crypto (passwordHashHex)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..))
import Utils (withAVar)

type Accounts = Map String Account

data CreateAccountError = CreateAccountAlreadyExists

startUp :: Array Account -> Aff (AVar Accounts)
startUp accounts = do
  AVar.new $ Map.fromFoldable $ toTuple <$> accounts where
        toTuple = \a@(Account { userName }) -> Tuple userName a

shutdown :: AVar Accounts -> Aff Unit
shutdown = void <<< AVar.take

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon accountsAVar userName password = do
  accounts <- AVar.read accountsAVar
  passwordHash' <- passwordHashHex userName password
  let account = Map.lookup userName accounts
  pure $ case account of
    Nothing -> Nothing
    (Just
     (Account { passwordHash })) ->
      if passwordHash == passwordHash'
      then account
      else Nothing

createAccount :: AVar Accounts -> Account -> Aff (Either CreateAccountError Unit)
createAccount accountsAVar newAccount@(Account { userName }) = do
  withAVar accountsAVar
    \accounts -> pure $
      if Map.member userName accounts
      then Tuple accounts (Left CreateAccountAlreadyExists)
      else Tuple (Map.insert userName newAccount accounts) (Right unit)

findAccount :: AVar Accounts -> String -> Aff (Maybe Account)
findAccount accountsAVar userName = do
  accounts <- AVar.read accountsAVar
  pure $ Map.lookup userName accounts

getAccounts :: AVar Accounts -> Aff (Array Account)
getAccounts accountsAVar = AVar.read accountsAVar >>= pure <<< fromFoldable <<< Map.values
