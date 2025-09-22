module Manager.Account where

import Prelude

import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..))
import Handler.Account (passwordHashHex)

type Accounts = Map String Account

startUp :: Array Account -> Aff (AVar Accounts)
startUp accounts = do
  AVar.new $ fromFoldable $ toTuple <$> accounts where
        toTuple = \a@(Account { userName }) -> Tuple userName a

shutdown :: AVar Accounts -> Aff Unit
shutdown = void <<< AVar.take

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon accountsAVar userName password = do
  accounts <- AVar.read accountsAVar
  passwordHash' <- passwordHashHex userName password
  let account = lookup userName accounts
  pure $ case account of
    Nothing -> Nothing
    (Just
     (Account { passwordHash })) ->
      if passwordHash == passwordHash'
      then account
      else Nothing
