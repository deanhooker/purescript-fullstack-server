module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Crypto (passwordHashHex)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Entity.Account (Account(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, exists, readTextFile, writeTextFile)
import Parser.Account (accountsParser)
import Text.Parsing.Parser (ParseError, runParserT)

data CreateUserFailureReason
  = NotAuthorized
  | NotAuthenticated
  | AlreadyExists
  | FileIOError String

data CreateAccountError = CreateAccountFileError String

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists' <- try $ exists accountsFile
  let exists = case exists' of
        Left _  -> false
        Right b -> b
  unless exists do
    bsa <- bootstrapAccount
    writeTextFile ASCII accountsFile bsa
  fileData <- readTextFile ASCII accountsFile
  pure $ unwrap $ runParserT fileData accountsParser

createAccount :: Account -> Aff (Either CreateAccountError Unit)
createAccount account =
  lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account)
  <#> lmap CreateAccountFileError

accountToCSV :: Account -> String
accountToCSV (Account
              { userName
              , passwordHash
              , temporaryPassword
              , admin
              , firstName
              , lastName }) =
  joinWith ","
  [ userName
  , passwordHash
  , show temporaryPassword
  , show admin
  , firstName
  , lastName
  ] <> "\n"

accountsFile :: String
accountsFile = "accounts.csv"

-- userName,passwordHash,temporaryPassword,admin,firstName,lastName --
bootstrapAccount :: Aff String
bootstrapAccount = do
  let username = "admin"
      password = "placeholder"
  passwordHex <- passwordHashHex username password
  pure $ accountToCSV (Account
                       { userName: username
                       , passwordHash: passwordHex
                       , temporaryPassword: true
                       , admin: true
                       , firstName: "Joe"
                       , lastName: "Admin"})
