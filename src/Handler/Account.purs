module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.Either (Either, isLeft)
import Data.Foldable (foldl)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.String.CodePoints (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Entity.Account (Account(..))
import Node.Buffer as Buffer
import Node.Crypto.Hash (Algorithm(..))
import Node.Crypto.Hash as Hash
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile, exists, readTextFile, writeTextFile)
import Parser.Account (accountParser)
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)
import Text.Parsing.Parser (ParseError, runParserT)

data CreateUserFailureReason
  = NotAuthorized
  | NotAuthenticated
  | AlreadyExists
  | FileIOError String

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists <- try $ exists accountsFile
  when (isLeft exists) do
    bsa <- bootstrapAccount
    writeTextFile ASCII accountsFile bsa
  accountsLines <- lines <$> readTextFile ASCII accountsFile
  pure $ sequence $ unwrap <<< flip runParserT accountParser <$> accountsLines

createAccount :: Account -> Aff (Either String Unit)
createAccount account =
  lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account)

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
  ]

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

hex :: Algorithm -> String -> Effect String
hex algorithm input = do
  buf <- Buffer.fromString input UTF8
  Hash.createHash algorithm >>= \h -> Hash.update h buf >>= Hash.digest >>= Buffer.toString Hex

passwordHashHex :: String -> String -> Aff String
passwordHashHex userName password =
  let salt = userNameSalt (3 * length userName) userName in
  liftEffect $ hex SHA512 $ password <> salt

userNameSeed :: String -> Seed
userNameSeed s = s
  # toCharArray
  <#> toCharCode
  # foldl (*) 1
  # mkSeed

userNameSalt :: Int -> String -> String
userNameSalt n userName = fromCharArray $
  sample (userNameSeed userName) n arbitrary
