module Crypto where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.String.CodePoints (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Crypto.Hash (Algorithm(..))
import Node.Crypto.Hash as Hash
import Node.Encoding (Encoding(..))
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)

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
