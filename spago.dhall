{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "crypto"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "httpure"
  , "identity"
  , "lcg"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "parsing"
  , "posix-types"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "stringutils"
  , "transformers"
  , "unicode"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
