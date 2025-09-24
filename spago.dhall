{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "crypto"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "httpure"
  , "identity"
  , "js-date"
  , "lcg"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "posix-types"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "record"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unicode"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
