{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erlps-core"
, dependencies =
  [ "aff"
  , "arraybuffer"
  , "arrays"
  , "avar"
  , "bigints"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "rationals"
  , "spec"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "uint"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "ISC"
, repository = "https://github.com/erlscripten/erlps-core"
}
