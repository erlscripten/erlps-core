{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erlps-core"
, dependencies =
  [ "arraybuffer"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "psci-support"
  , "rationals"
  , "spec"
  , "numbers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "ISC"
, repository = "https://github.com/erlscripten/erlps-core"
}
