{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erlps-core"
, dependencies =
  [ "arraybuffer"
  , "base58"
  , "bigints"
  , "console"
  , "effect"
  , "integers"
  , "lists"
  , "node-buffer"
  , "psci-support"
  , "purescript-numerics"
  , "rationals"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
