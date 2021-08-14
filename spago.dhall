{ name = "fast-vect"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "integers"
  , "prelude"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
