{ name = "fast-vect"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "integers"
  , "partial"
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
