{ name = "fast-vect"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "partial"
  , "prelude"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
