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
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-fast-vect.git"
}
