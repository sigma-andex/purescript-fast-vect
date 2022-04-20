{ name = "fast-vect"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "maybe"
  , "partial"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "typelevel-arithmetic"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-fast-vect.git"
}
