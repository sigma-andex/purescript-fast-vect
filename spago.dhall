{ name = "fast-vect"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-fast-vect.git"
}
