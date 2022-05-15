{ name = "fast-vect"
, dependencies =
  [ "aff"
  , "arrays"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "spec"
  , "spec-discovery"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-fast-vect.git"
}
