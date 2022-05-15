{ name = "fast-vect"
, dependencies =
  [ "arrays"
  , "foldable-traversable"
  , "maybe"
  , "partial"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-fast-vect.git"
}
