# purescript-fast-vect 🐆

Fast, type-safe vector libary for purescript. Inspired by [Idris vectors](https://www.idris-lang.org/).

## tl;dr

https://user-images.githubusercontent.com/77549848/129489569-6ba0ef67-6bd1-477e-9a76-2577c7bfe4f8.mp4

## Usage

This library will soon be published to the package-set. In the meantime you can add it manually to your project, by adding the dependency to your `packages.dhall`

```dhall
let upstream =
      https://github.com/purescript/package-sets/releases/download/<<SOME-VERSION>>/packages.dhall
      with fast-vect =
        { dependencies = [ "arrays", "partial", "prelude", "tuples" ]
        , repo = "https://github.com/sigma-andex/purescript-fast-vect.git"
        , version = "cfa14c2"
        }

in  upstream
```

and then hit 
```bash
spago install fast-vect
```
