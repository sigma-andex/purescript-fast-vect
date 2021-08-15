# purescript-fast-vect 🐆

Fast, type-safe vector libary for purescript. Inspired by [Idris](https://www.idris-lang.org/).

## tl;dr

https://user-images.githubusercontent.com/77549848/129489569-6ba0ef67-6bd1-477e-9a76-2577c7bfe4f8.mp4

## Installation

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

## Usage 

```purescript
import Data.FastVect.FastVect
import Prelude

import Data.FastVect.Add (term)
import Data.FastVect.FastVect as FV
import Type.Proxy (Proxy(..))

as :: Vect "300" String
as = FV.replicate (term :: _ "300") "a"

bs :: Vect "200" String
bs = FV.replicate (term :: _ "200") "b"

cs :: Vect "500" String
cs = FV.append as bs

ds :: Vect "2" String
ds = cs # FV.drop (term :: _ "299") # FV.take (term :: _ "2")

x :: String
x = FV.index (term :: _ "499") cs

y :: String
y = FV.head (FV.singleton "a")

big1 :: Vect "23923498230498230420" String
big1 = FV.replicate (term :: _ "23923498230498230420") "a"

big2 :: Vect "203948023984590684596840586" String
big2 = FV.replicate (term :: _ "203948023984590684596840586") "b"

big :: Vect "203948047908088915095071006" String
big = FV.append big1 big2
```
