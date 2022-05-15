let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220513/packages.dhall sha256:1ed784f37ae6131d99acd542d058d5ce39954ccaacc3adba5cc7cf1549d2bffa

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220507/packages.dhall
        sha256:cf54330f3bc1b25a093b69bff8489180c954b43668c81288901a2ec29a08cc64

let additions =
      { typelevel-arithmetic =
        { dependencies =
          [ "tuples"
          ]
        , repo = "https://github.com/sigma-andex/purescript-typelevel-arithmetic.git"
        , version = "v0.1.0"
        }
      }

in  upstream // additions
