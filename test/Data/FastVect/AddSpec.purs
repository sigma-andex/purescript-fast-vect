module Data.FastVect.AddSpec where

import Data.FastVect.Add (class Add)
import Data.FastVect.ToInt (toInt)
import Prelude (Unit)
import Prim.Symbol (class Cons)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec =
  describe "Data.FastVect.Add" do
    describe "Add typeclass" do
      it "should add numbers at the typelevel" do
        let
          test3 ∷ ∀ augend addend carry sum. Add augend addend carry sum ⇒ Proxy augend → Proxy addend → Proxy sum
          test3 _ _ = Proxy

          test4 ∷ ∀ augend addend carry sum. Add augend addend carry sum ⇒ Proxy augend → Proxy sum → Proxy addend
          test4 _ _ = Proxy

          test5 ∷ ∀ augend addend carry sum result. Cons carry sum result ⇒ Add augend addend carry sum ⇒ Proxy augend → Proxy addend → Proxy result
          test5 _ _ = Proxy

          test6 ∷ ∀ augend addend carry sum result. Cons carry sum result ⇒ Add augend addend carry sum ⇒ Proxy augend → Proxy result → Proxy addend
          test6 _ _ = Proxy

          test6b ∷ ∀ augend addend carry sum. Add augend addend carry sum ⇒ Proxy augend → Proxy carry → Proxy sum → Proxy addend
          test6b _ _ _ = Proxy

          term ∷ ∀ t. Proxy t
          term = Proxy

          z ∷ Proxy "3"
          z = test3 (term ∷ _ "1") (term ∷ _ "2")

          zz ∷ Proxy "33"
          zz = test3 (term ∷ Proxy "11") (term ∷ Proxy "22")

          a ∷ Proxy "1"
          a = test4 (term ∷ Proxy "1") (term ∷ Proxy "2")

          aa ∷ Proxy "11"
          aa = test4 (term ∷ Proxy "11") (term ∷ Proxy "22")

          bb ∷ Proxy "176"
          bb = test5 (term ∷ Proxy "99") (term ∷ Proxy "77")

          cc ∷ Proxy "77"
          cc = test6 (term ∷ Proxy "99") (term ∷ Proxy "176")

          ccc ∷ Proxy "77"
          ccc = test6b (term ∷ Proxy "99") (term ∷ Proxy "1") (term ∷ Proxy "76")
        true `shouldEqual` true
