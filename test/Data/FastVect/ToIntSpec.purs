module Data.FastVect.ToIntSpec where

import Data.FastVect.ToInt (toInt)
import Prelude (Unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec ∷ Spec Unit
spec =
  describe "Data.FastVect.ToInt" do
    describe "toInt" do
      it "should convert a symbol to an int" do
        let
          actual ∷ Int
          actual = toInt (Proxy ∷ Proxy "23423423")
          expected = 23423423
        actual `shouldEqual` expected
