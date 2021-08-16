module Data.FastVect.FastVectSpec where

import Data.FastVect.FastVect
import Data.FastVect.Add (term)
import Data.Maybe (Maybe(..))
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec =
  describe "Data.FastVect.FastVect" do
    describe "fromArray" do
      it "should create a Vect from an Array" do
        let
          actualSuccess ∷ Maybe (Vect "3" String)
          actualSuccess = fromArray (term ∷ _ "3") [ "a", "b", "c" ]

          expectedSuccess = append (singleton "a") (append (singleton "b") (singleton "c"))
          actualFail1 ∷ Maybe (Vect "4" String)
          actualFail1 = fromArray (term ∷ _ "4") [ "a", "b", "c" ]

          actualFail2 ∷ Maybe (Vect "2" String)
          actualFail2 = fromArray (term ∷ _ "2") [ "a", "b", "c" ]
        actualSuccess `shouldEqual` (Just expectedSuccess)
        actualFail1 `shouldEqual` Nothing
        actualFail2 `shouldEqual` Nothing
