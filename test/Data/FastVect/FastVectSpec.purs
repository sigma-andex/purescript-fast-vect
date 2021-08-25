module Data.FastVect.FastVectSpec where

import Data.FastVect.FastVect
import Typelevel.Arithmetic.Add (term)
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
      it "should adjust an Array to a Vect" do
        let
          expectedPad = [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3 ]

          actualPad = adjust (term ∷ _ "10") 0 [ 1, 2, 3 ]

          expectedDrop = [ 1, 2, 3 ]

          actualDrop = adjust (term ∷ _ "3") 0 [ 0, 0, 0, 0, 1, 2, 3 ]

          expectedEqual = [ 1, 2, 3, 4, 5 ]
          actualEqual = adjust (term ∷ _ "5") 0 [ 1, 2, 3, 4, 5 ]

          expectedPadM = [ "", "", "", "", "a", "b", "c" ]
          actualPadM = adjustM (term ∷ _ "7") [ "a", "b", "c" ]
        (toArray actualPad) `shouldEqual` expectedPad
        (toArray actualDrop) `shouldEqual` expectedDrop
        (toArray actualEqual) `shouldEqual` expectedEqual
        (toArray actualPadM) `shouldEqual` expectedPadM
