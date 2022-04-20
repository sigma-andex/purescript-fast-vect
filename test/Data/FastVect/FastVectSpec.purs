module Data.FastVect.FastVectSpec where

import Data.FastVect.FastVect

import Data.Maybe (Maybe(..))
import Prelude (Unit, discard, pure, unit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec =
  describe "Data.FastVect.FastVect" do
    describe "fromArray" do
      it "should create a Vect from an Array" do
        let
          actualSuccess ∷ Maybe (Vect 3 String)
          actualSuccess = fromArray (term ∷ _ 3) [ "a", "b", "c" ]

          expectedSuccess = append (singleton "a") (append (singleton "b") (singleton "c"))
          actualFail1 ∷ Maybe (Vect 4 String)
          actualFail1 = fromArray (term ∷ _ 4) [ "a", "b", "c" ]

          actualFail2 ∷ Maybe (Vect 2 String)
          actualFail2 = fromArray (term ∷ _ 2) [ "a", "b", "c" ]
        actualSuccess `shouldEqual` (Just expectedSuccess)
        actualFail1 `shouldEqual` Nothing
        actualFail2 `shouldEqual` Nothing

      it "should successfully acccess elements from a Vect" do
        let 
          vect = 1 : 2 : 3 : 4 : empty
        (head vect) `shouldEqual` 1
        --(head empty) `shouldEqual` 1 -- should not compile
        (index (term :: _ 0) vect) `shouldEqual` 1
        (index (term :: _ 3) vect) `shouldEqual` 4
        --(index (term :: _ 4) vect) `shouldEqual` 1 -- should not compile

        (drop (term :: _ 4) vect) `shouldEqual` empty
        (drop (term :: _ 3) vect) `shouldEqual` (singleton 4)
        --(drop (term :: _ 5) vect) `shouldEqual` (singleton 4) -- should not compile

        (take (term :: _ 4) vect) `shouldEqual` vect
        (take (term :: _ 3) vect) `shouldEqual` (1 : 2 : 3 : empty)
        --let _ = (take (term :: _ 5) vect) -- should not compile
        pure unit
      it "should adjust an Array to a Vect" do
        let
          expectedPad = [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3 ]

          actualPad = adjust (term ∷ _ 10) 0 [ 1, 2, 3 ]

          expectedDrop = [ 1, 2, 3 ]

          actualDrop = adjust (term ∷ _ 3) 0 [ 0, 0, 0, 0, 1, 2, 3 ]

          expectedEqual = [ 1, 2, 3, 4, 5 ]
          actualEqual = adjust (term ∷ _ 5) 0 [ 1, 2, 3, 4, 5 ]

          expectedPadM = [ "", "", "", "", "a", "b", "c" ]
          actualPadM = adjustM (term ∷ _ 7) [ "a", "b", "c" ]
        (toArray actualPad) `shouldEqual` expectedPad
        (toArray actualDrop) `shouldEqual` expectedDrop
        (toArray actualEqual) `shouldEqual` expectedEqual
        (toArray actualPadM) `shouldEqual` expectedPadM
