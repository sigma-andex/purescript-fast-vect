module Data.FastVect.FastVectSpec where

import Prelude

import Data.FastVect.Common as C
import Data.FastVect.FastVect as FV
import Data.FastVect.Sparse.Read as FVR
import Data.FastVect.Sparse.Write as FVW
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (foldl1, foldr1)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "FastVect" do
    describe "Data.FastVect.FastVect" do
      describe "fromArray" do
        it "should create a Vect from an Array" do
          let
            actualSuccess = FV.fromArray (C.term :: _ 3) [ "a", "b", "c" ]

            expectedSuccess = FV.append (FV.singleton "a") (FV.append (FV.singleton "b") (FV.singleton "c"))
            actualFail1 = FV.fromArray (C.term :: _ 4) [ "a", "b", "c" ]

            actualFail2 = FV.fromArray (C.term :: _ 2) [ "a", "b", "c" ]
          actualSuccess `shouldEqual` (Just expectedSuccess)
          actualFail1 `shouldEqual` Nothing
          actualFail2 `shouldEqual` Nothing

        it "should successfully acccess elements from a Vect" do
          let
            vect = FV.cons 1 $ FV.cons 2 $ FV.cons 3 $ FV.cons 4 FV.empty
          (foldl (+) 0 vect) `shouldEqual` 10
          (foldl1 (+) vect) `shouldEqual` 10
          (foldr1 (+) vect) `shouldEqual` 10
          (FV.head vect) `shouldEqual` 1
          --(head empty) `shouldEqual` 1 -- should not compile
          (FV.index (C.term :: _ 0) vect) `shouldEqual` 1
          (FV.index (C.term :: _ 3) vect) `shouldEqual` 4
          (FV.modify (C.term :: _ 3) (add 100) vect) `shouldEqual` (FV.cons 1 $ FV.cons 2 $ FV.cons 3 $ FV.cons 104 FV.empty)
          --(index (term :: _ 4) vect) `shouldEqual` 1 -- should not compile

          (FV.drop (C.term :: _ 4) vect) `shouldEqual` FV.empty
          (FV.drop (C.term :: _ 3) vect) `shouldEqual` (FV.singleton 4)
          --(drop (term :: _ 5) vect) `shouldEqual` (singleton 4) -- should not compile

          (FV.take (C.term :: _ 4) vect) `shouldEqual` vect
          (FV.take (C.term :: _ 3) vect) `shouldEqual` (FV.cons 1 $ FV.cons 2 $ FV.cons 3 FV.empty)
          --let _ = (take (term :: _ 5) vect) -- should not compile
          pure unit
        it "should adjust an Array to a Vect" do
          let
            expectedPad = [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3 ]

            actualPad = FV.adjust (C.term :: _ 10) 0 [ 1, 2, 3 ]

            expectedDrop = [ 1, 2, 3 ]

            actualDrop = FV.adjust (C.term :: _ 3) 0 [ 0, 0, 0, 0, 1, 2, 3 ]

            expectedEqual = [ 1, 2, 3, 4, 5 ]
            actualEqual = FV.adjust (C.term :: _ 5) 0 [ 1, 2, 3, 4, 5 ]

            expectedPadM = [ "", "", "", "", "a", "b", "c" ]
            actualPadM = FV.adjustM (C.term :: _ 7) [ "a", "b", "c" ]
          (FV.toArray actualPad) `shouldEqual` expectedPad
          (FV.toArray actualDrop) `shouldEqual` expectedDrop
          (FV.toArray actualEqual) `shouldEqual` expectedEqual
          (FV.toArray actualPadM) `shouldEqual` expectedPadM
        it "should apply" do
          let
            applies = FV.cons (add 1) $ FV.cons (add 42) $ FV.cons (mul 5) $ FV.cons (sub 6) FV.empty

            expectedApplies = FV.cons 6 $ FV.cons 47 $ FV.cons 25 $ FV.cons 1 FV.empty
            actualApplies = applies <*> pure 5

          actualApplies `shouldEqual` expectedApplies

        it "should be generated" do
          let
            generated = FV.generate (C.term :: _ 4) \i -> C.toInt i
            expectedGenerated = FV.cons 0 $ FV.cons 1 $ FV.cons 2 $ FV.cons 3 FV.empty
          generated `shouldEqual` expectedGenerated

        it "should be mapped with terms" do
          let
            vector = FV.cons 1 $ FV.cons 2 $ FV.cons 3 $ FV.cons 4 FV.empty

            mapped = FV.mapWithTerm (\i a -> C.toInt i + a) vector
            expectedMapped = FV.cons 1 $ FV.cons 3 $ FV.cons 5 $ FV.cons 7 FV.empty

          mapped `shouldEqual` expectedMapped
    describe "Data.FastVect.Sparse.Read" do
      describe "fromArray" do
        it "should create a Vect from an Array" do
          let
            actualSuccess = FVR.fromMap (C.term :: _ 3) $ Map.fromFoldable [ 0 /\ "a", 2 /\ "b", 1 /\ "c" ]

            expectedSuccess = FVR.append (FVR.singleton "a") (FVR.append (FVR.singleton "c") (FVR.singleton "b"))
            actualFail1 = FVR.fromMap (C.term :: _ 4) $ Map.fromFoldable [ 0 /\ "a", 22 /\ "b" ]

            actualFail2 = FVR.fromMap (C.term :: _ 2) $ Map.fromFoldable [ 0 /\ "a", 52 /\ "b" ]
          actualSuccess `shouldEqual` (Just expectedSuccess)
          actualFail1 `shouldEqual` Nothing
          actualFail2 `shouldEqual` Nothing

        it "should successfully acccess elements from a Vect" do
          let
            vect = FVR.cons 1 $ FVR.cons 2 $ FVR.cons 3 $ FVR.cons 4 FVR.empty
          (foldl (+) 0 vect) `shouldEqual` 10
          (FVR.head vect) `shouldEqual` (Just 1)
          --(head empty) `shouldEqual` 1 -- should not compile
          (FVR.index (C.term :: _ 0) vect) `shouldEqual` (Just 1)
          (FVR.index (C.term :: _ 3) vect) `shouldEqual` (Just 4)
          (FVR.modify (C.term :: _ 3) (add 100) vect) `shouldEqual` (FVR.cons 1 $ FVR.cons 2 $ FVR.cons 3 $ FVR.cons 104 FVR.empty)
          --(index (term :: _ 4) vect) `shouldEqual` 1 -- should not compile

          (FVR.drop (C.term :: _ 4) vect) `shouldEqual` FVR.empty
          (FVR.drop (C.term :: _ 3) vect) `shouldEqual` (FVR.singleton 4)
          --(drop (term :: _ 5) vect) `shouldEqual` (singleton 4) -- should not compile

          (FVR.take (C.term :: _ 4) vect) `shouldEqual` vect
          (FVR.take (C.term :: _ 3) vect) `shouldEqual` (FVR.cons 1 $ FVR.cons 2 $ FVR.cons 3 FVR.empty)
          --let _ = (take (term :: _ 5) vect) -- should not compile
          pure unit
        it "should apply" do
          let
            applies = FVR.cons (add 1) $ FVR.cons (add 42) $ FVR.cons (mul 5) $ FVR.cons (sub 6) FVR.empty

            expectedApplies = FVR.cons 6 $ FVR.cons 47 $ FVR.cons 25 $ FVR.cons 1 FVR.empty
            actualApplies = applies <*> pure 5

          actualApplies `shouldEqual` expectedApplies
    describe "Data.FastVect.Sparse.Write" do
      describe "fromArray" do
        it "should create a Vect from an Array" do
          let
            actualSuccess = FVW.fromMap (C.term :: _ 3) $ Map.fromFoldable [ 0 /\ "a", 2 /\ "b", 1 /\ "c" ]

            expectedSuccess = FVW.append (FVW.singleton "a") (FVW.append (FVW.singleton "c") (FVW.singleton "b"))
            actualFail1 = FVW.fromMap (C.term :: _ 4) $ Map.fromFoldable [ 0 /\ "a", 22 /\ "b" ]

            actualFail2 = FVW.fromMap (C.term :: _ 2) $ Map.fromFoldable [ 0 /\ "a", 52 /\ "b" ]
          actualSuccess `shouldEqual` (Just expectedSuccess)
          actualFail1 `shouldEqual` Nothing
          actualFail2 `shouldEqual` Nothing

        it "should successfully acccess elements from a Vect" do
          let
            vect = FVW.cons 1 $ FVW.cons 2 $ FVW.cons 3 $ FVW.cons 4 FVW.empty
          (foldl (+) 0 vect) `shouldEqual` 10
          (foldl (+) 0 (FVW.set (Proxy :: _ 0) 101 vect)) `shouldEqual` 110
          (traverse Just vect) `shouldEqual` Just vect
          (traverse Just $ (FVW.set (Proxy :: _ 0) 101 vect)) `shouldEqual` Just (FVW.set (Proxy :: _ 0) 101 vect)
          (FVW.head vect) `shouldEqual` (Just 1)
          --(head empty) `shouldEqual` 1 -- should not compile
          (FVW.index (C.term :: _ 0) vect) `shouldEqual` (Just 1)
          (FVW.index (C.term :: _ 3) vect) `shouldEqual` (Just 4)
          (FVW.modify (C.term :: _ 3) (add 100) vect) `shouldEqual` (FVW.cons 1 $ FVW.cons 2 $ FVW.cons 3 $ FVW.cons 104 FVW.empty)
          --(index (term :: _ 4) vect) `shouldEqual` 1 -- should not compile

          (FVW.drop (C.term :: _ 4) vect) `shouldEqual` FVW.empty
          (FVW.drop (C.term :: _ 3) vect) `shouldEqual` (FVW.singleton 4)
          --(drop (term :: _ 5) vect) `shouldEqual` (singleton 4) -- should not compile

          (FVW.take (C.term :: _ 4) vect) `shouldEqual` vect
          (FVW.take (C.term :: _ 3) vect) `shouldEqual` (FVW.cons 1 $ FVW.cons 2 $ FVW.cons 3 FVW.empty)
          --let _ = (take (term :: _ 5) vect) -- should not compile
          pure unit
        it "should apply" do
          let
            applies = FVW.cons (add 1) $ FVW.cons (add 42) $ FVW.cons (mul 5) $ FVW.cons (sub 6) FVW.empty

            expectedApplies = FVW.cons 6 $ FVW.cons 47 $ FVW.cons 25 $ FVW.cons 1 FVW.empty
            actualApplies = applies <*> pure 5

          actualApplies `shouldEqual` expectedApplies
