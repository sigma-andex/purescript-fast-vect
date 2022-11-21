module Test.Data.FastVect.FastVectSpec.MatrixSpec where

import Prelude

import Data.FastVect.Common (term)
import Data.FastVect.Common as C
import Data.FastVect.Common as Common
import Data.FastVect.FastVect ((:))
import Data.FastVect.FastVect as FV
import Data.FastVect.FastVect.Matrix as FM
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (foldl1, foldr1)
import Data.Traversable (foldl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "FastVect" do
    describe "Data.FastVect.FastVect.Matrix" do
      describe "fromArray" do
        it "should create a Matrix from an Array of Array" do
          let
            testArr :: Array (Array Int)
            testArr = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]

            actualSuccess :: Maybe (FM.Matrix 3 2 Int)
            actualSuccess = FM.fromArrayArray testArr

            expectedSuccess = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty

            actualFail :: Maybe (FM.Matrix 3 3 Int)
            actualFail = FM.fromArrayArray testArr

          actualSuccess `shouldEqual` (Just expectedSuccess)
          actualFail `shouldEqual` Nothing

        it "should successfully acccess elements from a Matrix" do
          let
            matrix :: FM.Matrix 3 2 Int
            matrix = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty
          (foldl (+) 0 matrix) `shouldEqual` 21
          (foldl1 (+) matrix) `shouldEqual` 21
          (foldr1 (+) matrix) `shouldEqual` 21
          (FM.index (C.term :: _ 0) (C.term :: _ 0) matrix) `shouldEqual` 1
          (FM.index (C.term :: _ 2) (C.term :: _ 1) matrix) `shouldEqual` 6
          (FM.modify (C.term :: _ 1) (C.term :: _ 1) (add 100) matrix) `shouldEqual` (FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 105 : 6 : FV.empty) : FV.empty)

        it "should apply" do
          let
            applies = FM.Matrix $ (add 1 : add 2 : add 3 : FV.empty) : (add 4 : add 5 : add 6 : FV.empty) : FV.empty
            matrix = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty

            expectedApplies = FM.Matrix $ (2 : 4 : 6 : FV.empty) : (8 : 10 : 12 : FV.empty) : FV.empty
            actualApplies = applies <*> matrix

          actualApplies `shouldEqual` expectedApplies

        it "should be generated" do
          let
            generated = FM.generate (term :: _ 3) (term :: _ 2) \i j -> Common.toInt i + Common.toInt j
            matrix = FM.Matrix $ (0 : 1 : 2 : FV.empty) : (1 : 2 : 3 : FV.empty) : FV.empty

          generated `shouldEqual` matrix

        it "should be mapped with terms" do
          let
            matrix = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty
            mapped = FM.mapWithTerm (\i j x -> Common.toInt i + Common.toInt j + x) matrix
            expected = FM.Matrix $ (1 : 3 : 5 : FV.empty) : (5 : 7 : 9 : FV.empty) : FV.empty

          mapped `shouldEqual` expected

        it "should successfully transform" do
          let
            matrix :: FM.Matrix 3 2 Int
            matrix = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty

            vect :: FV.Vect 2 Int
            vect = 1 : 2 : FV.empty

            expectedTransformedVect :: FV.Vect 3 Int
            expectedTransformedVect = 9 : 12 : 15 : FV.empty

            actualTransformedVect :: FV.Vect 3 Int
            actualTransformedVect = FM.transform matrix vect

          actualTransformedVect `shouldEqual` expectedTransformedVect

        it "should successfully product" do
          let
            matrix :: FM.Matrix 3 2 Int
            matrix = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty

            matrix2 :: FM.Matrix 2 2 Int
            matrix2 = FM.Matrix $ (1 : 2 : FV.empty) : (3 : 4 : FV.empty) : FV.empty

            expectedProductMatrix :: FM.Matrix 3 2 Int
            expectedProductMatrix = FM.Matrix $ (9 : 12 : 15 : FV.empty) : (19 : 26 : 33 : FV.empty) : FV.empty

            actualProductMatrix :: FM.Matrix 3 2 Int
            actualProductMatrix = matrix `FM.product` matrix2

          actualProductMatrix `shouldEqual` expectedProductMatrix

        it "should successfully transpose" do
          let
            matrix :: FM.Matrix 3 2 Int
            matrix = FM.Matrix $ (1 : 2 : 3 : FV.empty) : (4 : 5 : 6 : FV.empty) : FV.empty

            expectedTransposedMatrix :: FM.Matrix 2 3 Int
            expectedTransposedMatrix = FM.Matrix $ (1 : 4 : FV.empty) : (2 : 5 : FV.empty) : (3 : 6 : FV.empty) : FV.empty

            actualTransposedMatrix :: FM.Matrix 2 3 Int
            actualTransposedMatrix = FM.transpose matrix

          actualTransposedMatrix `shouldEqual` expectedTransposedMatrix

        it "should successfully trace" do
          let
            matrix :: FM.Matrix 2 2 Int
            matrix = FM.Matrix $ (1 : 2 : FV.empty) : (3 : 4 : FV.empty) : FV.empty

            expectedTracedVector :: FV.Vect 2 Int
            expectedTracedVector = 1 : 4 : FV.empty

            actualTracedVector :: FV.Vect 2 Int
            actualTracedVector = FM.traced matrix

          actualTracedVector `shouldEqual` expectedTracedVector

        it "should successfully dot-product" do
          let
            vect :: FV.Vect 2 Int
            vect = 1 : 2 : FV.empty

            vect2 :: FV.Vect 2 Int
            vect2 = 3 : 4 : FV.empty

            expectedDotProduct :: Int
            expectedDotProduct = 11

            actualDotProduct :: Int
            actualDotProduct = FM.dotProduct vect vect2

          actualDotProduct `shouldEqual` expectedDotProduct

        it "should successfully outer-product" do
          let
            vect :: FV.Vect 2 Int
            vect = 1 : 2 : FV.empty

            vect2 :: FV.Vect 2 Int
            vect2 = 3 : 4 : FV.empty

            expectedOuterProduct :: FM.Matrix 2 2 Int
            expectedOuterProduct = FM.Matrix $ (3 : 6 : FV.empty) : (4 : 8 : FV.empty) : FV.empty

            actualOuterProduct :: FM.Matrix 2 2 Int
            actualOuterProduct = FM.outerProduct vect vect2

          actualOuterProduct `shouldEqual` expectedOuterProduct

        it "should successfully make diag matrix" do
          let
            vect :: FV.Vect 2 Int
            vect = 1 : 2 : FV.empty

            expectedDiagMatrix :: FM.Matrix 2 2 Int
            expectedDiagMatrix = FM.Matrix $ (1 : 0 : FV.empty) : (0 : 2 : FV.empty) : FV.empty

            actualDiagMatrix :: FM.Matrix 2 2 Int
            actualDiagMatrix = FM.diag vect

          actualDiagMatrix `shouldEqual` expectedDiagMatrix
