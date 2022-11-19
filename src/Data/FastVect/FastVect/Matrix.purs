module Data.FastVect.FastVect.Matrix
  ( Matrix(..)
  , diag
  , dotProduct
  , empty
  , fromArrayArray
  , fromVect
  , fromVectArray
  , fromVectVect
  , generate
  , index
  , mapWithTerm
  , modify
  , outerMap
  , outerProduct
  , product
  , set
  , singleton
  , toArrayArray
  , toVect
  , toVectArray
  , toVectVect
  , trace
  , traced
  , transform
  , transpose
  )
  where

import Prelude

import Control.Apply (lift2)
import Data.FastVect.Common as Common
import Data.FastVect.Common.Matrix as CommonM
import Data.FastVect.FastVect (Vect)
import Data.FastVect.FastVect as V
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe)
import Data.Reflectable (class Reflectable)
import Data.Semigroup.Foldable (class Foldable1, foldMap1, foldl1, foldr1)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, sum, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Prim.Int (class Compare)
import Prim.Ordering (GT)

newtype Matrix h w a = Matrix (V.Vect w (V.Vect h a))

instance (Show a, Compare h Common.NegOne GT, Compare w Common.NegOne GT) => Show (Matrix h w a) where
  show m = show $ toArrayArray $ m

derive instance Eq a => Eq (Matrix h w a)
derive instance Ord a => Ord (Matrix h w a)

instance Functor (Matrix h w) where
  map f (Matrix v) = Matrix $ map (map f) v

instance FunctorWithIndex (Tuple Int Int) (Matrix h w) where
  mapWithIndex f (Matrix v) = Matrix $ mapWithIndex (\i -> mapWithIndex (\j -> f (Tuple i j))) v

instance Apply (Matrix h w) where
  apply (Matrix f) (Matrix v) = Matrix $ lift2 apply f v

instance (Compare h Common.NegOne GT, Reflectable h Int, Compare w Common.NegOne GT, Reflectable w Int) ⇒ Applicative (Matrix h w) where
  pure = replicate Common.term Common.term

instance Foldable (Matrix h w) where
  foldl f z (Matrix v) = foldl (foldl f) z v
  foldr f z (Matrix v) = foldr (flip $ foldr f) z v
  foldMap f (Matrix v) = foldMap (foldMap f) v

instance FoldableWithIndex (Tuple Int Int) (Matrix h w) where
  foldlWithIndex f z (Matrix v) = foldlWithIndex (\i -> foldlWithIndex (\j -> f (Tuple i j))) z v
  foldrWithIndex f z (Matrix v) = foldrWithIndex (\i -> flip $ foldrWithIndex (\j -> f (Tuple i j))) z v
  foldMapWithIndex f (Matrix v) = foldMapWithIndex (\i -> foldMapWithIndex (\j -> f (Tuple i j))) v

instance (Compare h Common.Zero GT, Compare w Common.Zero GT) => Foldable1 (Matrix h w) where
  foldl1 f (Matrix v) = foldl1 f $ map (foldl1 f) v
  foldr1 f (Matrix v) = foldr1 f $ map (foldr1 f) v
  foldMap1 f (Matrix v) = foldMap1 (foldMap1 f) v

instance Traversable (Matrix h w) where
  traverse f (Matrix v) = Matrix <$> traverse (traverse f) v
  sequence (Matrix v) = Matrix <$> traverse sequence v

instance TraversableWithIndex (Tuple Int Int) (Matrix h w) where
  traverseWithIndex f (Matrix v) = Matrix <$> traverseWithIndex (\i -> traverseWithIndex (\j -> f (Tuple i j))) v

index :: forall h w i j elem. CommonM.Index Matrix h w i j elem
index _ _ (Matrix m) = V.index (Common.term :: _ i) $ V.index (Common.term :: _ j) m

replicate :: forall h w a. CommonM.Replicate Matrix h w a
replicate _ _ a = Matrix $ V.replicate Common.term $ V.replicate Common.term a

generate :: forall h w elem. CommonM.Generate Matrix h w elem
generate _ _ f = Matrix $ V.generate Common.term \j -> V.generate Common.term \i -> f i j

mapWithTerm :: forall h w elem elem'. CommonM.MapWithTerm Matrix h w elem elem'
mapWithTerm f (Matrix m) = Matrix $ V.mapWithTerm (\j -> V.mapWithTerm (\i -> f i j)) m

toVectVect
  :: forall h w elem
   . Matrix h w elem
  -> Vect w (Vect h elem)
toVectVect (Matrix v) = v

toVectArray
  :: forall h w elem
   . Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Matrix h w elem
  -> Array (Vect h elem)
toVectArray (Matrix m) = V.toArray m

toVect
  :: forall h elem
   . Compare h Common.NegOne GT
  => Matrix h 1 elem
  -> Vect h elem
toVect (Matrix m) = V.index (Common.term :: _ 0) m

toArrayArray :: forall h w elem.
  Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Matrix h w elem
  -> Array (Array elem)
toArrayArray (Matrix m) = V.toArray $ map V.toArray m

fromVectVect
  :: forall h w elem
   . Vect w (Vect h elem)
  -> Matrix h w elem
fromVectVect = Matrix

fromVectArray
  :: forall h w elem
   . Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Reflectable w Int
  => Array (Vect h elem)
  -> Maybe (Matrix h w elem)
fromVectArray arr = Matrix <$> V.fromArray Common.term arr

fromVect :: forall h elem. Compare h Common.NegOne GT => Vect h elem -> Matrix h 1 elem
fromVect v = Matrix $ V.singleton v

fromArrayArray :: forall h w elem. Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Reflectable w Int
  => Compare h Common.NegOne GT
  => Reflectable h Int
  => Array (Array elem)
  -> Maybe (Matrix h w elem)
fromArrayArray arr = fromVectArray =<< (sequence $ map (V.fromArray Common.term) arr)

singleton :: forall elem. elem -> Matrix 1 1 elem
singleton a = Matrix $ V.singleton $ V.singleton a

transpose :: forall h w elem. CommonM.Transpose Matrix h w elem
transpose m = generate Common.term Common.term \i j -> index j i m

dotProduct :: forall h elem. CommonM.DotProduct Vect h elem
dotProduct v1 v2 = sum $ lift2 (*) v1 v2

outerMap :: forall h w elemH elemW elem. CommonM.OuterMap Vect Matrix h w elemH elemW elem
outerMap f v1 v2 = Matrix $ map (\elemW -> map (\elemH -> f elemH elemW) v1) v2

outerProduct :: forall h w elem. CommonM.OuterProduct Vect Matrix h w elem
outerProduct = outerMap (*)

diag :: forall h elem. CommonM.Diag Vect Matrix h elem
diag v = generate Common.term Common.term
  \i j -> if Common.toInt i == Common.toInt j then V.index i v else zero

traced :: forall h elem. CommonM.Traced Vect Matrix h elem
traced m = V.generate Common.term \i -> index i i m

trace :: forall h elem. CommonM.Trace Matrix h elem
trace m = sum $ traced m

transform :: forall h w elem. CommonM.Transform Vect Matrix h w elem
transform m v =
  let
    Matrix transposedVs = transpose m
  in
    map (dotProduct v) transposedVs

product :: forall h m w elem. CommonM.Product Matrix h m w elem
product m1 (Matrix vs) = Matrix $ map (\v -> transform m1 v) vs

empty :: forall elem. CommonM.Empty Matrix elem
empty = Matrix $ V.empty

modify :: forall h w i j elem. CommonM.Modify Matrix h w i j elem
modify _ _ f (Matrix m) = Matrix $ V.modify (Common.term :: _ j) (V.modify (Common.term :: _ i) f) m

set :: forall h w i j elem. CommonM.Set Matrix h w i j elem
set i j a m = modify i j (const a) m
