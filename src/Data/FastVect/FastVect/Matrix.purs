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
  , replicate
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
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.FastVect.Common as Common
import Data.FastVect.Common.Matrix as CommonM
import Data.FastVect.FastVect (Vect)
import Data.FastVect.FastVect as V
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe)
import Data.Reflectable (class Reflectable)
import Data.Semigroup.Foldable (class Foldable1, foldMap1, foldl1, foldr1)
import Data.Semigroup.Traversable (class Traversable1, sequence1, traverse1)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, sum, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Prim.Int (class Compare)
import Prim.Ordering (GT)

-- | A matrix of elements of type `a`, implemented as a array vector of array vectors.
-- | The first type argument represents the height of the matrix and the second type argument represents the width.
-- | Note that the internal implementation is a horizontal row of vertical vectors.
-- | This is because in mathematics, matrices are often treated as a row of vertical vectors.
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

instance (Compare h Common.NegOne GT, Reflectable h Int, Compare w Common.NegOne GT, Reflectable w Int) => Applicative (Matrix h w) where
  pure = replicate Common.term Common.term

instance (Compare h Common.Zero GT, Compare w Common.Zero GT, Reflectable h Int, Reflectable w Int) => Bind (Matrix h w) where
  bind matrix f = distribute f <*> matrix

instance (Compare h Common.Zero GT, Compare w Common.Zero GT, Reflectable h Int, Reflectable w Int) => Monad (Matrix h w)

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

instance (Compare h Common.Zero GT, Compare w Common.Zero GT) => Traversable1 (Matrix h w) where
  traverse1 f (Matrix v) = Matrix <$> traverse1 (traverse1 f) v
  sequence1 (Matrix v) = Matrix <$> traverse1 sequence1 v

instance (Compare h Common.Zero GT, Compare w Common.Zero GT, Reflectable h Int, Reflectable w Int) => Distributive (Matrix h w) where
  distribute = map (\(Matrix v) -> v) >>> distribute >>> map distribute >>> Matrix
  collect = collectDefault

instance Semigroup a => Semigroup (Matrix h w a) where
  append = lift2 append

instance (Compare h Common.NegOne GT, Reflectable h Int, Compare w Common.NegOne GT, Reflectable w Int, Monoid a) => Monoid (Matrix h w a) where
  mempty = pure mempty

instance (Compare h Common.NegOne GT, Reflectable h Int, Compare w Common.NegOne GT, Reflectable w Int, Semiring a) => Semiring (Matrix h w a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance (Compare h Common.NegOne GT, Reflectable h Int, Compare w Common.NegOne GT, Reflectable w Int, Ring a) => Ring (Matrix h w a) where
  sub = lift2 sub

instance (Compare h Common.NegOne GT, Reflectable h Int, Compare w Common.NegOne GT, Reflectable w Int, CommutativeRing a) => CommutativeRing (Matrix h w a)

-- | Safely accesses the `j` -th element of the `i` -th row of `Matrix`.
index :: forall h w i j elem. CommonM.Index Matrix h w i j elem
index _ _ (Matrix m) = V.index (Common.term :: _ i) $ V.index (Common.term :: _ j) m

-- | Create a `Matrix` by replicating the given element.
replicate :: forall h w a. CommonM.Replicate Matrix h w a
replicate _ _ a = Matrix $ V.replicate Common.term $ V.replicate Common.term a

-- | Generate a `Matrix` by applying the given function to each type-level index.
generate :: forall h w elem. CommonM.Generate Matrix h w elem
generate _ _ f = Matrix $ V.generate Common.term \j -> V.generate Common.term \i -> f i j

-- | Map a function over the elements of a `Matrix` with its type-level indices.
mapWithTerm :: forall h w elem elem'. CommonM.MapWithTerm Matrix h w elem elem'
mapWithTerm f (Matrix m) = Matrix $ V.mapWithTerm (\j -> V.mapWithTerm (\i -> f i j)) m

-- | Convert a `Matrix` to a vector of vectors.
toVectVect
  :: forall h w elem
   . Matrix h w elem
  -> Vect w (Vect h elem)
toVectVect (Matrix v) = v

-- | Convert a `Matrix` to a array of vectors.
toVectArray
  :: forall h w elem
   . Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Matrix h w elem
  -> Array (Vect h elem)
toVectArray (Matrix m) = V.toArray m

-- | Convert a nx1 `Matrix` to a vector.
toVect
  :: forall h elem
   . Compare h Common.NegOne GT
  => Matrix h 1 elem
  -> Vect h elem
toVect (Matrix m) = V.index (Common.term :: _ 0) m

-- | Convert a `Matrix` to a array of arrays.
toArrayArray
  :: forall h w elem
   . Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Matrix h w elem
  -> Array (Array elem)
toArrayArray (Matrix m) = V.toArray $ map V.toArray m

-- | Convert a vector of vectors to a `Matrix`.
fromVectVect
  :: forall h w elem
   . Vect w (Vect h elem)
  -> Matrix h w elem
fromVectVect = Matrix

-- | Convert a vector of arrays to a `Matrix`.
fromVectArray
  :: forall h w elem
   . Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Reflectable w Int
  => Array (Vect h elem)
  -> Maybe (Matrix h w elem)
fromVectArray arr = Matrix <$> V.fromArray Common.term arr

-- | Convert a vector to a nx1 `Matrix`.
fromVect :: forall h elem. Compare h Common.NegOne GT => Vect h elem -> Matrix h 1 elem
fromVect v = Matrix $ V.singleton v

-- | Convert an array of arrays to a `Matrix`.
fromArrayArray
  :: forall h w elem
   . Compare h Common.NegOne GT
  => Compare w Common.NegOne GT
  => Reflectable w Int
  => Compare h Common.NegOne GT
  => Reflectable h Int
  => Array (Array elem)
  -> Maybe (Matrix h w elem)
fromArrayArray arr = fromVectArray =<< (sequence $ map (V.fromArray Common.term) arr)

-- | Create `Matrix` of one element.
singleton :: forall elem. elem -> Matrix 1 1 elem
singleton a = Matrix $ V.singleton $ V.singleton a

-- | Transpose a `Matrix`.
transpose :: forall h w elem. CommonM.Transpose Matrix h w elem
transpose m = generate Common.term Common.term \i j -> index j i m

-- | Dot product of two vectors.
dotProduct :: forall h elem. CommonM.DotProduct Vect h elem
dotProduct v1 v2 = sum $ lift2 (*) v1 v2

-- | Outer product with multiplication function
outerMap :: forall h w elemH elemW elem. CommonM.OuterMap Vect Matrix h w elemH elemW elem
outerMap f v1 v2 = Matrix $ map (\elemW -> map (\elemH -> f elemH elemW) v1) v2

-- | Outer product
outerProduct :: forall h w elem. CommonM.OuterProduct Vect Matrix h w elem
outerProduct = outerMap (*)

-- | Create Matrix from its diagonal elements.
diag :: forall h elem. CommonM.Diag Vect Matrix h elem
diag v = generate Common.term Common.term
  \i j -> if Common.toInt i == Common.toInt j then V.index i v else zero

-- | Get the diagonal elements of a `Matrix`.
traced :: forall h elem. CommonM.Traced Vect Matrix h elem
traced m = V.generate Common.term \i -> index i i m

-- | Get sum of diagonal elements of a `Matrix`.
trace :: forall h elem. CommonM.Trace Matrix h elem
trace m = sum $ traced m

-- | Transform a `Vector` by a `Matrix`.
transform :: forall h w elem. CommonM.Transform Vect Matrix h w elem
transform m v = V.generate Common.term \i -> sum $ V.generate (Common.term :: _ w) \j -> index i j m * V.index j v

-- | Matrix multiplication.
product :: forall h m w elem. CommonM.Product Matrix h m w elem
product m1 (Matrix vs) = Matrix $ map (\v -> transform m1 v) vs

-- | Create empty `Matrix`.
empty :: forall elem. CommonM.Empty Matrix elem
empty = Matrix $ V.empty

-- | Modify the `j` -th element of the `i` -th row of a `Matrix`.
modify :: forall h w i j elem. CommonM.Modify Matrix h w i j elem
modify _ _ f (Matrix m) = Matrix $ V.modify (Common.term :: _ j) (V.modify (Common.term :: _ i) f) m

-- | Set the `j` -th element of the `i` -th row of a `Matrix`.
set :: forall h w i j elem. CommonM.Set Matrix h w i j elem
set i j a m = modify i j (const a) m
