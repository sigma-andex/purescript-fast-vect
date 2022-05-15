module Data.FastVect.Sparse.Read
  ( Vect
  , replicate
  , empty
  , singleton
  , append
  , drop
  , take
  , splitAt
  , modify
  , index
  , indexModulo
  , head
  , fromMap
  , toMap
  , cons
  , snoc
  , term
  , toInt
  , (:)
  )
  where

import Prelude

import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))

term ∷ forall (i ∷ Int). Proxy i
term = Proxy

toInt ∷ forall (len ∷ Int). Reflectable len Int ⇒ Proxy len → Int
toInt = reflectType

newtype Vect ∷ Int → Type → Type
-- | A Sparse Vector Implementation backed by a `Map`. Medium-fast reads, medium-fast writes.
-- |
-- | ```
-- | vect ∷ Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem
  = Vect (Map Int elem)

instance (Show elem, Reflectable len Int) ⇒ Show (Vect len elem) where
  show (Vect elems) = "Vect.Sparse.Read " <> show (toInt (term ∷ _ len)) <> " " <> show elems

derive newtype instance Eq elem ⇒ Eq (Vect len elem)
derive newtype instance Ord elem ⇒ Ord (Vect len elem)
derive newtype instance Functor (Vect len)
derive newtype instance Apply (Vect len)
instance (Compare len (-1) GT, Reflectable len Int) ⇒ Applicative (Vect len) where
  pure = replicate (Proxy :: _ len)
derive newtype instance FunctorWithIndex Int (Vect len)
derive newtype instance Foldable (Vect len)
derive newtype instance FoldableWithIndex Int (Vect len)
derive newtype instance Traversable (Vect len)
derive newtype instance TraversableWithIndex Int (Vect len)

-- -- | Create a `Vect` by replicating `len` times the given element
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- | ```
replicate ∷
  ∀ len elem.
  Compare len (-1) GT ⇒
  Reflectable len Int ⇒ Proxy len → elem → Vect len elem
replicate proxy elem = Vect $ (Map.fromFoldable :: Array _ -> _) $ (unfoldr (\i@(ix /\ b) -> if ix == terminus then Nothing else Just (i /\ ((ix + 1) /\ b))) (0 /\ elem))
  where
  terminus = toInt proxy


-- -- | Creates the empty `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 0 String
-- -- | vect = empty
-- -- | ```
empty ∷
  ∀ elem.
  Vect 0 elem
empty = Vect (Map.empty)

-- -- | Create a `Vect` of one element.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 1 String
-- -- | vect = singleton "a"
-- -- | ```
singleton ∷
  ∀ elem.
  elem → Vect 1 elem
singleton elem = Vect (Map.singleton 0 elem)

-- -- | Append two `Vect`s.
-- -- |
-- -- | ```
-- -- | as ∷ Vect 300 String
-- -- | as = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | bs ∷ Vect 200 String
-- -- | bs = replicate (term ∷ _ 200) "b"
-- -- |
-- -- | cs ∷ Vect 500 String
-- -- | cs = append as bs
-- -- | ```
append ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Reflectable m Int ⇒
  Compare m (-1) GT ⇒
  Compare n (-1) GT ⇒
  Vect m elem → Vect n elem → Vect m_plus_n elem
append (Vect xs) (Vect ys) = Vect (Map.union xs (Map.fromFoldable $ map (\(ix /\ a) -> ((ix + (toInt (Proxy :: _ m))) /\ a)) $ (Map.toUnfoldableUnordered :: _ -> Array _) ys))

-- -- | Safely drop `m` elements from a `Vect`.
-- -- | Will result in a compile-time error if you are trying to drop more elements than exist in the vector.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 200 String
-- -- | newVect = drop (term ∷ _ 100) vect
-- -- | ```
drop ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Reflectable m Int ⇒
  Compare m (-1) GT ⇒
  Compare n (-1) GT ⇒
  Proxy m → Vect m_plus_n elem → Vect n elem
drop proxy (Vect xs) = Vect ((Map.fromFoldable :: Array _ -> _) $ filterMap (\(ix /\ a) -> if ix >= drops then Just ((ix - drops) /\ a) else Nothing) $ Map.toUnfoldableUnordered xs)
  where
  drops = toInt proxy

-- -- | Safely take `m` elements from a `Vect`.
-- -- | Will result in a compile-time error if you are trying to take more elements than exist in the vector.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 100 String
-- -- | newVect = take (term ∷ _ 100) vect
-- -- | ```
take ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Reflectable m Int ⇒
  Compare m (-1) GT ⇒
  Compare n (-1) GT ⇒
  Proxy m → Vect m_plus_n elem → Vect m elem
take proxy (Vect xs) = Vect (Map.fromFoldable $ Array.filter (\(ix /\ _) -> ix < takes) $ Map.toUnfoldableUnordered xs)
  where
  takes = toInt proxy

-- -- | Safely modify element `m` from a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 100 String
-- -- | newVect = modify (term ∷ _ 100) (append "b") vect
-- -- | ```
modify ∷
  ∀ m n elem.
  Reflectable m Int ⇒
  Compare m (-1) GT ⇒
  Compare n (-1) GT ⇒
  Compare m n LT ⇒
  Proxy m → (elem → elem) → Vect n elem → Vect n elem
modify proxy f (Vect xs) = Vect $ Map.update (f >>> Just) (toInt proxy) xs

-- -- | Split the `Vect` into two sub vectors `before` and `after`, where before contains up to `m` elements.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 10 String
-- -- | vect = replicate (term ∷ _ 10) "a"
-- -- |
-- -- | split ∷
-- -- |   { after ∷ Vect 7 String
-- -- |   , before ∷ Vect 3 String
-- -- |   }
-- -- | split = splitAt (term ∷ _ 3) vect
-- -- | ```
splitAt ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Reflectable m Int ⇒
  Compare m (-1) GT ⇒
  Compare n (-1) GT ⇒
  Proxy m → Vect m_plus_n elem → { before ∷ Vect m elem, after ∷ Vect n elem }
splitAt proxy (Vect xs) =  ( (\{yes,no} -> {before:  Vect $ Map.fromFoldable yes, after: Vect $ Map.fromFoldable no})  $ Array.partition (\(ix /\ _) -> ix < splits) $ Map.toUnfoldableUnordered xs)
  where
  splits = toInt proxy

-- -- | Safely access the `n`-th modulo m element of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | elem ∷ String
-- -- | elem = indexModulo 5352523 vect
-- -- | ```
indexModulo ∷
  ∀ m elem.
  Compare m 0 GT ⇒
  Reflectable m Int ⇒
  Int → Vect m elem → Maybe elem
indexModulo i (Vect xs) = Map.lookup i xs

-- -- | Safely access the `i`-th element of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | elem ∷ String
-- -- | elem = index (term ∷ _ 299) vect
-- -- | ```
index ∷
  ∀ m m_minus_one i n elem.
  Compare m_minus_one (-1) GT ⇒
  Add 1 m_minus_one m ⇒
  Compare n (-1) GT ⇒
  Add i n m_minus_one ⇒
  Compare i (-1) GT ⇒
  Reflectable i Int ⇒
  Proxy i → Vect m elem → Maybe elem
index proxy (Vect xs) = Map.lookup (toInt proxy) xs

-- -- | Safely access the head of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | elem ∷ String
-- -- | elem = head vect
-- -- | ```
head ∷
  ∀ m elem.
  Compare m 0 GT ⇒
  Vect m elem → Maybe elem
head (Vect xs) = Map.lookup 0 xs

-- -- | Attempt to create a `Vect` of a given size from an `Array`.
-- -- |
-- -- | ```
-- -- | fromArray (term ∷ _ 3) ["a", "b", "c"] = Just (Vect (term ∷ _ 3) ["a", "b", "c"])
-- -- |
-- -- | fromArray (term ∷ _ 4) ["a", "b", "c"] = Nothing
-- -- | ```
fromMap ∷ ∀ len elem.
  Reflectable len Int ⇒
  Compare len (-1) GT ⇒
  Proxy len →
  Map.Map Int elem →
  Maybe (Vect len elem)
fromMap proxy mp | Just { key } <- Map.findMax mp
                 , key < toInt proxy && key >= 0 = Just (Vect mp)
fromMap _ _ = Nothing

-- -- | Converts the `Vect` to an `Array`, effectively dropping the size information.
toMap ∷ ∀ len elem.
  Compare len (-1) GT ⇒
  Vect len elem → Map.Map Int elem
toMap (Vect arr) = arr

-- -- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- -- |
cons ∷
  ∀ len len_plus_1 elem.
  Add 1 len len_plus_1 ⇒
  Compare len (-1) GT ⇒
  elem → Vect len elem → Vect len_plus_1 elem
cons elem (Vect arr) = Vect (Map.insert 0 elem (Map.fromFoldable $ map (\(ix /\ a) -> ((ix + 1 ) /\ a)) $ (Map.toUnfoldableUnordered :: _ -> Array _) arr))

snoc ∷
  ∀ len len_plus_1 elem.
  Reflectable len Int ⇒
  Add 1 len len_plus_1 ⇒
  Compare len (-1) GT ⇒
  Vect len elem →  elem → Vect len_plus_1 elem
snoc (Vect xs) elem = Vect $ Map.insert (toInt (Proxy :: _ len)) elem xs

infixr 6 cons as :
infixr 6 index as !!
infixr 6 indexModulo as !%
