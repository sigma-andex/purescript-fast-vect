module Data.FastVect.Sparse.Write
  ( Vect
  , replicate
  , empty
  , singleton
  , append
  , drop
  , take
  , splitAt
  , modify
  , set
  , index
  , indexModulo
  , head
  , fromMap
  , toList
  , cons
  , snoc
  , term
  , toInt
  , (:)
  )
  where

import Prelude

import Data.Filterable (filter, filterMap)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable, sequenceDefault)
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
-- | A Sparse Vector Implementation backed by an `List` of tuples. Slow reads, fast writes.
-- |
-- | ```
-- | vect ∷ Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem
  = Vect (List.List { ix :: Int, elem :: elem })

instance (Show elem, Reflectable len Int) ⇒ Show (Vect len elem) where
  show (Vect elems) = "Vect.Sparse.Read " <> show (toInt (term ∷ _ len)) <> " " <> show elems

data JSSet

foreign import emptyImpl :: JSSet
foreign import insertImpl :: Int -> JSSet -> JSSet
foreign import hasImpl :: Int -> JSSet -> Boolean

fromIE :: forall elem. List.List { ix :: Int, elem :: elem } -> Map.Map Int elem
fromIE = foldl (\b {ix,elem} -> Map.alter (case _ of
  Just x -> Just x
  Nothing -> Just elem) ix b) Map.empty
toIE :: forall elem. Map.Map Int elem -> List.List { ix :: Int, elem :: elem }
toIE = foldlWithIndex (\ix b elem -> List.Cons {ix,elem} b) List.Nil

instance Eq elem ⇒ Eq (Vect len elem) where
  eq (Vect a) (Vect b) = eq (fromIE a) (fromIE b)
instance Ord elem ⇒ Ord (Vect len elem) where
  compare (Vect a) (Vect b) = compare (fromIE a) (fromIE b)
instance Functor (Vect len) where
  map f (Vect xs) = Vect $ map (\{ix, elem} -> {ix, elem: f elem}) xs
instance Apply (Vect len) where
  apply (Vect fab) (Vect a) = Vect $ toIE (fromIE fab <*> fromIE a)
instance (Compare len (-1) GT, Reflectable len Int) ⇒ Applicative (Vect len) where
  pure = replicate (Proxy :: _ len)
instance FunctorWithIndex Int (Vect len) where
  mapWithIndex f (Vect xs) = Vect $ map (\{ix, elem } -> {ix, elem: f ix elem }) xs
instance Foldable (Vect len) where
  foldl bab = go emptyImpl
    where
    go _ b (Vect List.Nil) = b
    go s b (Vect (List.Cons {ix} y)) | hasImpl ix s = go s b (Vect y)
    go s b (Vect (List.Cons {ix, elem} y)) = go (insertImpl ix s) (bab b elem) (Vect y)
  foldr i = foldrDefault i
  foldMap i = foldMapDefaultL i
instance FoldableWithIndex Int (Vect len) where
  foldlWithIndex ibab = go emptyImpl
    where
    go _ b (Vect List.Nil) = b
    go s b (Vect (List.Cons {ix} y)) | hasImpl ix s = go s b (Vect y)
    go s b (Vect (List.Cons {ix, elem} y)) = go (insertImpl ix s) (ibab ix b elem) (Vect y)
  foldrWithIndex i = foldrWithIndexDefault i
  foldMapWithIndex i = foldMapWithIndexDefaultL i
instance Traversable (Vect len) where
  traverse amb = go emptyImpl
    where
    go _ (Vect List.Nil) = pure $ Vect List.Nil
    go s (Vect (List.Cons {ix} y)) | hasImpl ix s = go s (Vect y)
    go s (Vect (List.Cons {ix, elem} y)) = ado
      res <- amb elem
      Vect vc <- go (insertImpl ix s) (Vect y)
      in Vect $ (List.Cons {ix, elem: res} vc)
  sequence = sequenceDefault
instance TraversableWithIndex Int (Vect len) where
  traverseWithIndex iamb = go emptyImpl
    where
    go _ (Vect List.Nil) = pure $ Vect List.Nil
    go s (Vect (List.Cons {ix} y)) | hasImpl ix s = go s (Vect y)
    go s (Vect (List.Cons {ix, elem} y)) = ado
      res <- iamb ix elem
      Vect vc <- go (insertImpl ix s) (Vect y)
      in Vect $ (List.Cons {ix, elem: res} vc)

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
replicate proxy elem = Vect $ (unfoldr (\i@{ix} -> if ix == terminus then Nothing else Just (i /\ {ix: ix + 1, elem} )) {ix: 0, elem})
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
empty = Vect List.Nil

-- -- | Create a `Vect` of one element.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 1 String
-- -- | vect = singleton "a"
-- -- | ```
singleton ∷
  ∀ elem.
  elem → Vect 1 elem
singleton elem = Vect (pure {ix: 0, elem })

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
append (Vect xs) (Vect ys) = Vect (xs <> map (\{ix,elem} -> {ix: ix + (toInt (Proxy :: _ m)), elem}) ys)

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
drop proxy (Vect xs) = Vect (filterMap (\{ix,elem} -> if ix >= drops then Just {ix: (ix - drops), elem} else Nothing) $ xs)
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
take proxy (Vect xs) = Vect (filter (\{ix} -> ix < takes) $ xs)
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
modify proxy f (Vect xs) = Vect $ toIE $ Map.update (f >>> Just) (toInt proxy) (fromIE xs)

-- -- | Safely set element `m` from a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 100 String
-- -- | newVect = modify (term ∷ _ 100) "b" vect
-- -- | `
set ∷
  ∀ m n elem.
  Reflectable m Int ⇒
  Compare m (-1) GT ⇒
  Compare n (-1) GT ⇒
  Compare m n LT ⇒
  Proxy m → elem → Vect n elem → Vect n elem
-- we use cons to represent that this is a newer value
-- this will often cause a duplicate, but we don't care
-- as we weed out duplicates during traversals
set proxy elem (Vect xs) = Vect $ List.Cons {ix: toInt proxy, elem} xs

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
splitAt proxy (Vect xs) =  ( (\{yes,no} -> {before:  Vect $ yes, after: Vect $ no})  $ List.partition (\{ix} -> ix < splits) $ xs)
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
indexModulo i (Vect xs) = List.findMap (\{ix,elem} -> if moded == ix then Just elem else Nothing) xs
  where
  moded = i `mod` toInt (Proxy ∷ _ m)
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
index proxy (Vect xs) = List.findMap (\{ix,elem} -> if ixInt == ix then Just elem else Nothing) xs
  where
  ixInt = toInt proxy

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
head (Vect xs) = List.findMap (\{ix,elem} -> if ix == 0 then Just elem else Nothing) xs

-- -- | Attempt to create a `Vect` of a given size from an `List`.
-- -- |
-- -- | ```
-- -- | fromList (term ∷ _ 3) ["a", "b", "c"] = Just (Vect (term ∷ _ 3) ["a", "b", "c"])
-- -- |
-- -- | fromList (term ∷ _ 4) ["a", "b", "c"] = Nothing
-- -- | ```
fromMap ∷ ∀ len elem.
  Reflectable len Int ⇒
  Compare len (-1) GT ⇒
  Proxy len →
  Map.Map Int elem →
  Maybe (Vect len elem)
fromMap proxy mp | Just { key } <- Map.findMax mp
                 , key < toInt proxy && key >= 0 = Just (Vect $ toIE mp)
fromMap _ _ = Nothing

-- -- | Converts the `Vect` to an `List`, effectively dropping the size information.
toList ∷ ∀ len elem.
  Compare len (-1) GT ⇒
  Vect len elem → List.List {ix :: Int, elem :: elem}
toList (Vect arr) = arr

-- -- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- -- |
cons ∷
  ∀ len len_plus_1 elem.
  Add 1 len len_plus_1 ⇒
  Compare len (-1) GT ⇒
  elem → Vect len elem → Vect len_plus_1 elem
cons elem (Vect arr) = Vect (List.Cons {ix:0,elem} (map (\{ix, elem: elt} -> {ix: ix + 1 , elem: elt}) arr))

snoc ∷
  ∀ len len_plus_1 elem.
  Reflectable len Int ⇒
  Add 1 len len_plus_1 ⇒
  Compare len (-1) GT ⇒
  Vect len elem →  elem → Vect len_plus_1 elem
-- we use cons to represent that this is a newer value
-- this will often cause a duplicate, but we don't care
-- as we weed out duplicates during traversals
snoc (Vect xs) elem = Vect $ List.Cons {ix: toInt (Proxy :: _ len), elem} xs

infixr 6 cons as :
infixr 6 index as !!
infixr 6 indexModulo as !%
