module Data.FastVect.Sparse.Write
  ( Vect
  , replicate
  , empty
  , sparse
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
  , (:)
  ) where

import Prelude

import Data.Filterable (filter, filterMap)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.FastVect.Common as Common
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import Type.Proxy (Proxy(..))

newtype Vect ∷ Int → Type → Type
-- | A Sparse Vector Implementation backed by an `List` of tuples. Slow reads, fast writes.
-- |
-- | ```
-- | vect ∷ Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem = Vect (List.List { ix :: Int, elem :: elem })

instance (Show elem, Reflectable len Int) ⇒ Show (Vect len elem) where
  show (Vect elems) = "Vect.Sparse.Read " <> show (Common.toInt @len) <> " " <> show elems

data JSSet

foreign import emptyImpl :: JSSet
foreign import insertImpl :: Int -> JSSet -> JSSet
foreign import hasImpl :: Int -> JSSet -> Boolean

asMap :: forall elem. List.List { ix :: Int, elem :: elem } -> Map.Map Int elem
asMap = foldl
  ( \b { ix, elem } -> Map.alter
      ( case _ of
          Just x -> Just x
          Nothing -> Just elem
      )
      ix
      b
  )
  Map.empty

asListOfTuples :: forall elem. Map.Map Int elem -> List.List { ix :: Int, elem :: elem }
asListOfTuples = foldlWithIndex (\ix b elem -> List.Cons { ix, elem } b) List.Nil

instance Eq elem ⇒ Eq (Vect len elem) where
  eq (Vect a) (Vect b) = eq (asMap a) (asMap b)

instance Ord elem ⇒ Ord (Vect len elem) where
  compare (Vect a) (Vect b) = compare (asMap a) (asMap b)

instance Functor (Vect len) where
  map f (Vect xs) = Vect $ map (\{ ix, elem } -> { ix, elem: f elem }) xs

instance Apply (Vect len) where
  apply (Vect fab) (Vect a) = Vect $ asListOfTuples (asMap fab <*> asMap a)

instance (Compare len Common.NegOne GT, Reflectable len Int) ⇒ Applicative (Vect len) where
  pure = replicate @len

instance FunctorWithIndex Int (Vect len) where
  mapWithIndex f (Vect xs) = Vect $ map (\{ ix, elem } -> { ix, elem: f ix elem }) xs

instance Foldable (Vect len) where
  foldl bab = go emptyImpl
    where
    go _ b (Vect List.Nil) = b
    go s b (Vect (List.Cons { ix } y)) | hasImpl ix s = go s b (Vect y)
    go s b (Vect (List.Cons { ix, elem } y)) = go (insertImpl ix s) (bab b elem) (Vect y)
  foldr i = foldrDefault i
  foldMap i = foldMapDefaultL i

instance FoldableWithIndex Int (Vect len) where
  foldlWithIndex ibab = go emptyImpl
    where
    go _ b (Vect List.Nil) = b
    go s b (Vect (List.Cons { ix } y)) | hasImpl ix s = go s b (Vect y)
    go s b (Vect (List.Cons { ix, elem } y)) = go (insertImpl ix s) (ibab ix b elem) (Vect y)
  foldrWithIndex i = foldrWithIndexDefault i
  foldMapWithIndex i = foldMapWithIndexDefaultL i

instance Traversable (Vect len) where
  traverse amb = go emptyImpl
    where
    go _ (Vect List.Nil) = pure $ Vect List.Nil
    go s (Vect (List.Cons { ix } y)) | hasImpl ix s = go s (Vect y)
    go s (Vect (List.Cons { ix, elem } y)) = ado
      res <- amb elem
      Vect vc <- go (insertImpl ix s) (Vect y)
      in Vect $ (List.Cons { ix, elem: res } vc)
  sequence = sequenceDefault

instance TraversableWithIndex Int (Vect len) where
  traverseWithIndex iamb = go emptyImpl
    where
    go _ (Vect List.Nil) = pure $ Vect List.Nil
    go s (Vect (List.Cons { ix } y)) | hasImpl ix s = go s (Vect y)
    go s (Vect (List.Cons { ix, elem } y)) = ado
      res <- iamb ix elem
      Vect vc <- go (insertImpl ix s) (Vect y)
      in Vect $ (List.Cons { ix, elem: res } vc)

-- -- | Create a `Vect` by replicating `len` times the given element
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- | ```
replicate ∷ ∀ @len elem. Common.Replicate Vect len elem
replicate elem = Vect $ (unfoldr (\i@{ ix } -> if ix == terminus then Nothing else Just (i /\ { ix: ix + 1, elem })) { ix: 0, elem })
  where
  terminus = Common.toInt @len

-- -- | Creates the empty `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 0 String
-- -- | vect = empty
-- -- | ```
empty ∷ ∀ elem. Common.Empty Vect elem
empty = Vect List.Nil

-- -- | Creates the sparse `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 40 String
-- -- | vect = sparse
-- -- | ```
sparse ∷ ∀ elem n. Common.Sparse Vect elem n
sparse = Vect List.Nil

-- -- | Create a `Vect` of one element.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 1 String
-- -- | vect = singleton "a"
-- -- | ```
singleton ∷ ∀ elem. Common.Singleton Vect elem
singleton elem = Vect (pure { ix: 0, elem })

-- -- | Append two `Vect`s.
-- -- |
-- -- | ```
-- -- | as ∷ Vect 300 String
-- -- | as = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | bs ∷ Vect 200 String
-- -- | bs = replicate (Common.term ∷ _ 200) "b"
-- -- |
-- -- | cs ∷ Vect 500 String
-- -- | cs = append as bs
-- -- | ```
append ∷ ∀ m n m_plus_n elem. Common.Append Vect m n m_plus_n elem
append (Vect xs) (Vect ys) = Vect (xs <> map (\{ ix, elem } -> { ix: ix + (Common.toInt @m), elem }) ys)

-- -- | Safely drop `m` elements from a `Vect`.
-- -- | Will result in a compile-time error if you are trying to drop more elements than exist in the vector.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 200 String
-- -- | newVect = drop (Common.term ∷ _ 100) vect
-- -- | ```
drop ∷ ∀ @m n m_plus_n elem. Common.Drop Vect m n m_plus_n elem
drop (Vect xs) = Vect (filterMap (\{ ix, elem } -> if ix >= drops then Just { ix: (ix - drops), elem } else Nothing) $ xs)
  where
  drops = Common.toInt @m

-- -- | Safely take `m` elements from a `Vect`.
-- -- | Will result in a compile-time error if you are trying to take more elements than exist in the vector.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 100 String
-- -- | newVect = take (Common.term ∷ _ 100) vect
-- -- | ```
take ∷ ∀ @m n m_plus_n elem. Common.Take Vect m n m_plus_n elem
take (Vect xs) = Vect (filter (\{ ix } -> ix < takes) $ xs)
  where
  takes = Common.toInt @m

-- -- | Safely modify element `m` from a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 100 String
-- -- | newVect = modify (Common.term ∷ _ 100) (append "b") vect
-- -- | ```
modify ∷ ∀ @m n elem. Common.Modify Vect m n elem
modify f (Vect xs) = Vect $ asListOfTuples $ Map.update (f >>> Just) (Common.toInt @m) (asMap xs)

-- -- | Safely set element `m` from a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | newVect ∷ Vect 100 String
-- -- | newVect = modify (Common.term ∷ _ 100) "b" vect
-- -- | `

set ∷ ∀ @m n elem. Common.Set Vect m n elem
-- we use cons to represent that this is a newer value
-- this will often cause a duplicate, but we don't care
-- as we weed out duplicates during traversals
set elem (Vect xs) = Vect $ List.Cons { ix: Common.toInt @m, elem } xs

-- -- | Split the `Vect` into two sub vectors `before` and `after`, where before contains up to `m` elements.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 10 String
-- -- | vect = replicate (Common.term ∷ _ 10) "a"
-- -- |
-- -- | split ∷
-- -- |   { after ∷ Vect 7 String
-- -- |   , before ∷ Vect 3 String
-- -- |   }
-- -- | split = splitAt (Common.term ∷ _ 3) vect
-- -- | ```
splitAt ∷ ∀ @m n m_plus_n elem. Common.SplitAt Vect m n m_plus_n elem
splitAt (Vect xs) = ((\{ yes, no } -> { before: Vect $ yes, after: Vect $ no }) $ List.partition (\{ ix } -> ix < splits) $ xs)
  where
  splits = Common.toInt @m

-- -- | Safely access the `n`-th modulo m element of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | elem ∷ String
-- -- | elem = indexModulo 5352523 vect
-- -- | ```
indexModulo ∷ ∀ m elem. Common.IndexModuloM Vect m elem
indexModulo i (Vect xs) = List.findMap (\{ ix, elem } -> if moded == ix then Just elem else Nothing) xs
  where
  moded = i `mod` Common.toInt @m

-- -- | Safely access the `i`-th element of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | elem ∷ String
-- -- | elem = index (Common.term ∷ _ 299) vect
-- -- | ```
index ∷ ∀ m m_minus_one @i n elem. Common.IndexM Vect m m_minus_one i n elem
index (Vect xs) = List.findMap (\{ ix, elem } -> if ixInt == ix then Just elem else Nothing) xs
  where
  ixInt = Common.toInt @i

-- -- | Safely access the head of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect ∷ Vect 300 String
-- -- | vect = replicate (Common.term ∷ _ 300) "a"
-- -- |
-- -- | elem ∷ String
-- -- | elem = head vect
-- -- | ```
head ∷ ∀ m elem. Common.HeadM Vect m elem
head (Vect xs) = List.findMap (\{ ix, elem } -> if ix == 0 then Just elem else Nothing) xs

-- -- | Attempt to create a `Vect` of a given size from an `List`.
-- -- |
-- -- | ```
-- -- | fromList (Common.term ∷ _ 3) ["a", "b", "c"] = Just (Vect (Common.term ∷ _ 3) ["a", "b", "c"])
-- -- |
-- -- | fromList (Common.term ∷ _ 4) ["a", "b", "c"] = Nothing
-- -- | ```
fromMap
  ∷ ∀ @len elem
  . Reflectable len Int
  ⇒ Compare len Common.NegOne GT
  ⇒ Map.Map Int elem
  → Maybe (Vect len elem)
fromMap mp
  | Just { key } <- Map.findMax mp
  , key < Common.toInt @len && key >= 0 = Just (Vect $ asListOfTuples mp)
fromMap _ = Nothing

-- -- | Converts the `Vect` to an `List`, effectively dropping the size information.
toList
  ∷ ∀ len elem
  . Compare len Common.NegOne GT
  ⇒ Vect len elem
  → List.List { ix :: Int, elem :: elem }
toList (Vect arr) = arr

-- -- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- -- |
cons ∷ ∀ len len_plus_1 elem. Common.Cons Vect len len_plus_1 elem
cons elem (Vect arr) = Vect (List.Cons { ix: 0, elem } (map (\{ ix, elem: elt } -> { ix: ix + 1, elem: elt }) arr))

snoc ∷ ∀ @len len_plus_1 elem. Common.Snoc Vect len len_plus_1 elem
-- we use cons to represent that this is a newer value
-- this will often cause a duplicate, but we don't care
-- as we weed out duplicates during traversals
snoc (Vect xs) elem = Vect $ List.Cons { ix: Common.toInt @len, elem } xs

infixr 6 cons as :
infixr 6 index as !!
infixr 6 indexModulo as !%

instance Common.IsVect (Vect n)
