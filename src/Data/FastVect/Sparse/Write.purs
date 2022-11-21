module Data.FastVect.Sparse.Write
  ( (:)
  , Vect
  , append
  , cons
  , drop
  , empty
  , fromMap
  , generate
  , head
  , index
  , indexModulo
  , mapWithTerm
  , modify
  , replicate
  , set
  , singleton
  , snoc
  , sparse
  , splitAt
  , take
  , toList
  ) where

import Prelude

import Data.FastVect.Common as Common
import Data.Filterable (filter, filterMap)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndexDefaultL, foldlWithIndex, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Vect :: Int -> Type -> Type
-- | A Sparse Vector Implementation backed by an `List` of tuples. Slow reads, fast writes.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem = Vect (List.List { ix :: Int, elem :: elem })

instance (Show elem, Reflectable len Int) => Show (Vect len elem) where
  show (Vect elems) = "Vect.Sparse.Read " <> show (Common.toInt (Common.term :: _ len)) <> " " <> show elems

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

instance Eq elem => Eq (Vect len elem) where
  eq (Vect a) (Vect b) = eq (asMap a) (asMap b)

instance Ord elem => Ord (Vect len elem) where
  compare (Vect a) (Vect b) = compare (asMap a) (asMap b)

instance Functor (Vect len) where
  map f (Vect xs) = Vect $ map (\{ ix, elem } -> { ix, elem: f elem }) xs

instance Apply (Vect len) where
  apply (Vect fab) (Vect a) = Vect $ asListOfTuples (asMap fab <*> asMap a)

instance (Compare len Common.NegOne GT, Reflectable len Int) => Applicative (Vect len) where
  pure = replicate (Proxy :: _ len)

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

-- | Create a `Vect` by replicating `len` times the given element
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- | ```
replicate :: forall len elem. Common.Replicate Vect len elem
replicate proxy elem = Vect $ (unfoldr (\i@{ ix } -> if ix == terminus then Nothing else Just (i /\ { ix: ix + 1, elem })) { ix: 0, elem })
  where
  terminus = Common.toInt proxy

-- | Creates the empty `Vect`.
-- |
-- | ```
-- | vect :: Vect 0 String
-- | vect = empty
-- | ```
empty :: forall elem. Common.Empty Vect elem
empty = Vect List.Nil

-- | Creates the sparse `Vect`.
-- |
-- | ```
-- | vect :: Vect 40 String
-- | vect = sparse
-- | ```
sparse :: forall elem n. Common.Sparse Vect elem n
sparse = Vect List.Nil

-- | Create a `Vect` of one element.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
singleton :: forall elem. Common.Singleton Vect elem
singleton elem = Vect (pure { ix: 0, elem })

-- | Append two `Vect`s.
-- |
-- | ```
-- | as :: Vect 300 String
-- | as = replicate (Common.term :: _ 300) "a"
-- |
-- | bs :: Vect 200 String
-- | bs = replicate (Common.term :: _ 200) "b"
-- |
-- | cs :: Vect 500 String
-- | cs = append as bs
-- | ```
append :: forall m n m_plus_n elem. Common.Append Vect m n m_plus_n elem
append (Vect xs) (Vect ys) = Vect (xs <> map (\{ ix, elem } -> { ix: ix + (Common.toInt (Proxy :: _ m)), elem }) ys)

-- | Safely drop `m` elements from a `Vect`.
-- | Will result in a compile-time error if you are trying to drop more elements than exist in the vector.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | newVect :: Vect 200 String
-- | newVect = drop (Common.term :: _ 100) vect
-- | ```
drop :: forall m n m_plus_n elem. Common.Drop Vect m n m_plus_n elem
drop proxy (Vect xs) = Vect (filterMap (\{ ix, elem } -> if ix >= drops then Just { ix: (ix - drops), elem } else Nothing) $ xs)
  where
  drops = Common.toInt proxy

-- | Safely take `m` elements from a `Vect`.
-- | Will result in a compile-time error if you are trying to take more elements than exist in the vector.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = take (Common.term :: _ 100) vect
-- | ```
take :: forall m n m_plus_n elem. Common.Take Vect m n m_plus_n elem
take proxy (Vect xs) = Vect (filter (\{ ix } -> ix < takes) $ xs)
  where
  takes = Common.toInt proxy

-- | Safely modify element `m` from a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = modify (Common.term :: _ 100) (append "b") vect
-- | ```
modify :: forall m n elem. Common.Modify Vect m n elem
modify proxy f (Vect xs) = Vect $ asListOfTuples $ Map.update (f >>> Just) (Common.toInt proxy) (asMap xs)

-- | Safely set element `m` from a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = modify (Common.term :: _ 100) "b" vect
-- | ```
set :: forall m n elem. Common.Set Vect m n elem
-- we use cons to represent that this is a newer value
-- this will often cause a duplicate, but we don't care
-- as we weed out duplicates during traversals
set proxy elem (Vect xs) = Vect $ List.Cons { ix: Common.toInt proxy, elem } xs

-- | Split the `Vect` into two sub vectors `before` and `after`, where before contains up to `m` elements.
-- |
-- | ```
-- | vect :: Vect 10 String
-- | vect = replicate (Common.term :: _ 10) "a"
-- |
-- | split ::
-- |   { after :: Vect 7 String
-- |   , before :: Vect 3 String
-- |   }
-- | split = splitAt (Common.term :: _ 3) vect
-- | ```
splitAt :: forall m n m_plus_n elem. Common.SplitAt Vect m n m_plus_n elem
splitAt proxy (Vect xs) = ((\{ yes, no } -> { before: Vect $ yes, after: Vect $ no }) $ List.partition (\{ ix } -> ix < splits) $ xs)
  where
  splits = Common.toInt proxy

-- | Safely access the `n`-th modulo m element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = indexModulo 5352523 vect
-- | ```
indexModulo :: forall m elem. Common.IndexModuloM Vect m elem
indexModulo i (Vect xs) = List.findMap (\{ ix, elem } -> if moded == ix then Just elem else Nothing) xs
  where
  moded = i `mod` Common.toInt (Proxy :: _ m)

-- | Safely access the `i`-th element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = index (Common.term :: _ 299) vect
-- | ```
index :: forall m n elem. Common.IndexM Vect m n elem
index proxy (Vect xs) = List.findMap (\{ ix, elem } -> if ixInt == ix then Just elem else Nothing) xs
  where
  ixInt = Common.toInt proxy

-- | Safely access the head of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = head vect
-- | ```
head :: forall m elem. Common.HeadM Vect m elem
head (Vect xs) = List.findMap (\{ ix, elem } -> if ix == 0 then Just elem else Nothing) xs

-- | Attempt to create a `Vect` of a given size from an `List`.
-- |
-- | ```
-- | fromList (Common.term :: _ 3) ["a", "b", "c"] = Just (Vect (Common.term :: _ 3) ["a", "b", "c"])
-- |
-- | fromList (Common.term :: _ 4) ["a", "b", "c"] = Nothing
-- | ```
fromMap
  :: forall len elem
   . Reflectable len Int
  => Compare len Common.NegOne GT
  => Proxy len
  -> Map.Map Int elem
  -> Maybe (Vect len elem)
fromMap proxy mp
  | Just { key } <- Map.findMax mp
  , key < Common.toInt proxy && key >= 0 = Just (Vect $ asListOfTuples mp)
fromMap _ _ = Nothing

-- | Converts the `Vect` to an `List`, effectively dropping the size information.
toList
  :: forall len elem
   . Compare len Common.NegOne GT
  => Vect len elem
  -> List.List { ix :: Int, elem :: elem }
toList (Vect arr) = arr

-- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- |
cons :: forall len len_plus_1 elem. Common.Cons Vect len len_plus_1 elem
cons elem (Vect arr) = Vect (List.Cons { ix: 0, elem } (map (\{ ix, elem: elt } -> { ix: ix + 1, elem: elt }) arr))

snoc :: forall len len_plus_1 elem. Common.Snoc Vect len len_plus_1 elem
-- we use cons to represent that this is a newer value
-- this will often cause a duplicate, but we don't care
-- as we weed out duplicates during traversals
snoc (Vect xs) elem = Vect $ List.Cons { ix: Common.toInt (Proxy :: _ len), elem } xs

infixr 6 cons as :
infixr 6 index as !!
infixr 6 indexModulo as !%

unsafeCoerceTerm
  :: forall len a
   . Proxy len
  -> ( forall i
        . Compare i Common.NegOne GT
       => Compare i len LT
       => Reflectable i Int
       => Proxy i
       -> a
     )
  -> Int
  -> a
unsafeCoerceTerm _ f i = internal f unit unit { reflectType: \_ -> i } Proxy
  where
  internal
    :: ( forall i
          . Compare i Common.NegOne GT
         => Compare i len LT
         => Reflectable i Int
         => Proxy i
         -> a
       )
    -> Unit
    -> Unit
    -> { reflectType :: Proxy _ -> Int }
    -> Proxy _
    -> a
  internal = unsafeCoerce

-- | Generate a `Vect` of the given size by applying a function to each type level index.
generate :: forall len elem. Common.Generate Vect len elem
generate _ f = Vect
  $ map (\i -> { ix: i, elem: unsafeCoerceTerm (Proxy :: _ len) f i })
  $ List.range 0 (Common.toInt (Proxy :: _ len) - 1)

-- | Map a function over a `Vect` with the type level index of each element.
mapWithTerm :: forall len elem elem'. Common.MapWithTerm Vect len elem elem'
mapWithTerm f xs = mapWithIndex (\i elem -> unsafeCoerceTerm (Proxy :: _ len) f i elem) xs

instance Common.IsVect (Vect n)
