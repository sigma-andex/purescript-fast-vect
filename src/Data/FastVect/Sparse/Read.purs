module Data.FastVect.Sparse.Read
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
  , toMap
  ) where

import Prelude

import Data.Array as Array
import Data.FastVect.Common as Common
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Reflectable (class Reflectable)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Vect :: Int -> Type -> Type
-- | A Sparse Vector Implementation backed by a `Map`. Medium-fast reads, medium-fast writes.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem = Vect (Map Int elem)

instance (Show elem, Reflectable len Int) => Show (Vect len elem) where
  show (Vect elems) = "Vect.Sparse.Read " <> show (Common.toInt (Common.term :: _ len)) <> " " <> show elems

derive newtype instance Eq elem => Eq (Vect len elem)
derive newtype instance Ord elem => Ord (Vect len elem)
derive newtype instance Functor (Vect len)
derive newtype instance Apply (Vect len)
instance (Compare len Common.NegOne GT, Reflectable len Int) => Applicative (Vect len) where
  pure = replicate (Proxy :: _ len)

derive newtype instance FunctorWithIndex Int (Vect len)
derive newtype instance Foldable (Vect len)
derive newtype instance FoldableWithIndex Int (Vect len)
derive newtype instance Traversable (Vect len)
derive newtype instance TraversableWithIndex Int (Vect len)

-- | Create a `Vect` by replicating `len` times the given element
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- | ```
replicate :: forall len elem. Common.Replicate Vect len elem
replicate proxy elem = Vect $ (Map.fromFoldable :: Array _ -> _) $ (unfoldr (\i@(ix /\ b) -> if ix == terminus then Nothing else Just (i /\ ((ix + 1) /\ b))) (0 /\ elem))
  where
  terminus = Common.toInt proxy

-- | Creates the empty `Vect`.
-- |
-- | ```
-- | vect :: Vect 0 String
-- | vect = empty
-- | ```
empty :: forall elem. Common.Empty Vect elem
empty = Vect (Map.empty)

-- | Creates the sparse `Vect`.
-- |
-- | ```
-- | vect :: Vect 40 String
-- | vect = sparse
-- | ```
sparse :: forall elem n. Common.Sparse Vect elem n
sparse = Vect (Map.empty)

-- | Create a `Vect` of one element.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
singleton :: forall elem. Common.Singleton Vect elem
singleton elem = Vect (Map.singleton 0 elem)

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
append (Vect xs) (Vect ys) = Vect (Map.union xs (Map.fromFoldable $ map (first (add (Common.toInt (Proxy :: _ m)))) $ (Map.toUnfoldableUnordered :: _ -> Array _) ys))

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
drop proxy (Vect xs) = Vect ((Map.fromFoldable :: Array _ -> _) $ filterMap (\(ix /\ a) -> if ix >= drops then Just ((ix - drops) /\ a) else Nothing) $ Map.toUnfoldableUnordered xs)
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
take proxy (Vect xs) = Vect (Map.fromFoldable $ Array.filter (fst >>> (_ < takes)) $ Map.toUnfoldableUnordered xs)
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
modify proxy f (Vect xs) = Vect $ Map.update (f >>> Just) (Common.toInt proxy) xs

-- | Safely set element `m` from a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = modify (Common.term :: _ 100) "b" vect
-- | `
set :: forall m n elem. Common.Set Vect m n elem
set proxy a (Vect xs) = Vect $ Map.alter (const (Just a)) (Common.toInt proxy) xs

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
splitAt proxy (Vect xs) = ((\{ yes, no } -> { before: Vect $ Map.fromFoldable yes, after: Vect $ Map.fromFoldable no }) $ Array.partition (fst >>> (_ < splits)) $ Map.toUnfoldableUnordered xs)
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
indexModulo i (Vect xs) = Map.lookup (i `mod` Common.toInt (Proxy :: _ m)) xs

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
index proxy (Vect xs) = Map.lookup (Common.toInt proxy) xs

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
head (Vect xs) = Map.lookup 0 xs

-- | Attempt to create a `Vect` of a given size from an `Array`.
-- |
-- | ```
-- | fromArray (Common.term :: _ 3) ["a", "b", "c"] = Just (Vect (Common.term :: _ 3) ["a", "b", "c"])
-- |
-- | fromArray (Common.term :: _ 4) ["a", "b", "c"] = Nothing
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
  , key < Common.toInt proxy && key >= 0 = Just (Vect mp)
fromMap _ _ = Nothing

-- | Converts the `Vect` to an `Array`, effectively dropping the size information.
toMap
  :: forall len elem
   . Compare len Common.NegOne GT
  => Vect len elem
  -> Map.Map Int elem
toMap (Vect arr) = arr

-- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- |
cons :: forall len len_plus_1 elem. Common.Cons Vect len len_plus_1 elem
cons elem (Vect arr) = Vect (Map.insert 0 elem (Map.fromFoldable $ map (first (add 1)) $ (Map.toUnfoldableUnordered :: _ -> Array _) arr))

snoc
  :: forall len len_plus_1 elem. Common.Snoc Vect len len_plus_1 elem
snoc (Vect xs) elem = Vect $ Map.insert (Common.toInt (Proxy :: _ len)) elem xs

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
  $ Map.fromFoldable
  $ map (\i -> Tuple i (unsafeCoerceTerm (Proxy :: _ len) f i))
  $ Array.range 0 (Common.toInt (Proxy :: _ len) - 1)

-- | Map a function over a `Vect` with the type level index of each element.
mapWithTerm :: forall len elem elem'. Common.MapWithTerm Vect len elem elem'
mapWithTerm f xs = mapWithIndex (\i elem -> unsafeCoerceTerm (Proxy :: _ len) f i elem) xs

instance Common.IsVect (Vect n)
