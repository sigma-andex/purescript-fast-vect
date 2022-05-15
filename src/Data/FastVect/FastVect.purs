module Data.FastVect.FastVect
  ( Vect
  , replicate
  , empty
  , singleton
  , append
  , drop
  , take
  , splitAt
  , index
  , head
  , fromArray
  , toArray
  , adjust
  , adjustM
  , cons
  , term
  , toInt
  , (:)
  )
  where

import Prelude

import Data.Array (unsafeIndex)
import Data.Array as A
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Partial.Unsafe (unsafePartial)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT)
import Type.Proxy (Proxy(..))

term :: forall (i :: Int). Proxy i
term = Proxy

toInt :: forall (len :: Int). Reflectable len Int => Proxy len -> Int
toInt = reflectType

newtype Vect ∷ Int → Type → Type
-- | A Vector: A list-like data structure that encodes it's length in the type, backed by an `Array`.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem
  = Vect (Array elem)

instance (Show elem, Reflectable len Int) ⇒ Show (Vect len elem) where
  show (Vect elems) = "Vect " <> show (toInt (term :: _ len)) <> " " <> show elems

instance Eq elem ⇒ Eq (Vect len elem) where
  eq (Vect arr1) (Vect arr2) = eq arr1 arr2

instance Functor (Vect len) where
  map f (Vect xs) = Vect (map f xs)

derive newtype instance FunctorWithIndex Int (Vect len)
derive newtype instance Foldable (Vect len)
derive newtype instance FoldableWithIndex Int (Vect len)
derive newtype instance Traversable (Vect len)
derive newtype instance TraversableWithIndex Int (Vect len)

-- -- | Create a `Vect` by replicating `len` times the given element
-- -- |
-- -- | ```
-- -- | vect :: Vect 300 String
-- -- | vect = replicate (term :: _ 300) "a"
-- -- | ```
replicate ∷
  ∀ len elem.
  Compare len (-1) GT =>
  Reflectable len Int => Proxy len → elem → Vect len elem
replicate proxy elem = Vect $ A.replicate (toInt proxy) elem


-- -- | Creates the empty `Vect`.
-- -- |
-- -- | ```
-- -- | vect :: Vect 0 String
-- -- | vect = empty
-- -- | ```
empty ∷
  ∀ elem.
  Vect 0 elem
empty = Vect []

-- -- | Create a `Vect` of one element.
-- -- |
-- -- | ```
-- -- | vect :: Vect 1 String
-- -- | vect = singleton "a"
-- -- | ```
singleton ∷
  ∀ elem.
  elem → Vect 1 elem
singleton elem = Vect [ elem ]

-- -- | Append two `Vect`s.
-- -- |
-- -- | ```
-- -- | as :: Vect 300 String
-- -- | as = replicate (term :: _ 300) "a"
-- -- |
-- -- | bs :: Vect 200 String
-- -- | bs = replicate (term :: _ 200) "b"
-- -- |
-- -- | cs :: Vect 500 String
-- -- | cs = append as bs
-- -- | ```
append ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Compare m (-1) GT =>
  Compare n (-1) GT =>
  Vect m elem → Vect n elem → Vect m_plus_n elem
append (Vect xs) (Vect ys) = Vect (xs <> ys)

-- -- | Safely drop `m` elements from a `Vect`.
-- -- | Will result in a compile-time error if you are trying to drop more elements than exist in the vector.
-- -- |
-- -- | ```
-- -- | vect :: Vect 300 String
-- -- | vect = replicate (term :: _ 300) "a"
-- -- |
-- -- | newVect :: Vect 200 String
-- -- | newVect = drop (term :: _ 100) vect
-- -- | ```
drop ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Reflectable m Int =>
  Compare m (-1) GT =>
  Compare n (-1) GT =>
  Proxy m → Vect m_plus_n elem → Vect n elem
drop proxy (Vect xs) = Vect (A.drop (toInt proxy) xs)

-- -- | Safely take `m` elements from a `Vect`.
-- -- | Will result in a compile-time error if you are trying to take more elements than exist in the vector.
-- -- |
-- -- | ```
-- -- | vect :: Vect 300 String
-- -- | vect = replicate (term :: _ 300) "a"
-- -- |
-- -- | newVect :: Vect 100 String
-- -- | newVect = take (term :: _ 100) vect
-- -- | ```
take ∷
  ∀ m n m_plus_n elem.
  Add m n m_plus_n ⇒
  Reflectable m Int =>
  Compare m (-1) GT =>
  Compare n (-1) GT =>
  Proxy m → Vect m_plus_n elem → Vect m elem
take proxy (Vect xs) = Vect (A.take (toInt proxy) xs)

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
  Reflectable m Int =>
  Compare m (-1) GT =>
  Compare n (-1) GT =>
  Proxy m → Vect m_plus_n elem → { before ∷ Vect m elem, after ∷ Vect n elem }
splitAt proxy (Vect xs) = { before: Vect before, after: Vect after }
  where
  { before, after } = A.splitAt (toInt proxy) xs

-- -- | Safely access the `i`-th element of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect :: Vect 300 String
-- -- | vect = replicate (term :: _ 300) "a"
-- -- |
-- -- | elem :: String
-- -- | elem = index (term :: _ 299) vect
-- -- | ```
index ∷
  ∀ m m_minus_one i n elem.
  Compare m_minus_one (-1) GT =>
  Add 1 m_minus_one m ⇒
  Compare n (-1) GT =>
  Add i n m_minus_one ⇒
  Compare i (-1) GT =>
  Reflectable i Int =>
  Proxy i → Vect m elem → elem
index proxy (Vect xs) = unsafePartial $ unsafeIndex xs (toInt proxy)

-- -- | Safely access the head of a `Vect`.
-- -- |
-- -- | ```
-- -- | vect :: Vect 300 String
-- -- | vect = replicate (term :: _ 300) "a"
-- -- |
-- -- | elem :: String
-- -- | elem = head vect
-- -- | ```
head ∷
  ∀ m elem.
  Compare m 0 GT =>
  Vect m elem → elem
head (Vect xs) = unsafePartial $ unsafeIndex xs 0

-- -- | Attempt to create a `Vect` of a given size from an `Array`.
-- -- |
-- -- | ```
-- -- | fromArray (term :: _ 3) ["a", "b", "c"] = Just (Vect (term :: _ 3) ["a", "b", "c"])
-- -- |
-- -- | fromArray (term :: _ 4) ["a", "b", "c"] = Nothing
-- -- | ```
fromArray ∷ ∀ len elem.
  Reflectable len Int =>
  Compare len (-1) GT =>
  Proxy len →
  Array elem →
  Maybe (Vect len elem)
fromArray proxy array | Array.length array == toInt proxy = Just (Vect array)
fromArray _ _ = Nothing

-- -- | Converts the `Vect` to an `Array`, effectively dropping the size information.
toArray ∷ ∀ len elem.
  Compare len (-1) GT =>
  Vect len elem → Array elem
toArray (Vect arr) = arr

-- -- | Creates a `Vect` by adjusting the given `Array`, padding with the provided element if the array is to small or dropping elements if the array is to big.
-- -- |
-- -- | ```
-- -- | toArray $ adjust (term ∷ _ 10) 0 [ 1, 2, 3 ] == [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3 ]
-- -- |
-- -- | toArray $ adjust (term ∷ _ 3) 0 [ 0, 0, 0, 0, 1, 2, 3 ] == [ 1, 2, 3 ]
-- -- | ```
adjust ∷ ∀ len elem.
  Reflectable len Int =>
  Compare len (-1) GT =>
  Proxy len →
  elem →
  Array elem →
  Vect len elem
adjust proxy elem array = case Array.length array - toInt proxy of
  0 → Vect array
  len | len < 0 → Vect $ A.replicate (abs len) elem <> array
  len → Vect $ A.drop len array

-- -- | Like `adjust` but uses the Moinoid instance of elem to create the elements.
adjustM ∷ ∀ len elem.
  Monoid elem ⇒
  Reflectable len Int =>
  Compare len (-1) GT =>
  Proxy len →
  Array elem →
  Vect len elem
adjustM proxy = adjust proxy mempty

-- -- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- -- |
-- -- | Note, the running time of this function is `O(n)`.
cons ∷
  ∀ len len_plus_1 elem.
  Add 1 len len_plus_1 ⇒
  Compare len (-1) GT =>
  elem → Vect len elem → Vect len_plus_1 elem
cons elem (Vect arr) = Vect (A.cons elem arr)

infixr 6 cons as :
