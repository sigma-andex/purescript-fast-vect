module Data.FastVect.Common
  ( Append
  , Cons
  , Drop
  , Empty
  , Generate
  , Head
  , HeadM
  , Last
  , Index
  , IndexM
  , IndexModulo
  , IndexModuloM
  , MapWithTerm
  , Modify
  , NegOne
  , One
  , Replicate
  , Set
  , Singleton
  , Snoc
  , Sparse
  , SplitAt
  , Take
  , Zero
  , class IsVect
  , term
  , toInt
  ) where

import Data.Maybe (Maybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Prim.Int (class Add, class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))

type Zero = 0
type One = 1
type NegOne = -1

class TraversableWithIndex Int f <= IsVect f

-- | Create a term for the index of a vector.
term :: forall (i :: Int). Proxy i
term = Proxy

-- | Convert a term to an Int
toInt :: forall (len :: Int). Reflectable len Int => Proxy len -> Int
toInt = reflectType

-- | Create a `Vect` by replicating `len` times the given element
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- | ```
type Replicate vect len elem =
  Compare len NegOne GT
  => Reflectable len Int
  => Proxy len
  -> elem
  -> vect len elem

-- | Creates the empty `Vect`.
-- |
-- | ```
-- | vect :: Vect 0 String
-- | vect = empty
-- | ```
type Empty :: forall k1 k2. (Int -> k1 -> k2) -> k1 -> k2
type Empty vect elem = vect Zero elem

-- | Creates the sparse `Vect`.
-- |
-- | ```
-- | vect :: Vect 40 String
-- | vect = sparse
-- | ```
type Sparse :: forall k1 k2 k3. (k1 -> k2 -> k3) -> k1 -> k2 -> k3
type Sparse vect n elem = vect n elem

-- | Create a `Vect` of one element.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
type Singleton vect elem = elem -> vect One elem

-- | Append two `Vect`s.
-- |
-- | ```
-- | as :: Vect 300 String
-- | as = replicate (term :: _ 300) "a"
-- |
-- | bs :: Vect 200 String
-- | bs = replicate (term :: _ 200) "b"
-- |
-- | cs :: Vect 500 String
-- | cs = append as bs
-- | ```
type Append :: forall k. (Int -> k -> Type) -> Int -> Int -> Int -> k -> Type
type Append vect m n m_plus_n elem =
  Add m n m_plus_n
  => Compare m NegOne GT
  => Reflectable m Int
  => Compare n NegOne GT
  => vect m elem
  -> vect n elem
  -> vect m_plus_n elem

-- | Safely drop `m` elements from a `Vect`.
-- | Will result in a compile-time error if you are trying to drop more elements than exist in the vector.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | newVect :: Vect 200 String
-- | newVect = drop (term :: _ 100) vect
-- | ```
type Drop :: forall k. (Int -> k -> Type) -> Int -> Int -> Int -> k -> Type
type Drop vect m n m_plus_n elem =
  Add m n m_plus_n
  => Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Proxy m
  -> vect m_plus_n elem
  -> vect n elem

-- | Safely take `m` elements from a `Vect`.
-- | Will result in a compile-time error if you are trying to take more elements than exist in the vector.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = take (term :: _ 100) vect
-- | ```
type Take :: forall k. (Int -> k -> Type) -> Int -> Int -> Int -> k -> Type
type Take vect m n m_plus_n elem =
  Add m n m_plus_n
  => Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Proxy m
  -> vect m_plus_n elem
  -> vect m elem

-- | Safely modify element `m` from a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = modify (term :: _ 100) (append "b") vect
-- | ```
type Modify vect m n elem =
  Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Compare m n LT
  => Proxy m
  -> (elem -> elem)
  -> vect n elem
  -> vect n elem

-- | Safely set element `m` from a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | newVect :: Vect 100 String
-- | newVect = modify (term :: _ 100) "b" vect
-- | `
type Set vect m n elem =
  Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Compare m n LT
  => Proxy m
  -> elem
  -> vect n elem
  -> vect n elem

-- | Split the `Vect` into two sub vectors `before` and `after`, where before contains up to `m` elements.
-- |
-- | ```
-- | vect :: Vect 10 String
-- | vect = replicate (term :: _ 10) "a"
-- |
-- | split ::
-- |   { after :: Vect 7 String
-- |   , before :: Vect 3 String
-- |   }
-- | split = splitAt (term :: _ 3) vect
-- | ```
type SplitAt :: forall k. (Int -> k -> Type) -> Int -> Int -> Int -> k -> Type
type SplitAt vect m n m_plus_n elem =
  Add m n m_plus_n
  => Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Proxy m
  -> vect m_plus_n elem
  -> { before :: vect m elem, after :: vect n elem }

-- | Safely access the `n`-th modulo m element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = indexModulo 5352523 vect
-- | ```
type IndexModulo vect m elem =
  Compare m Zero GT
  => Reflectable m Int
  => Int
  -> vect m elem
  -> elem

type IndexModuloM vect m elem =
  Compare m Zero GT
  => Reflectable m Int
  => Int
  -> vect m elem
  -> Maybe elem

-- | Safely access the `i`-th element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = index (term :: _ 299) vect
-- | ```
type Index vect m n elem =
  Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Compare m n LT
  => Proxy m
  -> vect n elem
  -> elem

type IndexM vect m n elem =
  Reflectable m Int
  => Compare m NegOne GT
  => Compare n NegOne GT
  => Compare m n LT
  => Proxy m
  -> vect n elem
  -> Maybe elem

-- | Safely access the head of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = head vect
-- | ```
type Head vect m elem =
  Compare m Zero GT
  => vect m elem
  -> elem

type HeadM vect m elem =
  Compare m Zero GT
  => vect m elem
  -> Maybe elem

-- | Safely access the last element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = last vect
-- | ```
type Last vect m elem =
  Compare m Zero GT
  => Reflectable m Int
  => vect m elem
  -> elem

-- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
type Cons vect len len_plus_1 elem =
  Add One len len_plus_1
  => Compare len NegOne GT
  => elem
  -> vect len elem
  -> vect len_plus_1 elem

-- | Attaches an element to the end of the `Vect`, creating a new `Vect` with size incremented.
type Snoc vect len len_plus_1 elem =
  Add One len len_plus_1
  => Reflectable len Int
  => Compare len NegOne GT
  => vect len elem
  -> elem
  -> vect len_plus_1 elem

type Generate vect m elem =
  Reflectable m Int
  => Compare m NegOne GT
  => Proxy m
  -> ( forall i
        . Compare i NegOne GT
       => Compare i m LT
       => Reflectable i Int
       => Proxy i
       -> elem
     )
  -> vect m elem

type MapWithTerm vect m elem elem' =
  Reflectable m Int
  => Compare m NegOne GT
  => ( forall i
        . Compare i NegOne GT
       => Compare i m LT
       => Reflectable i Int
       => Proxy i
       -> elem
       -> elem'
     )
  -> vect m elem
  -> vect m elem'
