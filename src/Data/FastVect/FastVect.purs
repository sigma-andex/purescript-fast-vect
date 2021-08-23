module Data.FastVect.FastVect
  ( Vect
  , replicate
  , empty
  , singleton
  , append
  , drop
  , take
  , index
  , head
  , splitAt
  , fromArray
  , toArray
  , adjust
  , adjustM
  , cons
  , (:)
  ) where

import Prelude

import Data.Array (length, unsafeIndex)
import Data.Array as A
import Data.FastVect.Add (class Add, class PadZeroes, class Trim, term)
import Data.FastVect.ToInt (class ToInt, toInt)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafePartial)
import Prim.Symbol (class Cons)
import Type.Proxy (Proxy)

data Vect ∷ Symbol → Type → Type
-- | A Vector: A list-like data structure that encodes it's length in the type, backed by an `Array`.
-- | 
-- | ```
-- | vect :: Vect "1" String
-- | vect = singleton "a"
-- | ```
data Vect len elem
  = Vect (Proxy len) (Array elem)

instance (IsSymbol len, Show elem) ⇒ Show (Vect len elem) where
  show (Vect proxy elems) = "Vect " <> (reflectSymbol proxy) <> " " <> show elems

instance Eq elem ⇒ Eq (Vect len elem) where
  eq (Vect _ arr1) (Vect _ arr2) = eq arr1 arr2

instance Functor (Vect len) where
  map f (Vect proxy xs) = Vect proxy (map f xs)

-- | Create a `Vect` by replicating `len` times the given element 
-- | 
-- | ```
-- | vect :: Vect "300" String
-- | vect = replicate (term :: _ "300") "a"
-- | ```
replicate ∷
  ∀ len elem.
  ToInt len ⇒
  Proxy len → elem → Vect len elem
replicate proxy elem = Vect proxy $ A.replicate (toInt proxy) elem

-- | Creates the empty `Vect`.
empty ∷
  ∀ elem.
  Vect "0" elem
empty = Vect (term ∷ _ "0") []

-- | Create a `Vect` of one element.
-- | 
-- | ```
-- | vect :: Vect "1" String
-- | vect = singleton "a"
-- | ```
singleton ∷
  ∀ elem.
  elem → Vect "1" elem
singleton elem = Vect (term ∷ _ "1") [ elem ]

-- | Append two `Vect`s.
-- | 
-- | ```
-- | as :: Vect "300" String
-- | as = replicate (term :: _ "300") "a"
-- | 
-- | bs :: Vect "200" String
-- | bs = replicate (term :: _ "200") "b"
-- | 
-- | cs :: Vect "500" String
-- | cs = append as bs
-- | ```
append ∷
  ∀ m n elem carry sum m_plus_n_untrimmed m_plus_n m_aligned n_aligned.
  PadZeroes m n m_aligned n_aligned ⇒
  Add m_aligned n_aligned carry sum ⇒
  Cons carry sum m_plus_n_untrimmed ⇒
  Trim m_plus_n_untrimmed m_plus_n ⇒
  Vect m elem → Vect n elem → Vect m_plus_n elem
append (Vect _ xs) (Vect _ ys) = Vect (term ∷ _ m_plus_n) (xs <> ys)

-- | Drop `m` elements from a `Vect`.
-- | 
-- | ```
-- | vect :: Vect "300" String
-- | vect = replicate (term :: _ "300") "a"
-- | 
-- | newVect :: Vect "200" String
-- | newVect = drop (term :: _ "100") vect
-- | ```
drop ∷
  ∀ m n elem m_aligned m_plus_n_aligned n_untrimmed m_plus_n.
  ToInt m ⇒
  PadZeroes m m_plus_n m_aligned m_plus_n_aligned ⇒
  Add m_aligned n_untrimmed "0" m_plus_n_aligned ⇒
  Trim n_untrimmed n ⇒
  Proxy m → Vect m_plus_n elem → Vect n elem
drop proxy (Vect _ xs) = Vect (term ∷ _ n) (A.drop (toInt proxy) xs)

-- | Take `m` elements from a `Vect`.
-- | 
-- | ```
-- | vect :: Vect "300" String
-- | vect = replicate (term :: _ "300") "a"
-- | 
-- | newVect :: Vect "100" String
-- | newVect = take (term :: _ "100") vect
-- | ```
take ∷
  ∀ m n elem m_plus_n m_aligned m_aligned_plus_n n_untrimmed.
  ToInt m ⇒
  PadZeroes m m_plus_n m_aligned m_aligned_plus_n ⇒
  Add m_aligned n_untrimmed "0" m_aligned_plus_n ⇒
  Trim n_untrimmed n ⇒
  Proxy m → Vect m_plus_n elem → Vect m elem
take proxy (Vect _ xs) = Vect proxy (A.take (toInt proxy) xs)

-- | Split the `Vect` into two sub vectors `before` and `after`, where before contains up to `m` elements.
-- | 
-- | ```
-- | vect ∷ Vect "10" String
-- | vect = replicate (term ∷ _ "10") "a"
-- | 
-- | split ∷
-- |   { after ∷ Vect "7" String
-- |   , before ∷ Vect "3" String
-- |   }
-- | split = splitAt (term ∷ _ "3") vect
-- | ```
splitAt ∷
  ∀ m n elem m_aligned m_plus_n_aligned n_untrimmed m_plus_n.
  ToInt m ⇒
  PadZeroes m m_plus_n m_aligned m_plus_n_aligned ⇒
  Add m_aligned n_untrimmed "0" m_plus_n_aligned ⇒
  Trim n_untrimmed n ⇒
  Proxy m → Vect m_plus_n elem → { before :: Vect m elem, after :: Vect n elem }
splitAt proxy (Vect _ xs) = { before: Vect proxy before, after: Vect (term ∷ _ n) after }
    where 
        { before, after} = A.splitAt (toInt proxy) xs

-- | Safely access the `i`-th element of a `Vect`.
-- | 
-- | ```
-- | vect :: Vect "300" String
-- | vect = replicate (term :: _ "300") "a"
-- | 
-- | elem :: String
-- | elem = index (term :: _ "299") vect
-- | ```
index ∷
  ∀ m m_minus_one aligned_one m_aligned m_aligned_minus_one i i_aligned n elem.
  ToInt i ⇒
  PadZeroes "1" m aligned_one m_aligned ⇒
  Add aligned_one m_minus_one "0" m_aligned ⇒
  PadZeroes i m_minus_one i_aligned m_aligned_minus_one ⇒
  Add i_aligned n "0" m_aligned_minus_one ⇒
  Proxy i → Vect m elem → elem
index proxy (Vect _ xs) = unsafePartial $ unsafeIndex xs (toInt proxy)

-- | Safely access the head of a `Vect`.
-- | 
-- | ```
-- | vect :: Vect "300" String
-- | vect = replicate (term :: _ "300") "a"
-- | 
-- | elem :: String
-- | elem = head vect
-- | ```
head ∷
  ∀ m m_minus_one aligned_one m_aligned m_aligned_minus_one i_aligned n elem.
  PadZeroes "1" m aligned_one m_aligned ⇒
  Add aligned_one m_minus_one "0" m_aligned ⇒
  PadZeroes "0" m_minus_one i_aligned m_aligned_minus_one ⇒
  Add i_aligned n "0" m_aligned_minus_one ⇒
  Vect m elem → elem
head = index (term ∷ _ "0")

-- | Attempt to create a `Vect` of a given size from an `Array`. 
-- | 
-- | ```
-- | fromArray (term :: _ "3") ["a", "b", "c"] = Just (Vect (term :: _ "3") ["a", "b", "c"])
-- | 
-- | fromArray (term :: _ "4") ["a", "b", "c"] = Nothing
-- | ```
fromArray ∷ ∀ len elem. ToInt len ⇒ Proxy len → Array elem → Maybe (Vect len elem)
fromArray proxy array | length array == toInt proxy = Just (Vect proxy array)
fromArray _ _ = Nothing

-- | Converts the `Vect` to an `Array`, effectively dropping the size information.
toArray ∷ ∀ len elem. Vect len elem → Array elem
toArray (Vect _ arr) = arr

-- | Creates a `Vect` by adjusting the given `Array`, padding with the provided element if the array is to small or dropping elements if the array is to big.
-- | 
-- | ```
-- | toArray $ adjust (term ∷ _ "10") 0 [ 1, 2, 3 ] == [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3 ]
-- | 
-- | toArray $ adjust (term ∷ _ "3") 0 [ 0, 0, 0, 0, 1, 2, 3 ] == [ 1, 2, 3 ]
-- | ```
adjust ∷ ∀ len elem. ToInt len ⇒ Proxy len → elem → Array elem → Vect len elem
adjust proxy elem array = case length array - toInt proxy of
  0 → Vect proxy array
  len | len < 0 → Vect proxy $ A.replicate (abs len) elem <> array
  len → Vect proxy $ A.drop len array

-- | Like `adjust` but uses the Moinoid instance of elem to create the elements.
adjustM ∷ ∀ len elem. Monoid elem ⇒ ToInt len ⇒ Proxy len → Array elem → Vect len elem
adjustM proxy = adjust proxy mempty

-- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented. 
-- | 
-- | Note, the running time of this function is `O(n)`.
cons ∷
  ∀ len elem one_aligned len_aligned carry sum len_plus_1_untrimmed len_plus_1.
  PadZeroes "1" len one_aligned len_aligned ⇒
  Add one_aligned len carry sum ⇒
  Cons carry sum len_plus_1_untrimmed ⇒
  Trim len_plus_1_untrimmed len_plus_1 ⇒
  elem → Vect len elem → Vect len_plus_1 elem
cons elem (Vect _ arr) = Vect (term ∷ _ len_plus_1) (A.cons elem arr)

infixr 6 cons as :
