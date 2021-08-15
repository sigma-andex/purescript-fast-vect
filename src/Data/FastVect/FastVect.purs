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
  ) where

import Prelude
import Data.Array (unsafeIndex)
import Data.Array as A
import Data.FastVect.Add (class Add, class AlignToSum, class Trim, term)
import Data.FastVect.ToInt (class ToInt, toInt)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafePartial)
import Prim.Symbol (class Cons)
import Type.Proxy (Proxy)

data Vect ∷ Symbol → Type → Type
data Vect len elem
  = Vect (Proxy len) (Array elem)

instance (IsSymbol len, Show elem) ⇒ Show (Vect len elem) where
  show (Vect proxy elems) = "Vect " <> (reflectSymbol proxy) <> " " <> show elems

instance Functor (Vect len) where
  map f (Vect proxy xs) = Vect proxy (map f xs)

replicate ∷
  ∀ len elem.
  ToInt len ⇒
  Proxy len → elem → Vect len elem
replicate proxy elem = Vect proxy $ A.replicate (toInt proxy) elem

empty ∷
  ∀ elem.
  Vect "0" elem
empty = Vect (term ∷ _ "0") []

singleton ∷
  ∀ elem.
  elem → Vect "1" elem
singleton elem = Vect (term ∷ _ "1") [ elem ]

append ∷
  ∀ m n elem carry sum m_plus_n_untrimmed m_plus_n m_aligned n_aligned.
  AlignToSum m n m_aligned n_aligned ⇒
  Add m_aligned n_aligned carry sum ⇒
  Cons carry sum m_plus_n_untrimmed ⇒
  Trim m_plus_n_untrimmed m_plus_n ⇒
  Vect m elem → Vect n elem → Vect m_plus_n elem
append (Vect _ xs) (Vect _ ys) = Vect (term ∷ _ m_plus_n) (xs <> ys)

drop ∷
  ∀ m n elem carry sum m_plus_n.
  ToInt m ⇒
  Cons carry sum m_plus_n ⇒
  Add m n carry sum ⇒
  Proxy m → Vect m_plus_n elem → Vect n elem
drop proxy (Vect _ xs) = Vect (term ∷ _ n) (A.drop (toInt proxy) xs)

take ∷
  ∀ m n elem m_plus_n aligned_m aligned_m_plus_n.
  ToInt m ⇒
  AlignToSum m m_plus_n aligned_m aligned_m_plus_n ⇒
  Add aligned_m n "0" aligned_m_plus_n ⇒
  Proxy m → Vect m_plus_n elem → Vect m elem
take proxy (Vect _ xs) = Vect proxy (A.take (toInt proxy) xs)

index ∷
  ∀ m m_minus_one aligned_one aligned_m aligned_m_minus_one i aligned_i n elem.
  ToInt i ⇒
  AlignToSum "1" m aligned_one aligned_m ⇒
  Add aligned_one m_minus_one "0" aligned_m ⇒
  AlignToSum i m_minus_one aligned_i aligned_m_minus_one ⇒
  Add aligned_i n "0" aligned_m_minus_one ⇒
  Proxy i → Vect m elem → elem
index proxy (Vect _ xs) = unsafePartial $ unsafeIndex xs (toInt proxy)

head ∷
  ∀ m m_minus_one aligned_one aligned_m aligned_m_minus_one aligned_i n elem.
  AlignToSum "1" m aligned_one aligned_m ⇒
  Add aligned_one m_minus_one "0" aligned_m ⇒
  AlignToSum "0" m_minus_one aligned_i aligned_m_minus_one ⇒
  Add aligned_i n "0" aligned_m_minus_one ⇒
  Vect m elem → elem
head = index (term ∷ _ "0")
