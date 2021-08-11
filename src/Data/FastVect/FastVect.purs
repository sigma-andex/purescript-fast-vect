module Data.FastVect.FastVect
  ( Vect
  , replicate
  , empty
  , singleton
  , append
  , drop
  , take
  ) where

import Prelude
import Data.Array as A
import Data.FastVect.Add (class Add, term)
import Data.FastVect.ToInt (class ToInt, toInt)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Symbol (class Cons)
import Type.Proxy (Proxy)

data Vect ∷ Symbol → Type → Type
data Vect len elem
  = Vect (Proxy len) (Array elem)

instance (IsSymbol len, Show elem) ⇒ Show (Vect len elem) where
  show (Vect proxy elems) = "Vect " <> (reflectSymbol proxy) <> " " <> show elems

instance Functor (Vect len) where
  map f (Vect proxy xs) = Vect proxy (map f xs)

replicate ∷ ∀ len elem. ToInt len ⇒ Proxy len → elem → Vect len elem
replicate proxy elem = Vect proxy $ A.replicate (toInt proxy) elem

empty ∷ ∀ elem. Vect "0" elem
empty = Vect (term ∷ _ "0") []

singleton ∷ ∀ elem. elem → Vect "1" elem
singleton elem = Vect (term ∷ _ "1") [ elem ]

append ∷ ∀ m n elem carry sum m_plus_n. Cons carry sum m_plus_n ⇒ Add m n carry sum ⇒ Vect m elem → Vect n elem → Vect m_plus_n elem
append (Vect _ xs) (Vect _ ys) = Vect (term ∷ _ m_plus_n) (xs <> ys)

drop ∷ ∀ m n elem carry sum m_plus_n. ToInt m ⇒ Cons carry sum m_plus_n ⇒ Add m n carry sum ⇒ Proxy m → Vect m_plus_n elem → Vect n elem
drop proxy (Vect _ xs) = Vect (term ∷ _ n) (A.drop (toInt proxy) xs)

take ∷ ∀ m n elem carry sum m_plus_n. ToInt m ⇒ Cons carry sum m_plus_n ⇒ Add m n carry sum ⇒ Proxy m → Vect m_plus_n elem → Vect m elem
take proxy (Vect _ xs) = Vect proxy (A.take (toInt proxy) xs)
