module Data.FastVect.Common.Matrix where

import Prelude

import Data.FastVect.Common (NegOne, Zero)
import Data.Reflectable (class Reflectable)
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy)

type Index matrix h w i j elem =
  Compare h NegOne GT
  => Compare w NegOne GT
  => Compare i NegOne GT
  => Compare j NegOne GT
  => Compare i h LT
  => Compare j w LT
  => Reflectable i Int
  => Reflectable j Int
  => Proxy i
  -> Proxy j
  -> matrix h w elem
  -> elem

type IndexModulo matrix h w elem =
  Compare h Zero GT
  => Compare w Zero GT
  => Reflectable h Int
  => Reflectable w Int
  => Int
  -> Int
  -> matrix h w elem
  -> elem

type Replicate matrix h w elem =
  Compare h NegOne GT
  => Reflectable h Int
  => Compare w NegOne GT
  => Reflectable w Int
  => Proxy h
  -> Proxy w
  -> elem
  -> matrix h w elem

type Generate matrix h w elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => Proxy h
  -> Proxy w
  -> ( forall i j
        . Reflectable i Int
       => Reflectable j Int
       => Compare i h LT
       => Compare j w LT
       => Compare i NegOne GT
       => Compare j NegOne GT
       => Proxy i
       -> Proxy j
       -> elem
     )
  -> matrix h w elem

type MapWithTerm matrix h w elem elem' =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => ( forall i j
        . Reflectable i Int
       => Reflectable j Int
       => Compare i h LT
       => Compare j w LT
       => Compare i NegOne GT
       => Compare j NegOne GT
       => Proxy i
       -> Proxy j
       -> elem
       -> elem'
     )
  -> matrix h w elem
  -> matrix h w elem'

type Transpose :: forall k. (Int -> Int -> k -> Type) -> Int -> Int -> k -> Type
type Transpose matrix h w elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => matrix h w elem
  -> matrix w h elem

type DotProduct vect h elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Semiring elem
  => vect h elem
  -> vect h elem
  -> elem

type OuterMap vect matrix h w elemH elemW elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => (elemH -> elemW -> elem)
  -> vect h elemH
  -> vect w elemW
  -> matrix h w elem

type OuterProduct vect matrix h w elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => Semiring elem
  => vect h elem
  -> vect w elem
  -> matrix h w elem

type Diag vect matrix h elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Semiring elem
  => vect h elem
  -> matrix h h elem

type Traced :: forall k. (Int -> k -> Type) -> (Int -> Int -> k -> Type) -> Int -> k -> Type
type Traced vect matrix h elem =
  Reflectable h Int
  => Compare h NegOne GT
  => matrix h h elem
  -> vect h elem

type Trace matrix h elem =
  Reflectable h Int
  => Semiring elem
  => Compare h NegOne GT
  => matrix h h elem
  -> elem

type Transform vect matrix h w elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => Semiring elem
  => matrix h w elem
  -> vect w elem
  -> vect h elem

type Product matrix h m w elem =
  Reflectable h Int
  => Compare h NegOne GT
  => Reflectable m Int
  => Compare m NegOne GT
  => Reflectable w Int
  => Compare w NegOne GT
  => Semiring elem
  => matrix h m elem
  -> matrix m w elem
  -> matrix h w elem

type Empty :: forall k1 k2. (Int -> Int -> k1 -> k2) -> k1 -> k2
type Empty matrix elem = matrix Zero Zero elem

type Modify matrix h w i j elem =
  Reflectable i Int
  => Compare i NegOne GT
  => Compare h NegOne GT
  => Compare i h LT
  => Reflectable j Int
  => Compare j NegOne GT
  => Compare w NegOne GT
  => Compare j w LT
  => Proxy i
  -> Proxy j
  -> (elem -> elem)
  -> matrix h w elem
  -> matrix h w elem

type Set matrix h w i j elem =
  Reflectable i Int
  => Compare i NegOne GT
  => Compare h NegOne GT
  => Compare i h LT
  => Reflectable j Int
  => Compare j NegOne GT
  => Compare w NegOne GT
  => Compare j w LT
  => Proxy i
  -> Proxy j
  -> elem
  -> matrix h w elem
  -> matrix h w elem
