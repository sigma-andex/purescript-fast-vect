module Data.FastVect.FastVect
  ( (:)
  , Vect
  , adjust
  , adjustM
  , append
  , cons
  , drop
  , empty
  , fromArray
  , generate
  , head
  , last
  , index
  , indexModulo
  , mapWithTerm
  , modify
  , reifyVect
  , replicate
  , set
  , singleton
  , snoc
  , splitAt
  , take
  , toArray
  , toNonEmptyArray
  ) where

import Prelude hiding (append)

import Control.Apply (lift2)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray(NonEmptyArray))
import Data.Distributive (class Distributive, collectDefault, distribute)
import Data.FastVect.Common as Common
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Reflectable (class Reflectable)
import Data.Semigroup.Foldable as Foldable1
import Data.Semigroup.Traversable as Traversable1
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Prelude as Prelude
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Vect :: Int -> Type -> Type
-- | A Vector: A list-like data structure that encodes it's length in the type, backed by an `Array`.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
newtype Vect len elem = Vect (Array elem)

instance (Show elem, Reflectable len Int) => Show (Vect len elem) where
  show (Vect elems) = "Vect " <> show (Common.toInt (Common.term :: _ len)) <> " " <> show elems

derive newtype instance Eq elem => Eq (Vect len elem)
derive newtype instance Ord elem => Ord (Vect len elem)
derive newtype instance Functor (Vect len)
instance Apply (Vect len) where
  apply (Vect fab) (Vect a) = Vect (Array.zipWith ($) fab a)

instance (Compare len Common.NegOne GT, Reflectable len Int) => Applicative (Vect len) where
  pure = replicate (Proxy :: _ len)

instance (Compare len Common.NegOne GT, Reflectable len Int) => Bind (Vect len) where
  bind vec f = distribute f <*> vec

instance (Compare len Common.NegOne GT, Reflectable len Int) => Monad (Vect len)
derive newtype instance FunctorWithIndex Int (Vect len)
derive newtype instance Foldable (Vect len)
derive newtype instance FoldableWithIndex Int (Vect len)

instance (Compare len Common.Zero GT) => Foldable1.Foldable1 (Vect len) where
  foldMap1 f xs = Foldable1.foldMap1 f $ toNonEmptyArray xs
  foldr1 f xs = Foldable1.foldr1 f $ toNonEmptyArray xs
  foldl1 f xs = Foldable1.foldl1 f $ toNonEmptyArray xs

derive newtype instance Traversable (Vect len)
derive newtype instance TraversableWithIndex Int (Vect len)

instance (Compare len Common.Zero GT) => Traversable1.Traversable1 (Vect len) where
  traverse1 = Traversable1.traverse1Default
  sequence1 xs = unsafeFromNonEmptyArray <$> (Traversable1.sequence1 $ toNonEmptyArray xs)
    where
    -- assumes the Traversable1 instance for NonEmptyArray keeps the same
    -- amount of elements
    unsafeFromNonEmptyArray = NEA.toArray >>> Vect

instance (Compare len Common.NegOne GT, Reflectable len Int) => Distributive (Vect len) where
  distribute :: forall a g. Functor g => g (Vect len a) -> Vect len (g a)
  distribute xss = generate (Proxy :: _ len) f
    where
    f :: forall i. Compare i Common.NegOne GT => Compare i len LT => Reflectable i Int => Proxy i -> g a
    f _ = index (Proxy :: _ i) <$> xss
  collect = collectDefault

instance Semigroup a => Semigroup (Vect len a) where
  append = lift2 Prelude.append

instance (Compare len Common.NegOne GT, Reflectable len Int, Monoid a) => Monoid (Vect len a) where
  mempty = pure mempty

instance (Compare len Common.NegOne GT, Reflectable len Int, Semiring a) => Semiring (Vect len a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance (Compare len Common.NegOne GT, Reflectable len Int, Ring a) => Ring (Vect len a) where
  sub = lift2 sub

instance (Compare len Common.NegOne GT, Reflectable len Int, CommutativeRing a) => CommutativeRing (Vect len a)

-- | Create a `Vect` by replicating `len` times the given element
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- | ```
replicate :: forall len elem. Common.Replicate Vect len elem
replicate proxy elem = Vect $ Array.replicate (Common.toInt proxy) elem

-- | Creates the empty `Vect`.
-- |
-- | ```
-- | vect :: Vect 0 String
-- | vect = empty
-- | ```
empty :: forall elem. Common.Empty Vect elem
empty = Vect []

-- | Create a `Vect` of one element.
-- |
-- | ```
-- | vect :: Vect 1 String
-- | vect = singleton "a"
-- | ```
singleton :: forall elem. Common.Singleton Vect elem
singleton elem = Vect [ elem ]

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
append (Vect xs) (Vect ys) = Vect (xs <> ys)

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
drop proxy (Vect xs) = Vect (Array.drop (Common.toInt proxy) xs)

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
take proxy (Vect xs) = Vect (Array.take (Common.toInt proxy) xs)

foreign import modifyImpl :: forall n elem. Int -> (elem -> elem) -> Vect n elem -> Vect n elem

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
modify proxy = modifyImpl (Common.toInt proxy)

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
set proxy = modify proxy <<< const

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
splitAt proxy (Vect xs) = { before: Vect before, after: Vect after }
  where
  { before, after } = Array.splitAt (Common.toInt proxy) xs

-- | Safely access the `n`-th modulo m element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = indexModulo 5352523 vect
-- | ```
indexModulo :: forall m elem. Common.IndexModulo Vect m elem
indexModulo i = indexImpl (i `mod` Common.toInt (Proxy :: _ m))

foreign import indexImpl :: forall m elem. Int -> Vect m elem -> elem

-- | Safely access the `i`-th element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = index (Common.term :: _ 299) vect
-- | ```
index :: forall m n elem. Common.Index Vect m n elem
index = indexImpl <<< Common.toInt

-- | Safely access the head of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = head vect
-- | ```
head :: forall m elem. Common.Head Vect m elem
head = indexImpl 0

-- | Safely access the last element of a `Vect`.
-- |
-- | ```
-- | vect :: Vect 300 String
-- | vect = replicate (Common.term :: _ 300) "a"
-- |
-- | elem :: String
-- | elem = last vect
-- | ```
last :: forall m elem. Common.Last Vect m elem
last = indexImpl $ Common.toInt (Common.term :: _ m) - 1

-- | Attempt to create a `Vect` of a given size from an `Array`.
-- |
-- | ```
-- | fromArray (Common.term :: _ 3) ["a", "b", "c"] = Just (Vect (Common.term :: _ 3) ["a", "b", "c"])
-- |
-- | fromArray (Common.term :: _ 4) ["a", "b", "c"] = Nothing
-- | ```
fromArray
  :: forall len elem
   . Reflectable len Int
  => Compare len Common.NegOne GT
  => Proxy len
  -> Array elem
  -> Maybe (Vect len elem)
fromArray proxy array | Array.length array == Common.toInt proxy = Just (Vect array)
fromArray _ _ = Nothing

-- | Converts the `Vect` to an `Array`, effectively dropping the size information.
toArray
  :: forall len elem
   . Compare len Common.NegOne GT
  => Vect len elem
  -> Array elem
toArray (Vect arr) = arr

-- | Converts the `Vect` to an `NonEmptyArray`, dropping most of the size information.
toNonEmptyArray
  :: forall len elem
   . Compare len Common.Zero GT
  => Vect len elem
  -> NEA.NonEmptyArray elem
toNonEmptyArray (Vect arr) = NonEmptyArray arr

-- | Creates a `Vect` by adjusting the given `Array`, padding with the provided element if the array is to small or dropping elements if the array is to big.
-- |
-- | ```
-- | toArray $ adjust (Common.term :: _ 10) 0 [ 1, 2, 3 ] == [ 0, 0, 0, 0, 0, 0, 0, 1, 2, 3 ]
-- |
-- | toArray $ adjust (Common.term :: _ 3) 0 [ 0, 0, 0, 0, 1, 2, 3 ] == [ 1, 2, 3 ]
-- | ```
adjust
  :: forall len elem
   . Reflectable len Int
  => Compare len Common.NegOne GT
  => Proxy len
  -> elem
  -> Array elem
  -> Vect len elem
adjust proxy elem array = case Array.length array - Common.toInt proxy of
  0 -> Vect array
  len | len < 0 -> Vect $ Array.replicate (abs len) elem <> array
  len -> Vect $ Array.drop len array

-- | Like `adjust` but uses the Moinoid instance of elem to create the elements.
adjustM
  :: forall len elem
   . Monoid elem
  => Reflectable len Int
  => Compare len Common.NegOne GT
  => Proxy len
  -> Array elem
  -> Vect len elem
adjustM proxy = adjust proxy mempty

-- | Attaches an element to the front of the `Vect`, creating a new `Vect` with size incremented.
-- |
-- | Note, the running time of this function is `O(n)`.
cons :: forall len len_plus_1 elem. Common.Cons Vect len len_plus_1 elem
cons elem (Vect arr) = Vect (Array.cons elem arr)

-- | Attaches an element to the end of the `Vect`, creating a new `Vect` with size incremented.
snoc
  :: forall len len_plus_1 elem. Common.Snoc Vect len len_plus_1 elem
snoc (Vect arr) elem = Vect (Array.snoc arr elem)

infixr 6 cons as :
infixr 6 index as !!
infixr 6 indexModulo as !%

-- | Applies a function to `Array` that takes a Vector of arbitrary length.
reifyVect
  :: forall elem r
   . Array elem
  -> (forall len. Vect len elem -> r)
  -> r
reifyVect arr f = f (Vect arr)

-- | Converts a function that takes any type-level integer value from 0 to len into a function that takes Int.
-- | Similar implementation to reifyType, but unsafe because type classes such as `Compare` are not ensured (so no export).
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
generate _ f = Vect $ map (\i -> unsafeCoerceTerm (Proxy :: _ len) f i) $ Array.range 0 (Common.toInt (Proxy :: _ len) - 1)

-- | Map a function over a `Vect` with the type level index of each element.
mapWithTerm :: forall len elem elem'. Common.MapWithTerm Vect len elem elem'
mapWithTerm f vect = mapWithIndex (\i elem -> unsafeCoerceTerm (Proxy :: _ len) f i elem) vect

instance Common.IsVect (Vect n)
