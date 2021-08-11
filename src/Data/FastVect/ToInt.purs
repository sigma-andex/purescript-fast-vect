module Data.FastVect.ToInt where

import Prelude
import Data.Function (applyN)
import Data.Tuple (Tuple(..), snd)
import Prim.Symbol (class Cons)
import Type.Proxy (Proxy(..))

class ToIntSingle (sym ∷ Symbol) where
  toIntSingle ∷ Proxy sym → Int

instance ToIntSingle "0" where
  toIntSingle _ = 0
else instance ToIntSingle "1" where
  toIntSingle _ = 1
else instance ToIntSingle "2" where
  toIntSingle _ = 2
else instance ToIntSingle "3" where
  toIntSingle _ = 3
else instance ToIntSingle "4" where
  toIntSingle _ = 4
else instance ToIntSingle "5" where
  toIntSingle _ = 5
else instance ToIntSingle "6" where
  toIntSingle _ = 6
else instance ToIntSingle "7" where
  toIntSingle _ = 7
else instance ToIntSingle "8" where
  toIntSingle _ = 8
else instance ToIntSingle "9" where
  toIntSingle _ = 9

class ToIntHelper (sym ∷ Symbol) where
  toIntHelper ∷ Proxy sym → Tuple Int Int

instance ToIntHelper "" where
  toIntHelper _ = Tuple 0 0
else instance
  ( Cons head tail sym
  , ToIntHelper tail
  , ToIntSingle head
  ) ⇒
  ToIntHelper sym where
  toIntHelper _ = case toIntHelper (Proxy ∷ Proxy tail) of
    Tuple currentExp result →
      let
        newExp = currentExp + 1
        newResult = applyN (_ * 10) currentExp (toIntSingle (Proxy ∷ Proxy head)) + result
      in
        Tuple newExp newResult

class ToInt (sym ∷ Symbol) where
  toInt ∷ Proxy sym → Int

instance (ToIntHelper sym) ⇒ ToInt sym where
  toInt = toIntHelper >>> snd
