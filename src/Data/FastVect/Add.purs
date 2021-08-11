module Data.FastVect.Add where

import Prim.Symbol (class Cons)
import Type.Proxy (Proxy(..))

class AddSingle (augend ∷ Symbol) (addend ∷ Symbol) (carry ∷ Symbol) (sum ∷ Symbol) | augend addend → carry sum
, augend sum → addend carry

instance AddSingle "0" "0" "0" "0"
else instance AddSingle "0" "0" "0" "0"
else instance AddSingle "1" "0" "0" "1"
else instance AddSingle "2" "0" "0" "2"
else instance AddSingle "3" "0" "0" "3"
else instance AddSingle "4" "0" "0" "4"
else instance AddSingle "5" "0" "0" "5"
else instance AddSingle "6" "0" "0" "6"
else instance AddSingle "7" "0" "0" "7"
else instance AddSingle "8" "0" "0" "8"
else instance AddSingle "9" "0" "0" "9"
else instance AddSingle "0" "1" "0" "1"
else instance AddSingle "1" "1" "0" "2"
else instance AddSingle "2" "1" "0" "3"
else instance AddSingle "3" "1" "0" "4"
else instance AddSingle "4" "1" "0" "5"
else instance AddSingle "5" "1" "0" "6"
else instance AddSingle "6" "1" "0" "7"
else instance AddSingle "7" "1" "0" "8"
else instance AddSingle "8" "1" "0" "9"
else instance AddSingle "9" "1" "1" "0"
else instance AddSingle "0" "2" "0" "2"
else instance AddSingle "1" "2" "0" "3"
else instance AddSingle "2" "2" "0" "4"
else instance AddSingle "3" "2" "0" "5"
else instance AddSingle "4" "2" "0" "6"
else instance AddSingle "5" "2" "0" "7"
else instance AddSingle "6" "2" "0" "8"
else instance AddSingle "7" "2" "0" "9"
else instance AddSingle "8" "2" "1" "0"
else instance AddSingle "9" "2" "1" "1"
else instance AddSingle "0" "3" "0" "3"
else instance AddSingle "1" "3" "0" "4"
else instance AddSingle "2" "3" "0" "5"
else instance AddSingle "3" "3" "0" "6"
else instance AddSingle "4" "3" "0" "7"
else instance AddSingle "5" "3" "0" "8"
else instance AddSingle "6" "3" "0" "9"
else instance AddSingle "7" "3" "1" "0"
else instance AddSingle "8" "3" "1" "1"
else instance AddSingle "9" "3" "1" "2"
else instance AddSingle "0" "4" "0" "4"
else instance AddSingle "1" "4" "0" "5"
else instance AddSingle "2" "4" "0" "6"
else instance AddSingle "3" "4" "0" "7"
else instance AddSingle "4" "4" "0" "8"
else instance AddSingle "5" "4" "0" "9"
else instance AddSingle "6" "4" "1" "0"
else instance AddSingle "7" "4" "1" "1"
else instance AddSingle "8" "4" "1" "2"
else instance AddSingle "9" "4" "1" "3"
else instance AddSingle "0" "5" "0" "5"
else instance AddSingle "1" "5" "0" "6"
else instance AddSingle "2" "5" "0" "7"
else instance AddSingle "3" "5" "0" "8"
else instance AddSingle "4" "5" "0" "9"
else instance AddSingle "5" "5" "1" "0"
else instance AddSingle "6" "5" "1" "1"
else instance AddSingle "7" "5" "1" "2"
else instance AddSingle "8" "5" "1" "3"
else instance AddSingle "9" "5" "1" "4"
else instance AddSingle "0" "6" "0" "6"
else instance AddSingle "1" "6" "0" "7"
else instance AddSingle "2" "6" "0" "8"
else instance AddSingle "3" "6" "0" "9"
else instance AddSingle "4" "6" "1" "0"
else instance AddSingle "5" "6" "1" "1"
else instance AddSingle "6" "6" "1" "2"
else instance AddSingle "7" "6" "1" "3"
else instance AddSingle "8" "6" "1" "4"
else instance AddSingle "9" "6" "1" "5"
else instance AddSingle "0" "7" "0" "7"
else instance AddSingle "1" "7" "0" "8"
else instance AddSingle "2" "7" "0" "9"
else instance AddSingle "3" "7" "1" "0"
else instance AddSingle "4" "7" "1" "1"
else instance AddSingle "5" "7" "1" "2"
else instance AddSingle "6" "7" "1" "3"
else instance AddSingle "7" "7" "1" "4"
else instance AddSingle "8" "7" "1" "5"
else instance AddSingle "9" "7" "1" "6"
else instance AddSingle "0" "8" "0" "8"
else instance AddSingle "1" "8" "0" "9"
else instance AddSingle "2" "8" "1" "0"
else instance AddSingle "3" "8" "1" "1"
else instance AddSingle "4" "8" "1" "2"
else instance AddSingle "5" "8" "1" "3"
else instance AddSingle "6" "8" "1" "4"
else instance AddSingle "7" "8" "1" "5"
else instance AddSingle "8" "8" "1" "6"
else instance AddSingle "9" "8" "1" "7"
else instance AddSingle "0" "9" "0" "9"
else instance AddSingle "1" "9" "0" "0"
else instance AddSingle "2" "9" "1" "1"
else instance AddSingle "3" "9" "1" "2"
else instance AddSingle "4" "9" "1" "3"
else instance AddSingle "5" "9" "1" "4"
else instance AddSingle "6" "9" "1" "5"
else instance AddSingle "7" "9" "1" "6"
else instance AddSingle "8" "9" "1" "7"
else instance AddSingle "9" "9" "1" "8"



class AddSingle2 (augend ∷ Symbol) (addend ∷ Symbol) (carryPrevious ∷ Symbol) (carryNext ∷ Symbol) (sum ∷ Symbol) | augend addend carryPrevious → carryNext sum
, augend carryPrevious sum → addend carryNext 

instance AddSingle2 "0" "0" "0" "0" "0"
else instance AddSingle2 "1" "0" "0" "0" "1"
else instance AddSingle2 "2" "0" "0" "0" "2"
else instance AddSingle2 "3" "0" "0" "0" "3"
else instance AddSingle2 "4" "0" "0" "0" "4"
else instance AddSingle2 "5" "0" "0" "0" "5"
else instance AddSingle2 "6" "0" "0" "0" "6"
else instance AddSingle2 "7" "0" "0" "0" "7"
else instance AddSingle2 "8" "0" "0" "0" "8"
else instance AddSingle2 "9" "0" "0" "0" "9"
else instance AddSingle2 "0" "1" "0" "0" "1"
else instance AddSingle2 "1" "1" "0" "0" "2"
else instance AddSingle2 "2" "1" "0" "0" "3"
else instance AddSingle2 "3" "1" "0" "0" "4"
else instance AddSingle2 "4" "1" "0" "0" "5"
else instance AddSingle2 "5" "1" "0" "0" "6"
else instance AddSingle2 "6" "1" "0" "0" "7"
else instance AddSingle2 "7" "1" "0" "0" "8"
else instance AddSingle2 "8" "1" "0" "0" "9"
else instance AddSingle2 "9" "1" "0" "1" "0"
else instance AddSingle2 "0" "2" "0" "0" "2"
else instance AddSingle2 "1" "2" "0" "0" "3"
else instance AddSingle2 "2" "2" "0" "0" "4"
else instance AddSingle2 "3" "2" "0" "0" "5"
else instance AddSingle2 "4" "2" "0" "0" "6"
else instance AddSingle2 "5" "2" "0" "0" "7"
else instance AddSingle2 "6" "2" "0" "0" "8"
else instance AddSingle2 "7" "2" "0" "0" "9"
else instance AddSingle2 "8" "2" "0" "1" "0"
else instance AddSingle2 "9" "2" "0" "1" "1"
else instance AddSingle2 "0" "3" "0" "0" "3"
else instance AddSingle2 "1" "3" "0" "0" "4"
else instance AddSingle2 "2" "3" "0" "0" "5"
else instance AddSingle2 "3" "3" "0" "0" "6"
else instance AddSingle2 "4" "3" "0" "0" "7"
else instance AddSingle2 "5" "3" "0" "0" "8"
else instance AddSingle2 "6" "3" "0" "0" "9"
else instance AddSingle2 "7" "3" "0" "1" "0"
else instance AddSingle2 "8" "3" "0" "1" "1"
else instance AddSingle2 "9" "3" "0" "1" "2"
else instance AddSingle2 "0" "4" "0" "0" "4"
else instance AddSingle2 "1" "4" "0" "0" "5"
else instance AddSingle2 "2" "4" "0" "0" "6"
else instance AddSingle2 "3" "4" "0" "0" "7"
else instance AddSingle2 "4" "4" "0" "0" "8"
else instance AddSingle2 "5" "4" "0" "0" "9"
else instance AddSingle2 "6" "4" "0" "1" "0"
else instance AddSingle2 "7" "4" "0" "1" "1"
else instance AddSingle2 "8" "4" "0" "1" "2"
else instance AddSingle2 "9" "4" "0" "1" "3"
else instance AddSingle2 "0" "5" "0" "0" "5"
else instance AddSingle2 "1" "5" "0" "0" "6"
else instance AddSingle2 "2" "5" "0" "0" "7"
else instance AddSingle2 "3" "5" "0" "0" "8"
else instance AddSingle2 "4" "5" "0" "0" "9"
else instance AddSingle2 "5" "5" "0" "1" "0"
else instance AddSingle2 "6" "5" "0" "1" "1"
else instance AddSingle2 "7" "5" "0" "1" "2"
else instance AddSingle2 "8" "5" "0" "1" "3"
else instance AddSingle2 "9" "5" "0" "1" "4"
else instance AddSingle2 "0" "6" "0" "0" "6"
else instance AddSingle2 "1" "6" "0" "0" "7"
else instance AddSingle2 "2" "6" "0" "0" "8"
else instance AddSingle2 "3" "6" "0" "0" "9"
else instance AddSingle2 "4" "6" "0" "1" "0"
else instance AddSingle2 "5" "6" "0" "1" "1"
else instance AddSingle2 "6" "6" "0" "1" "2"
else instance AddSingle2 "7" "6" "0" "1" "3"
else instance AddSingle2 "8" "6" "0" "1" "4"
else instance AddSingle2 "9" "6" "0" "1" "5"
else instance AddSingle2 "0" "7" "0" "0" "7"
else instance AddSingle2 "1" "7" "0" "0" "8"
else instance AddSingle2 "2" "7" "0" "0" "9"
else instance AddSingle2 "3" "7" "0" "1" "0"
else instance AddSingle2 "4" "7" "0" "1" "1"
else instance AddSingle2 "5" "7" "0" "1" "2"
else instance AddSingle2 "6" "7" "0" "1" "3"
else instance AddSingle2 "7" "7" "0" "1" "4"
else instance AddSingle2 "8" "7" "0" "1" "5"
else instance AddSingle2 "9" "7" "0" "1" "6"
else instance AddSingle2 "0" "8" "0" "0" "8"
else instance AddSingle2 "1" "8" "0" "0" "9"
else instance AddSingle2 "2" "8" "0" "1" "0"
else instance AddSingle2 "3" "8" "0" "1" "1"
else instance AddSingle2 "4" "8" "0" "1" "2"
else instance AddSingle2 "5" "8" "0" "1" "3"
else instance AddSingle2 "6" "8" "0" "1" "4"
else instance AddSingle2 "7" "8" "0" "1" "5"
else instance AddSingle2 "8" "8" "0" "1" "6"
else instance AddSingle2 "9" "8" "0" "1" "7"
else instance AddSingle2 "0" "9" "0" "0" "9"
else instance AddSingle2 "1" "9" "0" "0" "0"
else instance AddSingle2 "2" "9" "0" "1" "1"
else instance AddSingle2 "3" "9" "0" "1" "2"
else instance AddSingle2 "4" "9" "0" "1" "3"
else instance AddSingle2 "5" "9" "0" "1" "4"
else instance AddSingle2 "6" "9" "0" "1" "5"
else instance AddSingle2 "7" "9" "0" "1" "6"
else instance AddSingle2 "8" "9" "0" "1" "7"
else instance AddSingle2 "9" "9" "0" "1" "8"
else instance AddSingle2 "0" "0" "1" "0" "1"
else instance AddSingle2 "1" "0" "1" "0" "2"
else instance AddSingle2 "2" "0" "1" "0" "3"
else instance AddSingle2 "3" "0" "1" "0" "4"
else instance AddSingle2 "4" "0" "1" "0" "5"
else instance AddSingle2 "5" "0" "1" "0" "6"
else instance AddSingle2 "6" "0" "1" "0" "7"
else instance AddSingle2 "7" "0" "1" "0" "8"
else instance AddSingle2 "8" "0" "1" "0" "9"
else instance AddSingle2 "9" "0" "1" "1" "0"
else instance AddSingle2 "0" "1" "1" "0" "2"
else instance AddSingle2 "1" "1" "1" "0" "3"
else instance AddSingle2 "2" "1" "1" "0" "4"
else instance AddSingle2 "3" "1" "1" "0" "5"
else instance AddSingle2 "4" "1" "1" "0" "6"
else instance AddSingle2 "5" "1" "1" "0" "7"
else instance AddSingle2 "6" "1" "1" "0" "8"
else instance AddSingle2 "7" "1" "1" "0" "9"
else instance AddSingle2 "8" "1" "1" "1" "0"
else instance AddSingle2 "9" "1" "1" "1" "1"
else instance AddSingle2 "0" "2" "1" "0" "3"
else instance AddSingle2 "1" "2" "1" "0" "4"
else instance AddSingle2 "2" "2" "1" "0" "5"
else instance AddSingle2 "3" "2" "1" "0" "6"
else instance AddSingle2 "4" "2" "1" "0" "7"
else instance AddSingle2 "5" "2" "1" "0" "8"
else instance AddSingle2 "6" "2" "1" "0" "9"
else instance AddSingle2 "7" "2" "1" "1" "0"
else instance AddSingle2 "8" "2" "1" "1" "1"
else instance AddSingle2 "9" "2" "1" "1" "2"
else instance AddSingle2 "0" "3" "1" "0" "4"
else instance AddSingle2 "1" "3" "1" "0" "5"
else instance AddSingle2 "2" "3" "1" "0" "6"
else instance AddSingle2 "3" "3" "1" "0" "7"
else instance AddSingle2 "4" "3" "1" "0" "8"
else instance AddSingle2 "5" "3" "1" "0" "9"
else instance AddSingle2 "6" "3" "1" "1" "0"
else instance AddSingle2 "7" "3" "1" "1" "1"
else instance AddSingle2 "8" "3" "1" "1" "2"
else instance AddSingle2 "9" "3" "1" "1" "3"
else instance AddSingle2 "0" "4" "1" "0" "5"
else instance AddSingle2 "1" "4" "1" "0" "6"
else instance AddSingle2 "2" "4" "1" "0" "7"
else instance AddSingle2 "3" "4" "1" "0" "8"
else instance AddSingle2 "4" "4" "1" "0" "9"
else instance AddSingle2 "5" "4" "1" "1" "0"
else instance AddSingle2 "6" "4" "1" "1" "1"
else instance AddSingle2 "7" "4" "1" "1" "2"
else instance AddSingle2 "8" "4" "1" "1" "3"
else instance AddSingle2 "9" "4" "1" "1" "4"
else instance AddSingle2 "0" "5" "1" "0" "6"
else instance AddSingle2 "1" "5" "1" "0" "7"
else instance AddSingle2 "2" "5" "1" "0" "8"
else instance AddSingle2 "3" "5" "1" "0" "9"
else instance AddSingle2 "4" "5" "1" "1" "0"
else instance AddSingle2 "5" "5" "1" "1" "1"
else instance AddSingle2 "6" "5" "1" "1" "2"
else instance AddSingle2 "7" "5" "1" "1" "3"
else instance AddSingle2 "8" "5" "1" "1" "4"
else instance AddSingle2 "9" "5" "1" "1" "5"
else instance AddSingle2 "0" "6" "1" "0" "7"
else instance AddSingle2 "1" "6" "1" "0" "8"
else instance AddSingle2 "2" "6" "1" "0" "9"
else instance AddSingle2 "3" "6" "1" "1" "0"
else instance AddSingle2 "4" "6" "1" "1" "1"
else instance AddSingle2 "5" "6" "1" "1" "2"
else instance AddSingle2 "6" "6" "1" "1" "3"
else instance AddSingle2 "7" "6" "1" "1" "4"
else instance AddSingle2 "8" "6" "1" "1" "5"
else instance AddSingle2 "9" "6" "1" "1" "6"
else instance AddSingle2 "0" "7" "1" "0" "8"
else instance AddSingle2 "1" "7" "1" "0" "9"
else instance AddSingle2 "2" "7" "1" "1" "0"
else instance AddSingle2 "3" "7" "1" "1" "1"
else instance AddSingle2 "4" "7" "1" "1" "2"
else instance AddSingle2 "5" "7" "1" "1" "3"
else instance AddSingle2 "6" "7" "1" "1" "4"
else instance AddSingle2 "7" "7" "1" "1" "5"
else instance AddSingle2 "8" "7" "1" "1" "6"
else instance AddSingle2 "9" "7" "1" "1" "7"
else instance AddSingle2 "0" "8" "1" "0" "9"
else instance AddSingle2 "1" "8" "1" "1" "0"
else instance AddSingle2 "2" "8" "1" "1" "1"
else instance AddSingle2 "3" "8" "1" "1" "2"
else instance AddSingle2 "4" "8" "1" "1" "3"
else instance AddSingle2 "5" "8" "1" "1" "4"
else instance AddSingle2 "6" "8" "1" "1" "5"
else instance AddSingle2 "7" "8" "1" "1" "6"
else instance AddSingle2 "8" "8" "1" "1" "7"
else instance AddSingle2 "9" "8" "1" "1" "8"
else instance AddSingle2 "0" "9" "1" "1" "0"
else instance AddSingle2 "1" "9" "1" "1" "1"
else instance AddSingle2 "2" "9" "1" "1" "2"
else instance AddSingle2 "3" "9" "1" "1" "3"
else instance AddSingle2 "4" "9" "1" "1" "4"
else instance AddSingle2 "5" "9" "1" "1" "5"
else instance AddSingle2 "6" "9" "1" "1" "6"
else instance AddSingle2 "7" "9" "1" "1" "7"
else instance AddSingle2 "8" "9" "1" "1" "8"
else instance AddSingle2 "9" "9" "1" "1" "9"

class AddIntermediate (headCarry ∷ Symbol) (headSum ∷ Symbol) (tailCarry ∷ Symbol) (carry :: Symbol)(sum ∷ Symbol) 
  -- | sum tailCarry → headCarry headSum
  -- , headSum tailCarry -> headCarry sum

     | headCarry headSum tailCarry -> carry sum
    , headCarry headSum carry sum -> tailCarry 
    , tailCarry carry sum -> headCarry headSum 
instance AddIntermediate "0" "0" "0" "0" "0"
else instance AddIntermediate "0" "1" "0" "0" "1"
else instance AddIntermediate "0" "2" "0" "0" "2"
else instance AddIntermediate "0" "3" "0" "0" "3"
else instance AddIntermediate "0" "4" "0" "0" "4"
else instance AddIntermediate "0" "5" "0" "0" "5"
else instance AddIntermediate "0" "6" "0" "0" "6"
else instance AddIntermediate "0" "7" "0" "0" "7"
else instance AddIntermediate "0" "8" "0" "0" "8"
else instance AddIntermediate "0" "9" "0" "0" "9"
else instance AddIntermediate "1" "0" "0" "1" "0"
else instance AddIntermediate "1" "1" "0" "1" "1"
else instance AddIntermediate "1" "2" "0" "1" "2"
else instance AddIntermediate "1" "3" "0" "1" "3"
else instance AddIntermediate "1" "4" "0" "1" "4"
else instance AddIntermediate "1" "5" "0" "1" "5"
else instance AddIntermediate "1" "6" "0" "1" "6"
else instance AddIntermediate "1" "7" "0" "1" "7"
else instance AddIntermediate "1" "8" "0" "1" "8"

else instance AddIntermediate "0" "0" "1" "0" "1"
else instance AddIntermediate "0" "1" "1" "0" "2"
else instance AddIntermediate "0" "2" "1" "0" "3"
else instance AddIntermediate "0" "3" "1" "0" "4"
else instance AddIntermediate "0" "4" "1" "0" "5"
else instance AddIntermediate "0" "5" "1" "0" "6"
else instance AddIntermediate "0" "6" "1" "0" "7"
else instance AddIntermediate "0" "7" "1" "0" "8"
else instance AddIntermediate "0" "8" "1" "0" "9"
else instance AddIntermediate "0" "9" "1" "1" "0"
else instance AddIntermediate "1" "0" "1" "1" "1"
else instance AddIntermediate "1" "1" "1" "1" "2"
else instance AddIntermediate "1" "2" "1" "1" "3"
else instance AddIntermediate "1" "3" "1" "1" "4"
else instance AddIntermediate "1" "4" "1" "1" "5"
else instance AddIntermediate "1" "5" "1" "1" "6"
else instance AddIntermediate "1" "6" "1" "1" "7"
else instance AddIntermediate "1" "7" "1" "1" "8"
else instance AddIntermediate "1" "8" "1" "1" "9"

class AddHelper1 (augendHead ∷ Symbol) (augendTail ∷ Symbol) (addendHead ∷ Symbol) (addendTail ∷ Symbol) (carry ∷ Symbol) (sum ∷ Symbol) | augendHead augendTail addendHead addendTail → carry sum

instance
  ( AddSingle augendHead addendHead carry sum
  ) ⇒
  AddHelper1 augendHead "" addendHead "" carry sum
else instance
  ( AddSingle augendTail addendTail tailCarry sum
  , AddSingle augendHead tailCarry x carry
  ) ⇒
  AddHelper1 augendHead augendTail "0" addendTail carry sum

class AddHelper (augendHead ∷ Symbol) (augendTail ∷ Symbol) (addendHead ∷ Symbol) (addendTail ∷ Symbol) (carry ∷ Symbol) (sum ∷ Symbol) | augendHead augendTail addendHead addendTail → carry sum

instance (AddSingle augendHead addendHead carry sum) ⇒ AddHelper augendHead "" addendHead "" carry sum
else instance
  ( AddSingle augendHead addendHead headCarry headSum
  , AddHelper1 headCarry headSum "0" tailCarry carry intermediateSum
  , Cons augendTailHead augendTailTail augendTail
  , Cons addendTailHead addendTailTail addendTail
  , AddHelper augendTailHead augendTailTail addendTailHead addendTailTail tailCarry tailSum
  , Cons intermediateSum tailSum sum
  ) ⇒
  AddHelper augendHead augendTail addendHead addendTail carry sum

class Add (augend ∷ Symbol) (addend ∷ Symbol) (carry ∷ Symbol) (sum ∷ Symbol) | augend addend → carry sum
instance
  ( Cons augendHead augendTail augend
  , Cons addendHead addendTail addend
  , AddHelper augendHead augendTail addendHead addendTail carry sum
  ) ⇒
  Add augend addend carry sum

term ∷ ∀ sym. Proxy sym
term = Proxy

class Add2 (augend ∷ Symbol) (addend ∷ Symbol) (carryPrevious ∷ Symbol) (carryNext ∷ Symbol) (sum ∷ Symbol) | augend carryPrevious sum → addend carryNext

-- instance
--   ( AddSingle augend addend carryNext sum
--   ) ⇒
--   Add2 augend addend "0" carryNext sum
-- else 

-- instance
--   ( AddSingle augend addend carryThru sumThru
--   , AddIntermediate carryNext sumThru "1" sum
--   ) ⇒
--   Add2 augend addend "1" carryNext sum

instance
  ( --AddSingle augend addend carryThru sumThru
  --, AddIntermediate carryThru sumThru carryPrevious carryNext sum
  AddSingle2 augend addend carryPrevious carryNext sum 
  ) ⇒
  Add2 augend addend carryPrevious carryNext sum


class AddX (augend ∷ Symbol) (addend ∷ Symbol) (carry ∷ Symbol) (sum ∷ Symbol) | augend addend → carry sum
 , augend sum → addend carry 
instance AddX "" "" "0" ""
else instance
  ( Cons augendHead augendTail augend
  , Cons addendHead addendTail addend
  , Cons sumHead sumTail sum
  , AddX augendTail addendTail carryPrevious sumTail 
  , AddSingle2 augendHead addendHead carryPrevious carry sumHead
  ) ⇒
  AddX augend addend carry sum

test ∷ ∀ augend addend carry carryPrevious sum. Add2 augend addend carryPrevious carry sum ⇒ Proxy carryPrevious -> Proxy augend → Proxy sum → Proxy addend
test _ _ _ = Proxy

test2 ∷ ∀ augend addend carry carryPrevious sum. Add2 augend addend carryPrevious carry sum ⇒ Proxy carryPrevious -> Proxy augend → Proxy addend → Proxy sum
test2 _ _ _ = Proxy


test3 ∷ ∀ augend addend carry sum. AddX augend addend carry sum ⇒ Proxy augend → Proxy addend → Proxy sum
test3 _ _ = Proxy

test4 ∷ ∀ augend addend carry sum. AddX augend addend carry sum ⇒ Proxy augend → Proxy sum → Proxy addend
test4 _ _ = Proxy

test5 ∷ ∀ augend addend carry sum result. Cons carry sum result => AddX augend addend carry sum ⇒ Proxy augend → Proxy addend → Proxy result
test5 _ _ = Proxy

test6 ∷ ∀ augend addend carry sum result. Cons carry sum result => AddX augend addend carry sum ⇒ Proxy augend → Proxy result → Proxy addend
test6 _ _ = Proxy

test6b ∷ ∀ augend addend carry sum. AddX augend addend carry sum ⇒ Proxy augend → Proxy carry -> Proxy sum → Proxy addend
test6b _ _ _ = Proxy

--x = test (term :: _ "1") (term ∷ _ "5") (term ∷ _ "2")


y :: Proxy "1"
y = test (term :: _ "0") (term ∷ _ "5") (term ∷ _ "6")

z :: Proxy "3"
z = test3 (term :: _ "1") (term :: _ "2")


zz :: Proxy "33"
zz = test3 (term :: Proxy "11") (term :: Proxy "22")

a :: Proxy "1"
a = test4 (term :: Proxy "1") (term :: Proxy "2")

aa :: Proxy "11"
aa = test4 (term :: Proxy "11") (term :: Proxy "22")

bb :: Proxy "176"
bb = test5 (term :: Proxy "99") (term :: Proxy "77")

cc :: Proxy "77"
cc = test6 (term :: Proxy "99") (term :: Proxy "176")

ccc :: Proxy "77"
ccc = test6b (term :: Proxy "99") (term :: Proxy "1") (term :: Proxy "76")
