module FastVect.Add where


import Prim.Symbol (class Cons)


class AddSingle (augend :: Symbol) (addend :: Symbol) (carry :: Symbol) (sum :: Symbol) | augend addend -> carry sum 


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

class AddHelper1 (augendHead :: Symbol) (augendTail :: Symbol) (addendHead :: Symbol) (addendTail :: Symbol) (carry :: Symbol) (sum :: Symbol) | augendHead augendTail addendHead addendTail -> carry sum 

instance (
    AddSingle augendHead addendHead carry sum 
) => AddHelper1 augendHead "" addendHead "" carry sum 
else instance (
    AddSingle augendTail addendTail tailCarry sum 
, AddSingle augendHead tailCarry x carry 
) => AddHelper1 augendHead augendTail "0" addendTail carry sum 

class AddHelper (augendHead :: Symbol) (augendTail :: Symbol) (addendHead :: Symbol) (addendTail :: Symbol) (carry :: Symbol) (sum :: Symbol) | augendHead augendTail addendHead addendTail -> carry sum 

instance (AddSingle augendHead addendHead carry sum) => AddHelper augendHead "" addendHead "" carry sum
else instance (AddSingle augendHead addendHead headCarry headSum
, AddHelper1 headCarry headSum "0" tailCarry carry intermediateSum 
, Cons augendTailHead augendTailTail augendTail 
, Cons addendTailHead addendTailTail addendTail 
, AddHelper augendTailHead augendTailTail addendTailHead addendTailTail tailCarry tailSum
, Cons intermediateSum tailSum sum 
) => 
    AddHelper augendHead augendTail addendHead addendTail carry sum 


class Add (augend :: Symbol) (addend :: Symbol) (carry :: Symbol) (sum :: Symbol) | augend addend -> carry sum 
instance (Cons augendHead augendTail augend, 
    Cons addendHead addendTail addend, 
    AddHelper augendHead augendTail addendHead addendTail carry sum) => Add augend addend carry sum 
