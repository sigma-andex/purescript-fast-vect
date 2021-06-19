module Data.FastVect.Add where


import Prim.Symbol (class Cons)
import Type.Prelude (Proxy(..))


class AddSingle (augend :: Symbol) (addend :: Symbol) (carry :: Symbol) (sum :: Symbol) 
    | augend addend -> carry sum 
    , augend carry -> sum addend
    , addend carry -> sum augend
-- AddSingle "4" t88 "0" t100 => Proxy "4"


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

class AddIntermediate (headCarry :: Symbol) (headSum :: Symbol) (tailCarry :: Symbol) (carry :: Symbol) (sum :: Symbol)
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

-- class AddHelper1 (augendHead :: Symbol) (augendTail :: Symbol) (addendHead :: Symbol) (addendTail :: Symbol) (carry :: Symbol) (sum :: Symbol) | augendHead augendTail addendHead addendTail -> carry sum 

-- instance (
--     AddSingle augendHead addendHead carry sum 
-- ) => AddHelper1 augendHead "" addendHead "" carry sum 
-- else instance (
--     AddSingle augendTail addendTail tailCarry sum 
-- , AddSingle augendHead tailCarry x carry 
-- ) => AddHelper1 augendHead augendTail "0" addendTail carry sum 

class AddHelper (augendHead :: Symbol) (augendTail :: Symbol) (addendHead :: Symbol) (addendTail :: Symbol) (carryHead :: Symbol) (carryTail :: Symbol) (sumHead :: Symbol) (sumTail :: Symbol) 
    | augendHead augendTail addendHead addendTail carryTail -> carryHead sumHead sumTail
    , augendHead augendTail carryHead sumHead sumTail -> addendHead addendTail carryTail
    , addendHead addendTail carryHead sumHead sumTail -> augendHead augendTail carryTail

instance (AddSingle augendHead addendHead carry sum) => AddHelper augendHead "" addendHead "" carry "" sumHead ""
{-

    t98 could not be determined
    t120 could not be determined
    t121 could not be determined
    t128 could not be determined
    t135 could not be determined
    t136 could not be determined
    t139 could not be determined

    forall (t98 :: Symbol) (t120 :: Symbol) (t121 :: Symbol) (t128 :: Symbol) (t135 :: Symbol) (t136 :: Symbol) (t139 :: Symbol). Cons t120 t121 t98 
    => AddSingle "1" t120 t135 t136 
    => AddIntermediate t135 t136 t139 "0" "1" 
    => Cons t128 "" t121
    => AddSingle "0" t128 t139 "1" 
    => Proxy @Symbol "10"
-}
--else instance (AddSingle augendHead addendHead carry sum) => AddHelper augendHead "" addendHead addendTail carry sum 
else instance (AddSingle augendHead addendHead headCarry headSum
--, AddIntermediate headCarry headSum "0" carry sumHead 
, Cons augendTailHead augendTailTail augendTail 
, Cons addendTailHead addendTailTail addendTail 
, Cons sumTailHead sumTailTail tailSum 
, AddHelper augendTailHead augendTailTail addendTailHead addendTailTail headCarry "0" sumTailHead sumTailTail
) => 
    AddHelper augendHead augendTail addendHead addendTail headCarry "0" headSum  tailSum


class Add (augend :: Symbol) (addend :: Symbol) (carry :: Symbol) (sum :: Symbol) | augend addend -> carry sum 
instance (Cons augendHead augendTail augend, 
    Cons addendHead addendTail addend, 
    Cons sumHead sumTail sum,
    AddHelper augendHead augendTail addendHead addendTail carryHead carryTail sumHead sumTail) => Add augend addend carryHead sum 


term :: forall sym. Proxy sym
term = Proxy 

sum :: forall augend addend carry sum result. 
    Add augend addend carry sum => Cons carry sum result => Proxy augend -> Proxy addend -> Proxy result 
sum _ _ = Proxy 

take :: forall augend addend carry sum result. 
    Add augend addend carry sum =>
    Cons carry sum result => 
    Proxy augend -> Proxy result -> Proxy augend
take _ _ = Proxy 


s :: Proxy "18"
s = sum (term :: _ "9") (term :: _ "9")

s2 :: Proxy "019"
s2 = sum (term :: _ "10") (term :: _ "09")


t :: Proxy "04363"
t = sum (term :: _ "4220") (term :: _ "0143")

u :: Proxy "040787"
u = sum (term :: _ "17220") (term :: _ "23567")

v :: Proxy "077868"
v = sum (term :: _ "23423") (term :: _ "54445")

--x :: Proxy "03543525"
x :: Proxy "03543525"
x = sum (term :: _ "1231293") (term :: _ "2312232")

--z :: Proxy "4"
z = take (term :: _ "4") (term :: _ "04")

headSym :: forall h t v. Cons h t v => Proxy v -> Proxy h
headSym _ = Proxy 

tailSym :: forall h t v. Cons h t v => Proxy v -> Proxy t
tailSym _ = Proxy 

z2 :: Proxy "0"
z2 = headSym (term :: _ "04")

z3 :: Proxy "4"
z3 = tailSym (term :: _ "04")

--z1 :: Proxy "10"
z1 = take (term :: _ "10") (term :: _ "011")

-- z4 :: Proxy "9"
-- z4 = take (term :: _ "9") (term :: _ "10")

-- z5 :: Proxy "11"
-- z5 = take (term :: _ "11") (term :: _ "011")

-- z6 :: Proxy "011"
-- z6 = take (term :: _ "12") (term :: _ "011")
