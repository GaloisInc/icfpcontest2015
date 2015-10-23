module PostfixSTLC.Test

import PostfixSTLC
import STLC

identityNat : Tm [] (NAT ===> NAT)
identityNat = tm $ \x => x

mkId : List Cmd
mkId = [EmptyCtxt, PushNat, ExtendCtxt, PushNat, VarZ, MkVar, MkLam]

idWorks : run mkId [] = Right [Deriv _ _ identityNat]
idWorks = Refl

add : Tm [] (NAT ===> NAT ===> NAT)
add = tm $ \x,y => NatRec NAT y (\a, b => Succ b) x

mkAdd : List Cmd
mkAdd = [EmptyCtxt, PushNat, ExtendCtxt, PushNat, ExtendCtxt, Dup, PushNat, ExtendCtxt, PushNat, ExtendCtxt, PushNat, VarZ, MkVar, MkSucc, MkLam, MkLam, PushNat, Swap, NatZ, NatS, NatS, Get, Dup, PushNat, VarZ, MkVar, Swap, Dup, PushNat, VarZ, VarS, MkVar, NatZ, NatS, NatS, NatS, NatS, Get, NatZ, NatS, NatS, NatS, Get, NatZ, NatS, NatS, NatS, NatS, Get, NatZ, NatS, NatS, NatS, Get, MkNatRec, Swap, Drop, MkLam, MkLam]

mkAddOk : run mkAdd [] = Right [Deriv _ _ add]
mkAddOk = Refl
