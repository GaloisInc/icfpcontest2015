module PostfixSTLC

import Data.Vect

import STLC
import public PostfixSTLC.Commands
import PostfixSTLC.Views

%default total
%access public

data StackVal : Type where
  ||| A type
  TyVal : Ty -> StackVal
  ||| Something that might someday be a de Bruijn index
  PreVar : Nat -> StackVal
  ||| A typing context
  Ctxt : List Ty -> StackVal
  ||| A typing derivation
  Deriv : (ctxt : List Ty) -> (t : Ty) -> Tm ctxt t -> StackVal
  ||| A numeric value
  NatVal : Nat -> StackVal
  ||| A quoted program
  QuoteVal : List Cmd -> StackVal

instance Show StackVal where
  show (TyVal t) = "Type: " ++ show t
  show (PreVar k) = "Index: " ++ show k
  show (Ctxt xs) = "Ctxt: " ++ show xs
  show (Deriv ctxt t tm) = show ctxt ++ " ⊢ " ++ show tm ++ " : " ++ show t
  show (NatVal k) = show k
  show (QuoteVal xs) = "'" ++ show xs

tyValInj : TyVal t = TyVal t' -> t = t'
tyValInj Refl = Refl

preVarInj : PreVar j = PreVar k -> j = k
preVarInj Refl = Refl

ctxtInj : Ctxt xs = Ctxt ys -> xs = ys
ctxtInj Refl = Refl

derivInj : Deriv ctxt t tm = Deriv ctxt' t' tm' -> (ctxt = ctxt', (t = t', tm = tm'))
derivInj Refl = (Refl, (Refl, Refl))

natValInj : NatVal j = NatVal k -> j = k
natValInj Refl = Refl

quoteValInj : QuoteVal prog = QuoteVal prog' -> prog = prog'
quoteValInj Refl = Refl

instance DecEq StackVal where
  decEq (TyVal t) (TyVal t') with (decEq t t')
    decEq (TyVal t) (TyVal t)  | Yes Refl = Yes Refl
    decEq (TyVal t) (TyVal t') | No contra = No $ contra . tyValInj
  decEq (TyVal t) (PreVar k) = No $ \(Refl) impossible
  decEq (TyVal t) (Ctxt xs) = No $ \(Refl) impossible
  decEq (TyVal t) (Deriv ctxt t' tm) = No $ \(Refl) impossible
  decEq (TyVal t) (NatVal k) = No $ \(Refl) impossible
  decEq (TyVal t) (QuoteVal xs) = No $ \(Refl) impossible
  decEq (PreVar k) (TyVal t) = No $ \(Refl) impossible
  decEq (PreVar k) (PreVar j) with (decEq k j)
    decEq (PreVar k) (PreVar k) | Yes Refl  = Yes Refl
    decEq (PreVar k) (PreVar j) | No contra = No $ contra . preVarInj
  decEq (PreVar k) (Ctxt xs) = No $ \(Refl) impossible
  decEq (PreVar k) (Deriv ctxt t tm) = No $ \(Refl) impossible
  decEq (PreVar k) (NatVal j) = No $ \(Refl) impossible
  decEq (PreVar k) (QuoteVal xs) = No $ \(Refl) impossible
  decEq (Ctxt xs) (TyVal t) = No $ \(Refl) impossible
  decEq (Ctxt xs) (PreVar k) = No $ \(Refl) impossible
  decEq (Ctxt xs) (Ctxt ys) with (decEq xs ys)
    decEq (Ctxt xs) (Ctxt xs) | Yes Refl = Yes Refl
    decEq (Ctxt xs) (Ctxt ys) | No contra = No $ contra . ctxtInj
  decEq (Ctxt xs) (Deriv ctxt t tm) = No $ \(Refl) impossible
  decEq (Ctxt xs) (NatVal k) = No $ \(Refl) impossible
  decEq (Ctxt xs) (QuoteVal ys) = No $ \(Refl) impossible
  decEq (Deriv ctxt t tm) (TyVal t') = No $ \(Refl) impossible
  decEq (Deriv ctxt t tm) (PreVar k) = No $ \(Refl) impossible
  decEq (Deriv ctxt t tm) (Ctxt xs) = No $ \(Refl) impossible
  decEq (Deriv ctxt t tm) (Deriv ctxt' t' tm') with (decEq ctxt ctxt')
    decEq (Deriv ctxt t tm) (Deriv ctxt' t' tm') | No contra =
        No $ contra . fst . derivInj
    decEq (Deriv ctxt t tm) (Deriv ctxt t' tm')  | Yes Refl with (decEq t t')
      decEq (Deriv ctxt t tm) (Deriv ctxt t' tm') | Yes Refl | No contra =
          No $ contra . fst . snd . derivInj
      decEq (Deriv ctxt t tm) (Deriv ctxt t tm')  | Yes Refl | Yes Refl with (decEq tm tm')
        decEq (Deriv ctxt t tm) (Deriv ctxt t tm')  | Yes Refl | Yes Refl | No contra =
          No $ contra . snd . snd . derivInj
        decEq (Deriv ctxt t tm) (Deriv ctxt t tm)   | Yes Refl | Yes Refl | Yes Refl = Yes Refl
  decEq (Deriv ctxt t tm) (NatVal k) = No $ \(Refl) impossible
  decEq (Deriv ctxt t tm) (QuoteVal xs) = No $ \(Refl) impossible
  decEq (NatVal k) (TyVal t) = No $ \(Refl) impossible
  decEq (NatVal k) (PreVar j) = No $ \(Refl) impossible
  decEq (NatVal k) (Ctxt xs) = No $ \(Refl) impossible
  decEq (NatVal k) (Deriv ctxt t tm) = No $ \(Refl) impossible
  decEq (NatVal k) (NatVal j) with (decEq k j)
    decEq (NatVal k) (NatVal k) | Yes Refl  = Yes Refl
    decEq (NatVal k) (NatVal j) | No contra = No $ contra . natValInj
  decEq (NatVal k) (QuoteVal xs) = No $ \(Refl) impossible
  decEq (QuoteVal xs) (TyVal t) = No $ \(Refl) impossible
  decEq (QuoteVal xs) (PreVar k) = No $ \(Refl) impossible
  decEq (QuoteVal xs) (Ctxt ys) = No $ \(Refl) impossible
  decEq (QuoteVal xs) (Deriv ctxt t tm) = No $ \(Refl) impossible
  decEq (QuoteVal xs) (NatVal k) = No $ \(Refl) impossible
  decEq (QuoteVal xs) (QuoteVal ys) with (decEq xs ys)
    decEq (QuoteVal xs) (QuoteVal xs) | Yes Refl = Yes Refl
    decEq (QuoteVal xs) (QuoteVal ys) | No contra = No $ contra . quoteValInj

record ProgramState where
  constructor MkState
  program : List Cmd
  stack : List StackVal
  callStack : List (List Cmd, List StackVal)


decLTE : (j,k : Nat) -> Dec (LTE j k)
decLTE Z k = Yes LTEZero
decLTE (S j) Z = No absurd
decLTE (S j) (S k) with (decLTE j k)
  decLTE (S j) (S k) | Yes prf = Yes $ LTESucc prf
  decLTE (S j) (S k) | No contra = No $ \(LTESucc x) => contra x


ltLookup : (k : Nat) -> (xs : List a) -> LT k (length xs) -> a
ltLookup k [] lt = absurd lt
ltLookup Z (x :: xs) lt = x
ltLookup (S k) (x :: xs) (LTESucc lt) = ltLookup k xs lt

toIndex : (k : Nat) -> (ctxt : List Ty) -> (lt : LT k (length ctxt)) -> Ix ctxt (ltLookup k ctxt lt)
toIndex k [] lt = absurd lt
toIndex Z (x :: xs) lt = Z
toIndex (S k) (x :: xs) (LTESucc lt) = S (toIndex k xs lt)

splitAt : (n : Nat) -> (xs : List a) -> LT n (length xs) -> (List a, List a)
splitAt Z xs lt = ([], xs)
splitAt (S k) [] lt = absurd lt
splitAt (S k) (x :: xs) (LTESucc lt) =
  -- NB: it's important that this _not_ be a pattern-matching let,
  -- because we will use intermediate results at various places in
  -- types, and the auxiliary definition gets in the way.
  let split = splitAt k xs lt
  in (x :: fst split, snd split)

||| A quick sanity check for splitAt
private
splitAtPreservesLength : (n : Nat) -> (xs : List a) -> (lt : LT n (length xs)) ->
                         let split = splitAt n xs lt
                         in List.length (fst split) + List.length (snd split) = List.length xs
splitAtPreservesLength Z     xs        lt           = Refl
splitAtPreservesLength (S k) []        lt           = absurd lt
splitAtPreservesLength (S k) (x :: xs) (LTESucc lt) = cong $ splitAtPreservesLength k xs lt

infix 5 ~~>

||| The operational semantics of Postlambda are given by the following
||| small-step description.
|||
||| `st ~~> st'` means that `st` transitions to `st'`. Stuck states
||| are not to the left of any arrow. The later function `canStep`
||| _checks_ whether a state is valid by finding the appropriate
||| constructor of `~~>` to apply.
|||
||| Not proven, but actually true:
|||  * (~~>) is a function over the domain of valid states (maps valid start states to unique new states)
|||
||| @ st a valid start state
||| @ st' the result of taking one step
data (~~>) : (st, st' : ProgramState) -> Type where
  DoPushNum : MkState (PushNum::todo) stk k ~~> MkState todo (TyVal NUM :: stk) k
  DoMkArr : MkState (MkArr::todo) (TyVal t' :: TyVal t :: stk) k ~~>
            MkState todo (TyVal (t ===> t') :: stk) k

  DoVarZ : MkState (VarZ :: todo) stk k ~~>
           MkState todo (PreVar Z :: stk) k
  DoVarS : MkState (VarS :: todo) (PreVar n :: stk) k ~~>
           MkState todo (PreVar (S n) :: stk) k

  DoEmptyCtxt : MkState (EmptyCtxt :: todo) stk k ~~>
                MkState todo (Ctxt [] :: stk) k
  DoExtendCtxt : MkState (ExtendCtxt :: todo) (TyVal t :: Ctxt ctxt :: stk) k ~~>
                 MkState todo (Ctxt (t :: ctxt) :: stk) k

  DoMkVar : (ok : LT n (length ctxt)) ->
            MkState (MkVar :: todo) (PreVar n :: Ctxt ctxt :: stk) k ~~>
            MkState todo (Deriv _ _ (Var (toIndex n ctxt ok)) :: stk) k
  DoMkZero : MkState (MkZero :: todo) (Ctxt ctxt :: stk) k ~~>
             MkState todo (Deriv ctxt _ (CstN 0) :: stk) k
  DoMkSucc : MkState (MkSucc :: todo) (Ctxt ctxt :: stk) k ~~>
             MkState todo (Deriv ctxt _ Succ :: stk) k
  DoMkLam : MkState (MkLam :: todo) (Deriv (t::ctxt) t' body :: stk) k ~~>
            MkState todo (Deriv ctxt (t ===> t') (Lam body) :: stk) k
  DoMkApp : MkState (MkApp :: todo) (Deriv ctxt t x :: Deriv ctxt (t ===> t') f :: stk) k ~~>
            MkState todo (Deriv ctxt t' (App f x) :: stk) k

  DoDup : MkState (Dup :: todo) (v :: stk) k ~~> MkState todo (v :: v :: stk) k
  DoDrop : MkState (Drop :: todo) (v :: stk) k ~~> MkState todo stk k
  DoSwap : MkState (Swap :: todo) (v :: v' :: stk) k ~~> MkState todo (v' :: v :: stk) k

  DoNatZ : MkState (NatZ :: todo) stk k ~~> MkState todo (NatVal 0 :: stk) k
  DoNatS : MkState (NatS :: todo) (NatVal n :: stk) k ~~> MkState todo (NatVal (S n) :: stk) k
  DoGet : (ok : LT n (length stk)) ->
          MkState (Get :: todo) (NatVal n :: stk) k ~~>
          MkState todo (ltLookup n stk ok :: stk) k

  DoRev : (ok : LT n (length stk)) ->
          MkState (Rev :: todo) (NatVal n :: stk) k ~~>
          let splitStk = splitAt n stk ok
          in MkState todo (reverse (fst splitStk) ++ snd splitStk) k

  DoQuote : MkState (Quote prog :: todo) stk k ~~>
            MkState todo (QuoteVal prog :: stk) k

  DoApply : (ok : LT n (length stk)) ->
            MkState (Apply :: todo) (QuoteVal prog :: NatVal n :: stk) k ~~>
            let splitStk = splitAt n stk ok
            in MkState prog (snd splitStk) ((todo, fst splitStk)::k)

  DoReturn : MkState [] stk (frame::k) ~~>
             MkState (fst frame) (snd frame ++ stk) k

emptyProgramDone : (st' : ProgramState) -> MkState [] stk [] ~~> st' -> Void
emptyProgramDone st' DoPushNum impossible


-- TODO: use a more informative type than Maybe
-- Note that the part we care about in the return values is the first
-- projection of the pair; however, it can be inferred from the second so we let Idris do that.
canStep : (st : ProgramState) -> Maybe (st' : ProgramState ** st ~~> st')
canStep (MkState [] stack []) = Nothing
canStep (MkState (PushNum :: todo) stk k) = Just (_ ** DoPushNum)

canStep (MkState (MkArr :: todo) (TyVal t' :: TyVal t :: stk) k) =
  Just (_ ** DoMkArr)
canStep (MkState (MkArr :: todo) stk k) = Nothing

canStep (MkState (VarZ :: todo) stk k) =
  Just (_ ** DoVarZ)

canStep (MkState (VarS :: todo) (PreVar n :: stk) k) =
  Just (_ ** DoVarS)
canStep (MkState (VarS :: todo) stk k) = Nothing

canStep (MkState (EmptyCtxt :: todo) stk k) =
  Just (_ ** DoEmptyCtxt)

canStep (MkState (ExtendCtxt :: todo) (TyVal t :: Ctxt ctxt :: stk) k) =
  Just (_ ** DoExtendCtxt)
canStep (MkState (ExtendCtxt :: todo) stk k) = Nothing

canStep (MkState (MkVar :: todo) (PreVar n :: Ctxt ctxt :: stk) k) with (decLTE (S n) (length ctxt))
  canStep (MkState (MkVar :: todo) (PreVar n :: Ctxt ctxt :: stk) k) | Yes ok =
    Just (_ ** DoMkVar ok)
  canStep (MkState (MkVar :: todo) (PreVar n :: Ctxt ctxt :: stk) k) | No _ = Nothing
canStep (MkState (MkVar :: todo) stk k) = Nothing

canStep (MkState (MkZero :: todo) (Ctxt ctxt :: stk) k) =
  Just (_ ** DoMkZero)
canStep (MkState (MkZero :: todo) stk k) = Nothing

canStep (MkState (MkSucc :: todo) (Ctxt ctxt :: stk) k) =
  Just (_ ** DoMkSucc)
canStep (MkState (MkSucc :: todo) stk k) = Nothing

canStep (MkState (MkLam :: todo) (Deriv (t::ctxt) t' body :: stk) k) =
  Just (_ ** DoMkLam)
canStep (MkState (MkLam :: todo) stk k) = Nothing

canStep (MkState (MkApp :: todo) (Deriv ctxt t1 x :: Deriv ctxt' (t ===> t') f :: stk) k) with (decEq t1 t)
  canStep (MkState (MkApp :: todo) (Deriv ctxt t x :: Deriv ctxt' (t ===> t')  f :: stk) k) | Yes Refl with (decEq ctxt ctxt')
    canStep (MkState (MkApp :: todo) (Deriv ctxt t x :: Deriv ctxt (t ===> t')  f :: stk) k) | Yes Refl | Yes Refl = Just (_ ** DoMkApp)
    canStep (MkState (MkApp :: todo) (Deriv ctxt t x :: Deriv ctxt' (t ===> t')  f :: stk) k) | Yes Refl | No contra = Nothing
  canStep (MkState (MkApp :: todo) (Deriv ctxt t1 x :: Deriv ctxt' (t ===> t') f :: stk) k) | No contra = Nothing
canStep (MkState (MkApp :: todo) stk k) = Nothing

canStep (MkState (Dup :: todo) [] k) = Nothing
canStep (MkState (Dup :: todo) (x :: xs) k) = Just (_ ** DoDup)

canStep (MkState (Drop :: todo) [] k) = Nothing
canStep (MkState (Drop :: todo) (x :: xs) k) = Just (_ ** DoDrop)

canStep (MkState (Swap :: todo) [] k) = Nothing
canStep (MkState (Swap :: todo) [x] k) = Nothing
canStep (MkState (Swap :: todo) (x :: y :: xs) k) = Just (_ ** DoSwap)

canStep (MkState (NatZ :: todo) stk k) = Just (_ ** DoNatZ)

canStep (MkState (NatS :: todo) (NatVal n :: stk) k) = Just (_ ** DoNatS)
canStep (MkState (NatS :: todo) stk k) = Nothing

canStep (MkState (Get :: todo) (NatVal n :: stk) k) with (decLTE (S n) (length stk))
  canStep (MkState (Get :: todo) (NatVal n :: stk) k) | No _ = Nothing
  canStep (MkState (Get :: todo) (NatVal n :: stk) k) | Yes ok = Just (_ ** DoGet ok)
canStep (MkState (Get :: todo) stk k) = Nothing

canStep (MkState (Rev :: todo) (NatVal n :: stk) k) with (decLTE (S n) (length stk))
  canStep (MkState (Rev :: todo) (NatVal n :: stk) k) | No _ = Nothing
  canStep (MkState (Rev :: todo) (NatVal n :: stk) k) | Yes ok = Just (_ ** DoRev ok)
canStep (MkState (Rev :: todo) stk k) = Nothing

canStep (MkState (Quote prog :: todo) stk k) = Just (_ ** DoQuote)

canStep (MkState (Apply :: todo) (QuoteVal prog :: NatVal n :: stk) k) with (decLTE (S n) (length stk))
  canStep (MkState (Apply :: todo) (QuoteVal prog :: NatVal n :: stk) k) | No _ = Nothing
  canStep (MkState (Apply :: todo) (QuoteVal prog :: NatVal n :: stk) k) | Yes ok = Just (_ ** DoApply ok)

canStep (MkState (Apply :: todo) stk k) = Nothing

canStep (MkState [] stk ((cmds, stk') :: k)) = Just (_ ** DoReturn)

codata Trace : ProgramState -> Type where
  Done : Trace (MkState [] stk [])
  Step : st ~~> st' -> Trace st' -> Trace st
  Stuck : Trace st

viewTrace : (fuel : Nat) -> Trace st -> List ProgramState
viewTrace {st = MkState [] stk []} fuel Done = [MkState [] stk []]
viewTrace {st = st} Z        (Step _ rest) = [st]
viewTrace {st = st} (S fuel) (Step _ rest) = st :: viewTrace fuel rest
viewTrace {st = st} fuel     Stuck         = [st]

run : (st : ProgramState) -> Trace st
run (MkState [] stk []) = Done
run st = case canStep st of
           Just (st' ** why) => Step why (run st')
           Nothing => Stuck

steps : Nat -> Trace st -> List ProgramState
steps {st} Z _ = [st]
steps {st=MkState [] stk []} (S k) Done = [MkState [] stk []]
steps {st} (S k) (Step x y) = st :: steps k y
steps {st} (S k) Stuck = [st]

runFor : Nat -> Trace st -> Either ProgramState (ProgramState, Nat)
runFor {st=MkState [] stk []} n Done  = Right (MkState [] stk [], n)
runFor {st} Z     (Step rule next) = Left st
runFor {st} (S n) (Step rule next) = runFor n next
runFor {st} _     Stuck = Left st

