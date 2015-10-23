module STLC

import Control.Monad.Identity
import Control.Monad.Writer

%access public

infixr 9 ===>

data Ty = NUM | (===>) Ty Ty
%name Ty t,t',t''

instance Show Ty where
  show NUM = "Num"
  show (t ===> t') = "(" ++ show t ++ " ⟶ " ++ show t' ++ ")"


arrInj : (t ===> t' = u ===> u') -> (t = u, t' = u')
arrInj Refl = (Refl, Refl)

instance DecEq Ty where
  decEq NUM         NUM         = Yes Refl
  decEq NUM         (t ===> t') = No (\(Refl) impossible)
  decEq (t ===> t') NUM         = No (\(Refl) impossible)
  decEq (t ===> t') (u ===> u') with (decEq t u)
    decEq (t ===> t') (t ===> u') | Yes Refl with (decEq t' u')
      decEq (t ===> t') (t ===> t') | Yes Refl | Yes Refl = Yes Refl
      decEq (t ===> t') (t ===> u') | Yes Refl | No contra = No $ contra . snd . arrInj
    decEq (t ===> t') (u ===> u') | No contra = No $ contra . fst . arrInj

data Ix : List Ty -> Ty -> Type where
  Z : Ix (t::ctxt) t
  S : Ix ctxt t -> Ix (t'::ctxt) t

%name Ix i,j

||| `STLC.S` is injective
IxSInj : STLC.S i = STLC.S j -> i = j
IxSInj Refl = Refl

instance DecEq (Ix ctxt t) where
  decEq Z Z = Yes Refl
  decEq Z (S i) = No $ \(Refl) impossible
  decEq (S i) Z = No $ \(Refl) impossible
  decEq (S i) (S j) with (decEq i j)
    decEq (S i) (S i) | (Yes Refl) = Yes Refl
    decEq (S i) (S j) | (No contra) = No $ contra . IxSInj

asNat : Ix ctxt t -> Nat
asNat Z = Z
asNat (S i) = S (asNat i)

instance Show (Ix ctxt t) where
  show = show . asNat

||| Typing derivations
data Tm : List Ty -> Ty -> Type where
  Lam : Tm (t :: ctxt) t' -> Tm ctxt (t ===> t')
  App : Tm ctxt (t ===> t') -> Tm ctxt t -> Tm ctxt t'
  Var : Ix ctxt t -> Tm ctxt t
  CstN : Integer -> Tm ctxt NUM
  Succ : Tm ctxt (NUM ===> NUM)

%name Tm tm,tm'

lamInj : with STLC Lam x = Lam y -> x = y
lamInj Refl = Refl

appInj : with STLC App {t=t1} f x = App {t=t2} g y -> (t1 = t2, (f = g, x = y))
appInj Refl = (Refl, Refl, Refl)

varInj : with STLC Var i = Var j -> i = j
varInj Refl = Refl

cstNInj : CstN n = CstN m -> n = m
cstNInj Refl = Refl

instance DecEq (Tm ctxt t) where
  decEq (Lam tm) (Lam tm') with (decEq tm tm')
    decEq (Lam tm) (Lam tm)  | Yes Refl = Yes Refl
    decEq (Lam tm) (Lam tm') | No contra = No $ contra . lamInj
  decEq (Lam tm) (App tm' x) = No $ \(Refl) impossible
  decEq (Lam tm) (Var i) = No $ \(Refl) impossible
  decEq (Lam tm) Succ = No $ \(Refl) impossible
  decEq (App tm tm') (Lam x) = No $ \(Refl) impossible
  decEq (App {t=t1} f x) (App {t=t2} f' x') with (decEq t1 t2)
    decEq (App f x) (App f' x') | Yes Refl with (decEq f f')
      decEq (App f x) (App f x')  | Yes Refl | Yes Refl with (decEq x x')
        decEq (App f x) (App f x)  | Yes Refl | Yes Refl | Yes Refl = Yes Refl
        decEq (App f x) (App f x') | Yes Refl | Yes Refl | No contra =
            No $ contra . snd . snd . appInj
      decEq (App f x) (App f' x') | Yes Refl | No contra =
          No $ contra . fst . snd . appInj
    decEq (App f x) (App f' x') | No contra =
        No $ contra . fst . appInj
  decEq (App tm tm') (Var i) = No $ \(Refl) impossible
  decEq (App tm tm') (CstN x) = No $ \(Refl) impossible
  decEq (App tm tm') Succ = No $ \(Refl) impossible
  decEq (Var i) (Lam tm) = No $ \(Refl) impossible
  decEq (Var i) (App tm tm') = No $ \(Refl) impossible
  decEq (Var i) (Var j) with (decEq i j)
    decEq (Var i) (Var i) | Yes Refl  = Yes Refl
    decEq (Var i) (Var j) | No contra = No $ contra . varInj
  decEq (Var i) (CstN x) = No $ \(Refl) impossible
  decEq (Var i) Succ = No $ \(Refl) impossible
  decEq (CstN x) (App tm tm') = No $ \(Refl) impossible
  decEq (CstN x) (Var i) = No $ \(Refl) impossible
  decEq (CstN x) (CstN y) with (decEq x y)
    decEq (CstN x) (CstN x) | Yes Refl  = Yes Refl
    decEq (CstN x) (CstN y) | No contra = No $ contra . cstNInj
  decEq Succ (Lam tm) = No $ \(Refl) impossible
  decEq Succ (App tm tm') = No $ \(Refl) impossible
  decEq Succ (Var i) = No $ \(Refl) impossible
  decEq Succ Succ = Yes Refl

instance Show (Tm ctxt t) where
  show (Lam tm) = "λ(" ++ show tm ++ ")"
  show (App tm tm') = "(" ++ show tm ++ " " ++ show tm' ++ ")"
  show (Var i) = "v" ++ show i
  show (CstN n) = show n
  show Succ = "Succ"

mkLam : TTName -> Tm (t::ctxt) t' -> Tm ctxt (t ===> t')
mkLam _ body = Lam body

dsl tm
  index_first = STLC.Z
  index_next  = STLC.S
  variable = STLC.Var
  lambda = mkLam

(<*>) : Tm ctxt (t ===> t') -> Tm ctxt t -> Tm ctxt t'
(<*>) = App

pure : Tm ctxt t -> Tm ctxt t
pure = id

closed : Tm [] t -> Tm [] t
closed x = x

namespace Output

  ixToNat : Ix ctxt t -> Nat
  ixToNat Z = Z
  ixToNat (S i) = S (ixToNat i)

  data Untyped = App Untyped Untyped
               | Lam Untyped
               | Var Nat
               | Number Integer
               | Succ

  instance Show Untyped where
    show (Lam tm) = "(lambda " ++ show tm ++ ")"
    show (App tm tm') = "(" ++ show tm ++ " " ++ show tm' ++ ")"
    show (Var i) = "v" ++ show i
    show (Number i) = show i
    show Succ = "Succ"


  extract : Tm ctxt t -> Untyped
  extract (Lam tm) = Lam $ extract tm
  extract (App tm tm') = App (extract tm) (extract tm')
  extract (Var i) = Var (ixToNat i)
  extract Succ = Succ
  extract (CstN n) = Number n

