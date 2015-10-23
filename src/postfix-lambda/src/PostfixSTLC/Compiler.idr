||| Example of compilation from STLC to Postlambda code.
|||
||| To use these examples, open a REPL. To run compiled code, do something like:
||| ```
||| λΠ> runFor 100 $ run $ MkState (church [] NUM 2) [Ctxt []] []
||| ```
||| The initial stack value is to satisfy a precondition of the compiler.
|||
||| To get it in a form suitable for batch execution, one must push an
||| empty starting context and then drop it at the end. Try something like:
||| ```
||| λΠ> concat $ intersperse " " $ map show $ [EmptyCtxt] ++ church [] NUM 5 ++ [Drop]
||| ```
module PostfixSTLC.Compiler

import PostfixSTLC
import STLC

%access private

deBruijn : Ix ctxt ty -> (k : Nat ** LT k (length ctxt))
deBruijn Z     = (Z ** LTESucc LTEZero)
deBruijn (S i) with (deBruijn i)
  deBruijn (S i) | (k ** lt) = (S k ** LTESucc lt)

mkPreVar : Nat -> List Cmd
mkPreVar Z = [VarZ] -- vz, ctxt
mkPreVar (S k) = mkPreVar k ++ -- vk, ctxt
                 [VarS]        -- vsk, ctxt

compileTy : Ty -> List Cmd
compileTy NUM = [PushNum]
compileTy (t ===> t') = compileTy t ++ compileTy t' ++ [MkArr]

nodes : Ty -> Nat
nodes NUM = 1
nodes (t ===> t') = nodes t + (nodes t' + 1)

pushNat : Nat -> List Cmd
pushNat j = NatZ :: pushNat' j
  where pushNat' : Nat -> List Cmd
        pushNat' Z = []
        pushNat' (S k) = NatS :: pushNat' k

compileTm : Tm ctxt ty -> List Cmd
compileTm (Lam {t} body) =
  [Dup] ++           -- ctxt, ctxt, ...
  compileTy t ++     -- t, ctxt, ctxt
  [ExtendCtxt] ++    -- (t::ctxt), ctxt
  compileTm body ++  -- (t :: ctxt), (t::ctxt |- body : t'), ctxt
  [Drop,             -- (t::ctxt |- body : t'), ctxt
   MkLam,            -- (ctxt |- Lam body : t ===> t'), ctxt
   Swap]             -- ctxt, (t::ctxt |- body : t')
compileTm (App tm tm') =
  compileTm tm ++  -- ctxt, (ctxt |- f : t ===> t')
  compileTm tm' ++ -- ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
  [NatZ,           -- 0, ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   NatS,           -- 1, ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   NatS,           -- 2, ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   NatS,           -- 3, ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Dup,            -- 3, 3, ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Get,            -- (ctxt |- f : t ===> t'), 3, ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Swap,           -- 3, (ctxt |- f : t ===> t'), ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Dup,            -- 3, 3, (ctxt |- f : t ===> t'), ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Get,            -- (ctxt |- x : t), 3, (ctxt |- f : t ===> t'), ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Swap,           -- 3, (ctxt |- x : t), (ctxt |- f : t ===> t'), ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Drop,           -- (ctxt |- x : t), (ctxt |- f : t ===> t'), ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   MkApp,          -- (ctxt |- f x : t'), ctxt, (ctxt |- x : t), (ctxt |- f : t ===> t')
   Swap] ++        -- ctxt, (ctxt |- f x : t'), (ctxt |- x : t), (ctxt |- f : t ===> t')
  pushNat 2 ++     -- 2, ctxt, (ctxt |- f x : t'), (ctxt |- x : t), (ctxt |- f : t ===> t')
  [Quote [Drop, Drop], Apply] -- ctxt, (ctxt |- f x : t')


compileTm (Var i) =
  let (k ** lt) = deBruijn i
  in [Dup] ++      -- ctxt, ctxt
     mkPreVar k ++ -- k, ctxt, ctxt
     [MkVar,       -- (ctxt |- vk : t), ctxt
      Swap]        -- ctxt, (ctxt |- vk : t)
compileTm (CstN x) = compNat (cast x)
  where compNat : Nat -> List Cmd
        compNat Z = [Dup,    -- ctxt, ctxt
                     MkZero, -- (ctxt |- 0 : NUM), ctxt
                     Swap]   -- ctxt, (ctxt |- 0 : NUM)
        compNat (S k) = [Dup,        -- ctxt, ctxt
                         Dup,        -- ctxt, ctxt, ctxt
                         MkSucc,     -- (ctxt |- S : NUM ===> NUM), ctxt, ctxt
                         Swap] ++    -- ctxt, (ctxt |- S : NUM ===> NUM), ctxt
                        compNat k ++ -- ctxt, (ctxt |- k : NUM), (ctxt |- S : NUM ===> NUM), ctxt
                        [Drop, MkApp, Swap]
compileTm Succ = [Dup, MkSucc, Swap]


||| Compile a closed term to a stack machine program that will produce it
||| @ t the type to use (lets Idris infer the internal expression type)
||| @ tm the typing derivation to compile
public
compileProg : (t : Ty) -> (tm : Tm [] t) -> List Cmd
compileProg _ tm = [EmptyCtxt] ++ compileTm tm ++ [Drop]

||| Create the identity function at `t` in `ctxt`, assuming ctxt is on the stack top
public
mkId : (ctxt : List Ty) -> (t : Ty) -> List Cmd
mkId ctxt t = compileTm (the (Tm ctxt (t ===> t)) $ tm $ \x => x)


||| Make a Church numeral at some type
||| @ ctxt the context, assumed to be on the stack as well
||| @ t the type of zero
||| @ k the number to compile
public
church : (ctxt : List Ty) -> (t : Ty) -> (k : Nat) -> List Cmd
church ctxt t k = compileTm $ the (Tm ctxt (churchTy t)) $
                    tm $ \f, x => iter k (App f) x
  where
    churchTy : Ty -> Ty
    churchTy t = (t ===> t) ===> t ===> t

    iter : Nat -> (a -> a) -> a -> a
    iter Z     f x = x
    iter (S k) f x = f (iter k f x)
