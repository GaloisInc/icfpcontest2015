module CBN where

import Control.Monad(mplus)
import Data.List(unfoldr)
import Text.Show.Pretty(ppShow)

data Expr = Abs Expr | Var Integer | App Expr Expr | Int !Integer | Prim Prim
            deriving Show

data Prim = P String (Integer -> Maybe Expr)

instance Show Prim where
  showsPrec p (P x _) = showsPrec p x


step :: Expr -> Maybe Expr
step expr =
  case expr of
    App f e -> ((`App` e) `fmap` step f) `mplus` fun
      where fun = case f of
                   Abs b  -> Just (subst b e)
                   Prim (P _ p) ->
                     case step e of
                       Just e' -> Just (App f e')
                       Nothing ->
                         case e of
                            Int n -> p n
                            _     -> Nothing
                   _     -> Nothing
    _       -> Nothing

subst :: Expr -> Expr -> Expr
subst f v = go 0 f v
  where
  go n expr arg =
    case expr of
      App e1 e2 -> App (go n e1 arg) (go n e2 arg)
      Prim p    -> Prim p
      Int i     -> Int i
      Abs e     -> Abs (go (n+1) e (lift 0 arg))
      Var x     -> case compare x n of
                     LT -> Var x
                     EQ -> arg
                     GT -> Var (x-1)

  lift n expr =
    case expr of
      App e1 e2 -> App (lift n e1) (lift n e2)
      Prim p    -> Prim p
      Int i     -> Int i
      Abs e     -> Abs (lift (n+1) e)
      Var x | x < n     -> Var x
            | otherwise -> Var (x+1)


size :: Expr -> Integer
size expr =
  case expr of
    Abs e     -> 1 + size e
    Var _     -> 1
    App e1 e2 -> size e1 + size e2
    Prim _    -> 1
    Int _     -> 1

steps :: Expr -> [Expr]
steps e = unfoldr (fmap copy . step) e
  where copy x = (x,x)

-- Testing ---------------------------------------------------------------------

test :: Expr -> IO ()
test e = putStrLn (ppShow (steps e))

pBin x f = Prim $ P x $ \i ->
           Just $ Prim $ P (x ++ " " ++ show i) $ \j -> Just $ Int $ f i j

pAdd  = pBin "add" (+)
pSub  = pBin "sub" (flip subtract)
pEq   = pBin "eq" $ \i j -> if i == j then 1 else 0

pIf   = Prim $ P "if" $ \i -> Just $ if i /= 0 then Abs $ Abs $ Var 1
                                               else Abs $ Abs $ Var 0
pSucc = pAdd `App` exOne


exId    = Abs (Var 0)
exZero  = Int 0
exOne   = Int 1
exTwise = Abs $ Abs $ Var 1 `App` (Var 1 `App` Var 0)
exSelf  = Abs $ Var 0 `App` Var 0
exOm    = exSelf `App` exSelf
exFix   = Abs $ (Abs $ Var 1 `App` (Var 0 `App` Var 0))
          `App` (Abs $ Var 1 `App` (Var 0 `App` Var 0))

exT1    = exId `App` exOne
exT2    = exTwise `App` pSucc `App` exZero
exT3    = (Abs $ Abs $ Var 1) `App` Var 0
exT4    = pAdd `App` Int 7 `App` Int 5
exT5    = pIf `App` exZero `App` Int 5 `App` Int 7
exT6    = pIf `App` exOne  `App` Int 5 `App` Int 7
exT7    = App exFix exId
exT8    = App exFix $ Abs $ Abs $ Abs $
          pIf `App` (Var 1)
              `App` (Var 2 `App` (pSub `App` Var 1 `App` exOne) `App`
                                      (pAdd `App` Var 0 `App` exOne))
              `App` (Var 0)
exT9    = exT8 `App` Int 10 `App` Int 8


