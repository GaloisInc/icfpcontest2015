module Language.SizedLambda where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.Char
import Data.List
import Prelude hiding (succ)

-- program construction and observation {{{
data Program
	= App Integer Program Program
	| Lambda Integer Program
	| Var Integer
	| Number Integer
	| Succ
	deriving (Eq, Ord, Read, Show)

type EvaluationContext = [Program]

app l r = App (size l + size r + 1) l r
lambda p = Lambda (size p + 1) p
var n = Var n
num n = Number n
succ = Succ

class Sized a where size :: a -> Integer

instance Sized Program where
	size (Var n) = 1
	size (Lambda s p) = s
	size (App s p p') = s
	size (Number n) = 1
	size (Succ) = 1

instance Sized a => Sized [a] where
	size xs = sum (map size xs) + genericLength xs

instance (Sized a, Sized b) => Sized (a, b) where
	size (a, b) = size a + size b + 1
-- }}}
-- parsers {{{
type Parser = StateT String []
parse = runStateT

token s = StateT $ \s' -> case stripPrefix s s' of
	Nothing -> []
	Just s'' -> [((), s'')]

whitespace = modify (dropWhile isSpace)
embedRead = StateT reads
rParens p = token "(" *> p <* token ")"

rProgram = whitespace *> (rApp <|> rLambda <|> rProgramAtom) <* whitespace
rApp = do
	l <- rProgramAtom
	r <- rProgram
	return (app l r)
rProgramAtom = rVar <|> rSucc <|> rLit <|> rParens rProgram
rSucc = succ <$ token "succ"
rLit = num <$> embedRead
rVar = var <$> (token "v" *> embedRead)
rLambda = do
	token "lambda"
	lambda <$> rProgram
-- }}}
-- pretty-printer {{{
ppProgram (App _ e e') = ppProgram e ++ " (" ++ ppProgram e' ++ ")"
ppProgram (Lambda _ e) = "lambda " ++ ppProgram e
ppProgram (Var n) = "v" ++ show n
ppProgram (Number n) = show n
ppProgram (Succ) = "succ"
-- }}}
-- evaluation of programs {{{
eval :: EvaluationContext -> Program -> Writer [(EvaluationContext, Program)] Program
eval ctxt p = do
	tell [(ctxt,p)]
	case p of
		App _ f x -> do
			f' <- eval (x:ctxt) f
			case f' of
				Lambda _ body -> eval ctxt (subst x 0 body)
				Succ -> applySucc <$> eval ctxt x
				_ -> return (app f' x)
		_ -> return p

flatten :: EvaluationContext -> Program -> Program
flatten [] p = p
flatten (c:cs) p = app p c

subst e v (Var v') = if v == v' then e else var v'
subst e v (Lambda _ e') = lambda (subst e (v+1) e')
subst e v (App _ e' e'') = app (subst e v e') (subst e v e'')
subst e v p = p

applySucc (Number n) = Number (n+1)
applySucc p = app Succ p

normal (Lambda _ _) = True
normal (Var _) = True
normal (Number _) = True
normal _ = False
-- }}}

run :: String -> Integer -> (Program, Integer)
run s n = case parse rProgram s of
	(p, ""):_ -> let (result, steps) = runWriter (eval [] p) in
		case genericSplitAt (n-1) steps of
			(execution, [_]) -> (result, 0)
			(execution, [ ]) -> (result, n - genericLength execution)
			(execution, (ctxt, p):_) -> (flatten ctxt p, 0)

runIO = do
	s <- getLine
	n <- readLn
	case run s n of
		(v, 0) | normal v  -> putStrLn $ "accepted; result: " ++ show v
		       | otherwise -> putStrLn $ "still running after " ++ show n ++ " steps"
		(v, n') -> putStrLn $ "finished too early! " ++ show n' ++ " steps remain"

traceIO p = case runWriter (eval [] p) of
	(result, trace) -> print (map size trace) >> putStrLn (ppProgram result)
