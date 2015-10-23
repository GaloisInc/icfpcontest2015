-- boilerplate {{{
{-# LANGUAGE FlexibleInstances #-}
module Language.Regular.Interval where

import Control.Applicative
import Control.Monad
import Language.Regular.Property
import Data.Map (Map)
import Data.Set (Set)
import Data.Universe.Instances.Eq   ()
import Data.Universe.Instances.Ord  ()
import Data.Universe.Instances.Read ()
import Data.Universe.Instances.Show ()
import Prelude hiding (null)

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
-- }}}
-- first-order logic sentences {{{
type Operator = Bool -> Bool -> Bool
data Sentence a
	= Lit Bool
	| Atom a
	| Not (Sentence a)
	| Bin (Sentence a) Operator (Sentence a)
	deriving (Eq, Ord, Show, Read)

evalSentence :: (a -> Bool) -> (Sentence a -> Bool)
evalSentence f = go where
	go (Lit  b) = b
	go (Atom a) = f a
	go (Not  s) = not (go s)
	go (Bin s (*) s') = go s * go s'
-- }}}
-- simple (<=) atoms {{{
data Bound a = LE a | GE a deriving (Eq, Ord, Read, Show)

evalBound :: Ord a => a -> Bound a -> Bool
evalBound x (LE a) = x <= a
evalBound x (GE a) = x >= a

boundaries :: Ord a => Sentence (Bound a) -> Set a
boundaries (Lit _) = S.empty
boundaries (Atom (LE x)) = S.singleton x
boundaries (Atom (GE x)) = S.singleton x
boundaries (Not s) = boundaries s
boundaries (Bin s op s') = boundaries s `S.union` boundaries s'
-- }}}
-- representatives {{{
-- The behavior of first-order sentences over a discrete domain with only (<=)
-- comparisons at their leaves is completely determined by their behavior on
-- just a handful of values. This induces a canonical form, which this
-- section's development can produce.
--
-- N.B. This technique is not suitable for continuous domains. Something
-- similar should work, but it would need a bit of thought to figure out
-- exactly what.

-- | Laws:
--
-- 1. If @below x == Just y@, then @y = sup {y' | y' < x}@.
-- 2. If @below x == Nothing@, then there is no value @y@ for which @y < x@.
-- 3. @length (below x) <= 1@
-- 4. Similarly for @above@, but with @(>)@ and @inf@ instead of @(<)@ and @sup@.
class Ord a => Representative a where
	below, above :: Alternative f => a -> f a

belowEnum x = pure (pred x)
aboveEnum x = pure (succ x)
belowBoundedEnum x = guard (x /= minBound) *> belowEnum x
aboveBoundedEnum x = guard (x /= maxBound) *> aboveEnum x

instance Representative Integer  where below = belowEnum; above = aboveEnum
instance Representative Char     where below = belowBoundedEnum; above = aboveBoundedEnum
instance Representative Int      where below = belowBoundedEnum; above = aboveBoundedEnum
instance Representative ()       where below = belowBoundedEnum; above = aboveBoundedEnum
instance Representative Bool     where below = belowBoundedEnum; above = aboveBoundedEnum
instance Representative Ordering where below = belowBoundedEnum; above = aboveBoundedEnum
instance Representative Word     where below = belowBoundedEnum; above = aboveBoundedEnum

representatives :: Representative a => Sentence (Bound a) -> Map a Bool
representatives s = M.fromList
	[ (x, evalSentence (evalBound x) s)
	| boundary  <- S.toList (boundaries s)
	, direction <- [below, above, return]
	, x <- direction boundary
	]

canonicalize_ :: Representative a => Sentence (Bound a) -> Map a Bool -> Sentence (Bound a)
canonicalize_ s summary = case collapse . M.toAscList $ summary of
	[] -> Lit constantValue
	bs -> reconstruct bs
	where
	-- if the formula mentions a particular bound, use a value near that bound;
	-- otherwise, it doesn't mention a bound, so we can evaluate the sentence
	-- with the undefined valuation of boundary atoms
	constantValue = case M.minView summary of
		Nothing     -> evalSentence undefined s
		Just (v, _) -> v

	-- pick out just the places where our formula transitions from one
	-- valuation to another
	collapse = filter (\((a, v), (a', v')) -> v /= v')
	         . ap zip tail

	-- given a bunch of phase transitions, cook up a formula with those
	-- transitions
	reconstruct (((x, v), (x', v')):rest) = if v'
		then recurse (&&) (Atom (GE x'))
		else recurse (||) (Atom (LE x ))
		where
		recurse f v = case rest of
			[] -> v
			_  -> Bin v f (reconstruct rest)

canonicalize :: Representative a => Sentence (Bound a) -> Sentence (Bound a)
canonicalize = ap canonicalize_ representatives
-- }}}
-- EDSL combinators {{{
instance Representative a => Property (Sentence (Bound a)) where
	null s = canonicalize s == Lit False
	combine ps = \bs -> canonicalize_ (pFor bs) (valFor bs) where
		globalReps = representatives (foldr (|||) nothing ps)
		valuations = map (\p -> M.mapWithKey (\x _ -> evalSentence (evalBound x) p) globalReps) ps
		pFor   bs  = foldr        (&&&) everything (negationsWith Not       bs ps        )
		valFor bs  = M.unionsWith (&&)             (negationsWith (not <$>) bs valuations)
		negationsWith not = zipWith (\b v -> if b then v else not v)

nothing, everything :: Sentence (Bound a)
nothing    = Lit False
everything = Lit True

le, lt, ge, gt, eq, nle, nlt, nge, ngt, neq :: a -> Sentence (Bound a)
le  = Atom . LE
ge  = Atom . GE
lt  = Not . ge
gt  = Not . le
nlt = ge
ngt = le
nle = gt
nge = lt
eq  x = ge x &&& le x
neq x = lt x ||| gt x

(&&&), (|||) :: Sentence a -> Sentence a -> Sentence a
x &&& y = Bin x (&&) y
x ||| y = Bin x (||) y
-- }}}
