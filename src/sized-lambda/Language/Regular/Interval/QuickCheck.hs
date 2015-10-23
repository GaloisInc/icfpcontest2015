module Language.Regular.Interval.QuickCheck where

import Control.Monad
import Data.Map hiding (map)
import Data.Maybe
import Language.Regular.Interval
import Language.Regular.Property (combine, equiv)
import Test.QuickCheck

instance Arbitrary a => Arbitrary (Bound a) where
	arbitrary = ap (elements [LE, GE]) arbitrary

instance Arbitrary a => Arbitrary (Sentence a) where
	arbitrary = sized $ \n -> do
		cons <- choose (1, max 1 n)
		case cons of
			1 -> Lit  <$> arbitrary
			2 -> Atom <$> arbitrary
			_ -> oneof [Not <$> scale (subtract 1) arbitrary, do
				m  <- choose (1, n)
				l  <- resize    m  arbitrary
				r  <- resize (n-m) arbitrary
				op <- elements [(&&), (||), (==), (/=), (not .) . (&&), (not .) . (||)]
				return (Bin l op r)
				]

type Prop = Sentence (Bound Integer)
prop_CanonicalizeEval      :: Prop -> Integer -> Bool
prop_CanonicalizeEquiv     :: Prop -> Bool
prop_EquivEval             :: Prop -> Prop -> Integer -> Property
prop_EvalEquiv             :: Prop -> Prop -> Integer -> Property
prop_EquivSometimesFalse   :: Prop -> Prop -> Property
prop_EvalNotEquiv          :: Prop -> Prop -> Integer -> Property
prop_CombineMutex          :: [Prop] -> Property
prop_CanonicalizeCanonical :: Prop -> Prop -> Property

prop_CanonicalizeEval s v = evalSentence (evalBound v) s
                         == evalSentence (evalBound v) (canonicalize s)

prop_CanonicalizeEquiv s = s `equiv` canonicalize s

prop_EquivEval s1 s2 v = (s1 `equiv` s2) ==> (evalSentence (evalBound v) s1 == evalSentence (evalBound v) s2)

prop_EvalEquiv s1 s2 v = ((eval s1 <$> [lo..hi]) == (eval s2 <$> [lo..hi]))
                     ==> (s1 `equiv` s2)
	where
	r1 = representatives s1
	r2 = representatives s2
	lo = orZero minimum . map (fst . fst) $ catMaybes [minViewWithKey r1, minViewWithKey r2]
	hi = orZero maximum . map (fst . fst) $ catMaybes [maxViewWithKey r1, maxViewWithKey r2]
	orZero f [] = 0
	orZero f xs = f xs
	eval s v = evalSentence (evalBound v) s

prop_EquivSometimesFalse s1 s2 = not (s1 `equiv` s2) ==> True

prop_EvalNotEquiv s1 s2 v = evalSentence (evalBound v) s1 /= evalSentence (evalBound v) s2
                        ==> not (s1 `equiv` s2)

prop_CombineMutex ss =
	let combination = combine ss in
	forAll bools $ \bs1 ->
	forAll bools $ \bs2 ->
	(bs1 /= bs2) ==>
	((combination bs1 &&& combination bs2) `equiv` Lit False)
	where
	bools = mapM (const arbitrary) ss

prop_CanonicalizeCanonical s1 s2 = (s1 `equiv` s2) ==> canonicalize s1 == canonicalize s2
