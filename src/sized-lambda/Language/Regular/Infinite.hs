{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Regular.Infinite where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Graph.Inductive.Tree (Gr) -- the PatriciaTree implementation does not allow multiple edges between two nodes
import Data.Graph.Inductive (Graph, DynGraph, Node, Edge, Adj)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Language.Regular.Property (Property)
import qualified Data.Graph.Inductive as G
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Regular.Interval as Interval
import qualified Language.Regular.Property as P

data Regex a = Lit a | Alt (Regex a) (Regex a) | Cat (Regex a) (Regex a) | Star (Regex a)
	deriving (Eq, Ord, Read, Show)

data FA a = FA
	{ start :: Node
	, step  :: Gr () a
	, final :: [Node]
	}
	deriving (Read, Show)

type EpsilonNFA a = FA (Maybe a)
type DFA = FA
-- eta-reduced version of G.NodeMapM
type NodeMapM a b g = State (G.NodeMap a, g a b)

-- parsers {{{
(f +++ g) s = f s ++ g s
(f ! g) (v, s) = f (g v, s)

rRegex = rRegexMolecule +++ rRegexAtom
rRegexMolecule s = rRegexAtom s >>= rPending
rPending (l, s) = case s of
	'|':s' -> rRegex s' >>= rPending ! Alt l
	'*':s' -> rPending (Star l, s')
	[]     -> [(l, s)]
	s'     -> rRegex s' >>= rPending ! Cat l
rRegexAtom ('\\':c:s) = [(Lit (Interval.eq c), s)]
rRegexAtom ('(':s) = do
	(v, ')':s') <- rRegex s
	return (v, s')
rRegexAtom ('.':s) = [(Lit Interval.everything, s)]
rRegexAtom (c:s)
	| c `elem` "()|*\\" = []
	| otherwise = [(Lit (Interval.eq c), s)]
-- }}}
-- regex -> epsilon-NFA {{{
-- (!) This assumes that the node identities in the two graphs correspond in a
-- meaningful way.
union = G.ufold (G.&)

fresh :: MonadState Node m => m Node
fresh = state (\s -> (s, s+1))

epsilonNFA :: MonadState Node m => Regex a -> m (EpsilonNFA a)
epsilonNFA (Lit a) = do
	s  <- fresh
	s' <- fresh
	return FA
		{ start = s
		, step  = G.mkGraph [(s, ()), (s', ())] [(s, s', Just a)]
		, final = [s']
		}
epsilonNFA (Alt l r) = do
	nfaL <- epsilonNFA l
	nfaR <- epsilonNFA r
	s    <- fresh
	return FA
		{ start = s
		, step  = ([], s,  (), [(Nothing, start nfaL), (Nothing, start nfaR)])
		      G.& union (step nfaL) (step nfaR)
		, final = final nfaL ++ final nfaR
		}
epsilonNFA (Cat l r) = do
	nfaL <- epsilonNFA l
	nfaR <- epsilonNFA r
	return FA
		{ start = start nfaL
		, step  = G.insEdges [(n, start nfaR, Nothing) | n <- final nfaL]
		        $ union (step nfaL) (step nfaR)
		, final = final nfaR
		}
epsilonNFA (Star r) = do
	nfaR <- epsilonNFA r
	s    <- fresh
	return FA
		{ start = s
		, step  = ([(Nothing, n) | n <- final nfaR], s, (), [(Nothing, start nfaR)])
		      G.& step nfaR
		, final = [s]
		}
-- }}}
-- epsilon-NFA -> DFA {{{
epsilonClose g n = G.xdfsWith epsilonSuccessors G.node' [n] g where
	epsilonSuccessors ctxt = [n | (n, Nothing) <- G.lsuc' ctxt]
epsilonCloseAll g = M.fromList [(n, S.fromList (epsilonClose g n)) | n <- G.nodes g]

-- (!) Assumption: 'combine' produces sentences in some canonical form;
-- specifically, that the 'Eq' instance for @p@ is actually property
-- equivalence checking when used on outputs of 'combine'.
determinize :: (Ord p, Property p) => EpsilonNFA p -> DFA p
determinize enfa = FA
	{ start = startNodeLabel
	, step  = G.nmap (\_ -> ()) graph
	, final = finalNodes
	}
	where
	epsilonClosures     = epsilonCloseAll (step enfa)
	startNode           = epsilonClosures M.! start enfa
	(startNodeLabel, _) = G.mkNode_ nodeMap startNode
	(finalNodes, (nodeMap, graph))
		= G.run G.empty (evalStateT (execWriterT (go startNode)) S.empty)

	edgesFrom_ s = M.fromListWith (<>) $ do
		label <- S.toList s
		(_, to, Just label) <- G.out (step enfa) label
		return (label, [to])

	-- bs: bools (telling whether to include a property or not)
	-- ps: properties
	-- nss: node sets
	edgesFrom s = M.fromListWith S.union $
		[ (p, foldMap (epsilonClosures M.!) s')
		| let esMapRaw    = edgesFrom_ s
		      esAssocsRaw = M.assocs esMapRaw
		      (ps, nss_)  = unzip esAssocsRaw
		      psCombined  = P.combine ps
		      nss         = S.fromList <$> nss_
		, bs <- replicateM (M.size esMapRaw) [False, True]
		, let p  = psCombined bs
		      s' = mask bs nss
		, not (P.null p)
		]
	mask bs nss = S.unions
	              $ zipWith (\b ns -> if b then ns else S.empty) bs nss

	liftNodeMapM :: m ~ NodeMapM (Set Node) a Gr
	             => m v -> WriterT [Node] (StateT (Set Node) m) v
	liftNodeMapM = lift . lift

	go n = do
		seen     <- get
		newLabel <- liftNodeMapM (insMapNodeM n)
		unless (newLabel `S.member` seen) $ do
			modify (S.insert newLabel)
			when (any (`elem` final enfa) n) (tell [newLabel])
			let newEdges = M.assocs $ edgesFrom n
			newEdgesRaw <- forM newEdges $ \(edgeLabel, to) -> do
				toLabel <- liftNodeMapM (insMapNodeM to)
				return (newLabel, toLabel, edgeLabel)
			liftNodeMapM (insEdgesM newEdgesRaw)
			mapM_ (go . snd) newEdges

-- don't crash when trying to insert a node that already exists
insMapNodeM :: (DynGraph g, Ord a) => a -> NodeMapM a b g Node
insMapNodeM n = do
	(m, g) <- get
	let ((nL, _), m') = G.mkNode m n
	put $ case G.match nL g of
		(Nothing, _) -> (m', G.insNode (nL, n) g)
		_            -> (m, g)
	return nL

-- insert edges where we don't have to look up node labels
insEdgesM :: DynGraph g => [(Node, Node, e)] -> NodeMapM n e g ()
insEdgesM es = modify (\(m, g) -> (m, G.insEdges es g))
-- }}}

dfa s = case rRegex s of
	(v, ""):_ -> determinize (evalState (epsilonNFA v) 0)
