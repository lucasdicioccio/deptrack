{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Deptrack.Applicative (
    DepTrack
  -- basic building blocks
  , rec, nest, value
  -- tree-representation
  , evalTree
  , drawDeps
  -- graph-representation
  , GraphWithLookupFunctions
  , evalGraph
  -- re-export applicative
  , module Control.Applicative
  ) where

import Control.Applicative.Free (Ap (..), liftAp)
import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)
import Control.Arrow ((&&&))
import Data.Monoid (Monoid, (<>))
import Data.Tree (Tree (..), drawTree)
import Data.Set (Set, singleton, toList)
import Data.Map.Strict (Map, fromListWith, toDescList)
import Data.Graph (Graph, Vertex, graphFromEdges)

-- | Dependency-tree building steps we can either push/pop dependency levels or
-- record values.
--
-- The `b` type parameter is the type of object we track when building the
-- tree.
--
-- TODO: move the recorded value after a "pop" in the "Pop" constructor
data Dep b a
  = Record Bool b a -- ^ record a value of type b, if the boolean value should be True after a pop
  | Push a          -- ^ push one level deeper
  | Pop a           -- ^ pop one level upper
  deriving Functor

type DepTrack b a = Ap (Dep b) a

record :: Bool -> a -> DepTrack a a
record x a = liftAp (Record x a a)

push :: DepTrack b ()
push = liftAp (Push ())

pop :: DepTrack b ()
pop = liftAp (Pop ())

-- | Evaluates a computation discarding the dep tracking.
value :: DepTrack b a -> a
value (Pure a)                   = a
value (Ap (Record _ x v) next)   = value (next <*> pure v)
value (Ap (Push v) next)         = value (next <*> pure v)
value (Ap (Pop v) next)          = value (next <*> pure v)

-- | Records and returns a value
rec :: a -> DepTrack a a 
rec = record False

-- | Nests a computed value.
nest :: (a -> b)       -- ^ a function to project the value to the recorded type, you can use a typeclass for it
     -> DepTrack b a   -- ^ an operation to nest one dependency level deeper
     -> DepTrack b a   
nest f op = push *> op <* pop <* (record True (f (value op)))

------------------------------------------------------------------------------

-- | Seed value for dependency trees.
nothingRoot = Node Nothing []

-- | Evaluates a computed value along with its dependency tree.
evalTree :: DepTrack b a -> (Tree (Maybe b), a)
evalTree = evalTree' nothingRoot []

-- | Evaluates a computed value along with its dependency tree.
evalTree' :: 
     Tree (Maybe b)   -- ^ current tree being evaluated
  -> [Tree (Maybe b)] -- ^ current stack of trees for the evaluation
  -> DepTrack b a     -- ^ computation to evaluate
  -> (Tree (Maybe b), a)
-- error cases when we have leftovers or asymmetric push/pop
evalTree' c (_:_) (Pure a)             = error "leftovers"
evalTree' c [] (Ap (Pop _) _)          = error "unmatched push/pop"
-- finish case in normal situation
evalTree' c [] (Pure a)                = (c, a)
-- record a node at current level
evalTree' c ts (Ap (Record False x v) next)  = 
  evalTree' 
    (addLeaf x c)
    ts
    (next <*> pure v)

    where addLeaf :: a -> Tree (Maybe a) -> Tree (Maybe a)
          addLeaf v (Node Nothing ts)
            = let t = Node (Just v) []
              in Node Nothing (t:ts) 
          addLeaf _ (Node (Just _) _)
            = error "should only add to a Nothing node"

-- record a node at current level and put into the first sibling (this was
-- after a "pop")
evalTree' c ts (Ap (Record True x v) next)  = 
  evalTree' 
    (setRoot x c) 
    ts
    (next <*> pure v)
    where setRoot :: a -> Tree (Maybe a) -> Tree (Maybe a)
          setRoot v (Node Nothing ((Node Nothing ts):siblings))
                = Node Nothing ((Node (Just v) ts):siblings)
          setRoot _ (Node (Just _) _)
                = error "should only set root to a Nothing node"
-- prepare a new level for next computations
evalTree' c ts (Ap (Push v) next) =
  evalTree'
    (nothingRoot)
    (c:ts)
    (next <*> pure v)
-- graft current value to the one pushed last on the stack
evalTree' c (t:ts) (Ap (Pop v) next)   =
  evalTree'
    (graftChild t c)
    ts
    (next <*> pure v)

    where graftChild parent@(Node x xs) child@(Node y ys) = Node x (child:xs)


------------------------------------------------------------------------------

-- | Generic list summary function.
--
-- Values for a same key gets crunched together using to the Monoid property of
-- the value space.
--
-- example: countBy f = aggolmerate f (const (Sum 1))
agglomerate :: (Ord k, Monoid v) => 
     (a -> k)  -- ^ projection to a key space
  -> (a -> v)  -- ^ projection to a value space
  -> [a]       -- ^ list of items to summarize
  -> Map k v
agglomerate fk fv = fromListWith (<>) . (map (fk &&& fv))

-- | Generates the list of directed pairs for a tree.
dagPairs :: Tree a -> [(a, a)]
dagPairs (Node x []) = [(x,x)]
dagPairs (Node a ys) = map ((a,) . rootLabel) ys ++ concatMap dagPairs ys

-- | Generates a graph with a Map node (Set node) representation from a list of
-- directed pairs.
mergePairs :: (Ord a) => [(a,a)] -> Map a (Set a)
mergePairs = agglomerate fst (singleton . snd)

-- | Generates a Graph and the associated Vertex lookup functions See
-- Data.Graph.graphFromEdges
pairsMapToGraph :: (Ord a) => Map a (Set a) -> GraphWithLookupFunctions a
pairsMapToGraph = graphFromEdges . map (\(d, ds) -> (d, d, toList ds)) . toDescList

type GraphWithLookupFunctions a = (Graph, Vertex -> (a, a, [a]), a -> Maybe Vertex)

-- | Evaluates a computation and generate a dependency as a side-effect of
-- computing the value.
evalGraph :: (Ord b) => DepTrack b a -> (GraphWithLookupFunctions (Maybe b), a)
evalGraph x = 
  let (t,v) = evalTree x
      g = mergePairs $ dagPairs t
      g' = pairsMapToGraph g
  in (g',v)

------------------------------------------------------------------------------

-- | Helper to print the dependency tree.
drawDeps :: Show b => DepTrack b a -> IO ()
drawDeps = putStrLn . drawTree . fmap show . fst. evalTree