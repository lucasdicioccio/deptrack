{-# LANGUAGE TupleSections #-}


module Deptrack.Graph (
    dagPairs
  , pairsMapToGraph
  , mergePairs
  , GraphWithLookupFunctions
  , dotGraph
  , ToDotInfos (..)
  , roots
  , module Text.Dot
  ) where

import Control.Arrow ((&&&))
import Data.Monoid (Monoid, (<>))
import Data.Map.Strict (Map, fromListWith, toDescList)
import Data.Set (Set, singleton, toList, delete)
import Data.Tree (Tree (..), drawTree)
import Data.Maybe (catMaybes)
import Data.Graph (Graph, Vertex, graphFromEdges, edges, vertices, indegree)
import Data.Array (assocs)

import Text.Dot (Dot, userNode, userNodeId, edge)

type GraphWithLookupFunctions a = (Graph, Vertex -> (a, a, [a]), a -> Maybe Vertex)

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
--
-- Note that we remove loops in the graph as we artificially added loops in
-- the first pattern match of dagPairs.
pairsMapToGraph :: (Ord a) => Map a (Set a) -> GraphWithLookupFunctions a
pairsMapToGraph = graphFromEdges . map (\(d, ds) -> (d, d, toList $ delete d ds)) . toDescList


class ToDotInfos a where
  toDotInfos :: a -> [(String, String)]

dotGraph :: (ToDotInfos a) => GraphWithLookupFunctions a -> Dot ()
dotGraph (g,f,_) = do
    let node v = y where (y,_,_) = f v
    let vs = vertices g
    let es = filter (uncurry (/=)) $ edges g

    mapM_ (\i -> userNode (userNodeId i) (toDotInfos (node i))) vs
    mapM_ (\(i,j) -> edge (userNodeId i) (userNodeId j) []) es

-- | Returns the vertices (indexes) of nodes with no predecessors in the
-- dependency graph.
roots :: Graph -> [Vertex]
roots g = [idx | (idx, v) <- assocs (indegree g)
               , v == 0 ]
