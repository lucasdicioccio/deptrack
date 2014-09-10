{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Deptrack.Monadic (
    DepTrack
  -- basic building blocks
  , rec, nest, value
  , evalTree
  , drawDeps
  -- graph-representation
  , GraphWithLookupFunctions
  , evalGraph
  -- re-export applicative
  , module Control.Applicative
  ) where

import Control.Monad.Free (Free (..), liftF)
import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)
import Data.Tree (Tree (..), drawTree)
import Data.Set (Set, singleton, toList)
import Data.Map.Strict (Map, fromListWith, toDescList)
import Data.Graph (Graph, Vertex, graphFromEdges)
import Data.Maybe (catMaybes)

import Deptrack.Graph

-- | Dependency-tree building steps we can either push/pop dependency levels or
-- record values.
--
-- The `b` type parameter is the type of object we track when building the
-- tree.
--
-- TODO: move the recorded value after a "pop" in the "Pop" constructor
data Dep b a
  = Record {-# UNPACK #-} !Bool {-# UNPACK #-} !b {-# UNPACK #-} !a -- ^ record a value of type b, if the boolean value should be True after a pop
  | Push {-# UNPACK #-} !a          -- ^ push one level deeper
  | Pop {-# UNPACK #-} !a           -- ^ pop one level upper
  deriving Functor

type DepTrack b a = Free (Dep b) a

record :: Bool -> a -> DepTrack a a
record x a = liftF (Record x a a)

push :: DepTrack b ()
push = liftF (Push ())

pop :: DepTrack b ()
pop = liftF (Pop ())

-- | Evaluates a computation discarding the dep tracking.
value :: DepTrack b a -> a
value (Pure a)                   = a
value (Free (Record _ x next))   = value next
value (Free (Push next))         = value next
value (Free (Pop next))          = value next

-- | Records and returns a value
rec :: a -> DepTrack a a 
rec = record False

-- | Nests a computed value.
nest :: (a -> b)       -- ^ a function to project the value to the recorded type, you can use a typeclass for it
     -> DepTrack b a   -- ^ an operation to nest one dependency level deeper
     -> DepTrack b a   
nest f op = do
  push
  x <- op
  pop
  record True (f x)
  return x

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
evalTree' c [] (Free (Pop _))          = error "unmatched push/pop"
-- finish case in normal situation
evalTree' c [] (Pure a)                = (c, a)
-- record a node at current level
evalTree' c ts (Free (Record False x next))  = 
  evalTree' 
    (addLeaf x c)
    ts
    next

    where addLeaf :: a -> Tree (Maybe a) -> Tree (Maybe a)
          addLeaf v (Node Nothing ts)
            = let t = Node (Just v) []
              in Node Nothing (t:ts) 
          addLeaf _ (Node (Just _) _)
            = error "should only add to a Nothing node"

-- record a node at current level and put into the first sibling (this was
-- after a "pop")
evalTree' c ts (Free (Record True x next))  = 
  evalTree' 
    (setRoot x c) 
    ts
    next
    where setRoot :: a -> Tree (Maybe a) -> Tree (Maybe a)
          setRoot v (Node Nothing ((Node Nothing ts):siblings))
                = Node Nothing ((Node (Just v) ts):siblings)
          setRoot _ (Node (Just _) _)
                = error "should only set root to a Nothing node"
-- prepare a new level for next computations
evalTree' c ts (Free (Push next)) =
  evalTree'
    (nothingRoot)
    (c:ts)
    next
-- graft current value to the one pushed last on the stack
evalTree' c (t:ts) (Free (Pop next))   =
  evalTree'
    (graftChild t c)
    ts
    next

    where graftChild parent@(Node x xs) child@(Node y ys) = Node x (child:xs)


------------------------------------------------------------------------------

-- | Evaluates a computation and generate a dependency as a side-effect of
-- computing the value.
evalGraph :: (Ord b) => DepTrack b a -> (GraphWithLookupFunctions b, a)
evalGraph x = 
  let (t,v) = evalTree x
      g = mergePairs $ catMaybes $ map genuinePair $ dagPairs t
      g' = pairsMapToGraph g
  in (g',v)

  where genuinePair (Nothing,_) = Nothing
        genuinePair (_,Nothing) = Nothing
        genuinePair (Just x, Just y) = Just (x,y)
      
-- | Helper to print the dependency tree.
drawDeps :: Show b => DepTrack b a -> IO ()
drawDeps = putStrLn . drawTree . fmap show . fst. evalTree
