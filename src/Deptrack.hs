
module Deptrack (
    module Deptrack.Applicative
  , module Text.Dot
  , dotGraph
  , ToDotInfos (..)
  ) where

import Deptrack.Applicative
import Data.Graph (edges, vertices)
import Text.Dot

class ToDotInfos a where
  toDotInfos :: a -> [(String, String)]

dotGraph :: (ToDotInfos a) => GraphWithLookupFunctions a -> Dot ()
dotGraph (g,f,_) = do
    let node v = y where (y,_,_) = f v
    let vs = vertices g
    let es = filter (uncurry (/=)) $ edges g

    mapM_ (\i -> userNode (userNodeId i) (toDotInfos (node i))) vs
    mapM_ (\(i,j) -> edge (userNodeId i) (userNodeId j) []) es
