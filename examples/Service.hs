{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | A Service monitoring.
--
-- Here we do merely use deptrack to record service dependencies for some
-- internet hosts that I own.
module Service where

import Prelude hiding (lookup)
import Deptrack.Graph
import Deptrack.Monadic
import Data.Traversable (sequenceA, traverse)
import Text.Dot (showDot)
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Data.Tree
import qualified Data.Array as A

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM hiding (check)
import Control.Monad.STM hiding (check)

import System.Process
import GHC.IO.Exception

-- | A representation of our dependency
data Dependency
  = Abstraction String
  | Checkable Check
  deriving (Show, Ord, Eq)

data Check = Check {
    checkKey    :: String
  , checkAction :: IO Bool
  }

depAction (Checkable (Check _ x)) = x
depAction s                       = print s >> return True

instance ToDotInfos Dependency where
  toDotInfos (Checkable (Check k _)) = [("label",k), ("shape","rectangle")]
  toDotInfos (Abstraction k)         = [("label",k), ("shape","egg")]

instance Show Check where
  show a = show $ checkKey a

instance Eq Check where
  a == b = checkKey a == checkKey b

instance Ord Check where
  a `compare` b = checkKey a `compare` checkKey b


check :: String -> IO (Bool) -> Dependency
check n x = Checkable $ Check n x

site :: String -> Dependency
site = Abstraction

pingDep :: String -> Dependency
pingDep host = check ("can-ping: " ++ host) 
                     ((==ExitSuccess) <$> rawSystem "ping" ["-c", "3", host])

curlDep :: String -> Dependency
curlDep url = check ("can-curl: " ++ url) 
                    ((==ExitSuccess) <$> rawSystem "curl" ["-i", url])

pingable = nest pingDep
curlable = nest curlDep

webServer x = do
  nest site (bookmyname)
  curlable (pure x)
  pingable (pure x)

dnsRoot x = do
  pingable (pure x)

dicioccio    = webServer "dicioccio.fr"
haskellParis = webServer "haskell-paris.fr"
probecraft   = webServer "probecraft.net"
bookmyname   = dnsRoot "bookmyname.com"
neverworks   = webServer "example.42.local"

hostnames = traverse (nest site) [dicioccio, haskellParis, probecraft, neverworks]

runDeps cache (Node Nothing xs)    = all id <$> mapConcurrently (runDeps cache) xs
runDeps cache (Node (Just dep) xs) = all id <$> zs
  where zs = (:) <$> runOne cache dep <*> mapConcurrently (runDeps cache) xs 

type Cache b a = TVar (Map b (TMVar (Maybe a)))
newCache = newTVarIO M.empty 

-- | runs a dependency check if its result is not already known from the cache
--
-- if there is a TVar for this key, we just wait for completion
-- else we set a TVar and propose an action to fill the TVar with the value
runOne :: Cache Dependency Bool -> Dependency -> IO Bool
runOne cache dependency = do
        let waitFull tmvar = do
                maybe retry (return . return) =<< readTMVar tmvar
        let finalize tmvar = do
                x <- depAction dependency -- XXX here we run the check, we should atomically catch errors and do something about it => retry, cancel, let other processes take a chance etc.
                atomically $ swapTMVar tmvar $ Just x
                return x
        let go = do
                tmvar <- newTMVar Nothing
                modifyTVar cache $ insert dependency tmvar
                return $ finalize tmvar
        let begin = do
                c <- lookup dependency <$> readTVar cache
                maybe go waitFull c
        act <- atomically begin
        act

example = do
  let x = fst $ evalTree hostnames
  newCache >>= flip runDeps x

instance ToDotInfos (Dependency, Maybe Bool) where
  toDotInfos (x,Nothing)       = ("color","blue") : toDotInfos x
  toDotInfos (x,Just True)     = ("color","green") : toDotInfos x
  toDotInfos (x,Just False)    = ("color","red") : toDotInfos x

example2 :: IO (Dot ())
example2 = do
  let ((g,f1,f2),_) = evalGraph hostnames
  let rs = roots g
  cache <- newCache
  mapConcurrently (go cache g f1) rs
  r <- atomically $ readTVar cache >>= traverse readTMVar
  let f1' idx = let (d,_,_) = f1 idx in ((d,join $ lookup d r), undefined, undefined)
  return $ dotGraph (g,f1',undefined)
  where go c g f idx = do
                 let (dependency,_,_) = (f idx)
                 runOne c dependency
                 let children = (A.!) g idx
                 concat <$> mapConcurrently (go c g f) children
