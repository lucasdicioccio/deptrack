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
import Data.Traversable (sequenceA)
import Text.Dot (showDot)
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Data.Tree

import Control.Monad
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM 
import Control.Monad.STM 

import System.Process
import GHC.IO.Exception

type Key = String
-- | A representation of our dependency
data Dependency = Dependency {
    depKey   :: Key
  , depCheck :: IO Bool
  }

instance Show Dependency where
  show a = show $ depKey a

instance Eq Dependency where
  a == b = depKey a == depKey b

instance Ord Dependency where
  a `compare` b = depKey a `compare` depKey b

pingDep :: String -> Dependency
pingDep host = Dependency ("can-ping: " ++ host) 
                          ((==ExitSuccess) <$> rawSystem "ping" ["-c", "3", host])

curlDep :: String -> Dependency
curlDep url = Dependency ("can-curl: " ++ url) 
                          ((==ExitSuccess) <$> rawSystem "curl" ["-i", url])

pingable = nest pingDep
curlable = nest curlDep

webServer x = do
  curlable (pure x)
  pingable (pure x)

dicioccio    = webServer "dicioccio.fr"
haskellParis = webServer "haskell-paris.fr"
probecraft   = webServer "probecraft.net"

hostnames = sequenceA [dicioccio, haskellParis, probecraft]

runDeps cache (Node Nothing xs)    = all id <$> mapConcurrently (runDeps cache) xs
runDeps cache (Node (Just dep) xs) = all id <$> zs
  where zs = (:) <$> runOne cache dep <*> mapConcurrently (runDeps cache) xs 

type Cache a = TVar (Map Key (TMVar (Maybe a)))
newCache = newTVarIO M.empty 

-- | runs a dependency check if its result is not already known from the cache
--
-- if there is a TVar for this key, we just wait for completion
-- else we set a TVar and propose an action to fill the TVar with the value
runOne :: Cache Bool -> Dependency -> IO Bool
runOne cache Dependency{..} = do
        let waitFull tmvar = do
                maybe retry (return . return) =<< readTMVar tmvar
        let finalize tmvar = do
                x <- depCheck -- XXX here we run the check, we should atomically catch errors and do something about it => retry, cancel, let other processes take a chance etc.
                atomically $ swapTMVar tmvar $ Just x
                return x
        let go = do
                tmvar <- newTMVar Nothing
                modifyTVar cache $ insert depKey tmvar
                return $ finalize tmvar
        let begin = do
                c <- lookup depKey <$> readTVar cache
                maybe go waitFull c
        act <- atomically begin
        act

example = do
  let x = fst $ evalTree hostnames
  newCache >>= flip runDeps x
