{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

module Sysops where

{- would be great to support something like
 -
 -      worker = dep $ do
 -        x `remote` (nat >> service "my-worker" [])
 -
 - we may want to explicit remotable actions from non-remotable,
 - GADTs/phantom types may help ensuring we only construct validly remotable actions
 - trick is to annotate all operations under a "remote" call to have them become remote
 - one option is to annotate the sub-tree by fmapping it before graph-generation
 -
 -      remotely :: DepTrack (Remote SysOp) a -> DepTrack SysOp a
 -      remotely = undefined
-}

import Deptrack.Monadic
import Control.Applicative ((<$>),(<*>))
import Data.Traversable (traverse)
import Control.Monad (forM_, void)
import Control.Exception (catch, SomeException)
import Data.Graph (topSort)
import Data.Monoid ((<>))
import System.Process (createProcess, shell, proc, readProcessWithExitCode, callProcess)
import System.Exit (ExitCode (..))
import System.Directory (doesFileExist, copyFile)

newtype Key = Key String
  deriving (Show, Ord, Eq)

newtype Source a = Source a
newtype Destination a = Destination a

newtype Action = Action { runAction :: IO () }
newtype Check  = Check { runCheck :: IO Bool }

instance Show Action where
  show _ = "<runAction>"
instance Show Check where
  show _ = "<runCheck>"

data SysOp where
  -- operation that are idempotent (and cheap)
  IdempotentOperation :: Key -> Action          -> SysOp 
  -- operation that cannot be made idempotent (or that are prohibitevly
  -- expensive like compiling a kernel)
  CheckableOperation  :: Key -> Check -> Action -> SysOp
  -- operation for checking a property dynamically
  VerificationOperation  :: Key -> Check -> SysOp
  deriving Show

instance Eq SysOp where
  IdempotentOperation j _ == IdempotentOperation k _      = k == j
  CheckableOperation j _ _ == CheckableOperation k _ _    = k == j
  VerificationOperation j _ == VerificationOperation k _  = k == j
  _ == _                                                  = False

instance Ord SysOp where
  IdempotentOperation j _ `compare` IdempotentOperation k _     = k `compare` j
  CheckableOperation j _ _ `compare` CheckableOperation k _ _   = k `compare` j
  VerificationOperation j _ `compare` VerificationOperation k _ = k `compare` j
  IdempotentOperation j _ `compare` VerificationOperation k _   = GT
  IdempotentOperation k _ `compare` CheckableOperation j _ _    = GT
  VerificationOperation k _ `compare` CheckableOperation j _ _  = GT
  _ `compare`_                                                  = LT

class IsSysOp a where
  toSysOp :: a -> SysOp

instance IsSysOp SysOp where
  toSysOp = id

-- modelization

op :: IsSysOp a => a -> DepTrack SysOp a
op = nest toSysOp . pure

dep :: IsSysOp a => DepTrack SysOp a -> DepTrack SysOp a
dep = nest toSysOp

type Login = String
type Name = String
type FullName = String
type Owner = String
type UID = Int
type GID = Int

data SaltUser = PresentSaltUser Login FullName FilePath UID GID [SaltGroup]
  | AbsentSaltUser Login
data SaltGroup = PresentSaltGroup Name GID
  | AbsentSaltGroup Name
data User = User Login
data Bridge = Bridge Name
data Tap = Tap Name User
data BridgedInterface = BridgedTap Bridge Tap
data NAT = NAT
data Forwarding = Forwarding
data File
  = FilePresent FilePath
  | FileCopy (Source File) (Destination FilePath)
data Process = ProcessRunning FilePath [String] Name
data Package = DebianPackage Name

procAction :: FilePath -> [String] -> Action
procAction x xs = Action (void (log >> act))
  where cp = proc x xs
        log = print (x,xs)
        act = createProcess cp

instance IsSysOp User where
  toSysOp (User k)    = IdempotentOperation (Key k) (procAction "adduser" [k])
instance IsSysOp Bridge where
  toSysOp (Bridge k)  = IdempotentOperation (Key k) (procAction "brctl" ["addbr", k])
instance IsSysOp BridgedInterface where
  toSysOp (BridgedTap (Bridge k) (Tap t _))  = IdempotentOperation (Key $ "iface" <> k <> "t") (procAction "brctl" ["addif", k, t])
instance IsSysOp Tap where
  toSysOp (Tap k (User o))   = IdempotentOperation (Key k) (procAction "tunctl" ["-u",o,"-t",k])
instance IsSysOp File where
  toSysOp (FilePresent p)    = VerificationOperation (Key p) (Check $ doesFileExist p)
  toSysOp (FileCopy (Source (FilePresent src)) (Destination p))    = CheckableOperation (Key p)
    (Check $ doesFileExist p)
    (Action $ copyFile src p)
instance IsSysOp Forwarding where
  toSysOp Forwarding  = IdempotentOperation (Key "forwarding")
    (Action $ writeFile "/proc/sys/net/ipv4/ip_forward" "1")
instance IsSysOp Process where
  toSysOp (ProcessRunning k xs p)  = CheckableOperation (Key $ "process:" <> k)
    (Check $ (\(c,_,_) -> c == ExitSuccess) <$> readProcessWithExitCode "pgrep" [k] "")
    (Action $ print (k,xs) >> callProcess k xs)
instance IsSysOp Package where
  toSysOp (DebianPackage p) = IdempotentOperation (Key p)
    (Action $ print (x, xs) >> callProcess x xs)
    where (x,xs) = ("apt-get", ["install", p])

instance IsSysOp NAT where
  toSysOp NAT         = CheckableOperation (Key "nat")
    (Check $ print "TODO: iptables -L …" >> return False)
    (Action $ print "TODO iptables -A FORWARD …")

deb n           = op (DebianPackage n)
nat             = op NAT
forwarding      = op Forwarding
bridge k        = op (Bridge k)
tap `bridgedTo` bridge = dep (BridgedTap <$> bridge <*> tap)
user k          = op (User k)
filePresent p   = op (FilePresent p)
fileCopy s d    = dep (f <$> filePresent s)
  where f s = FileCopy (Source s) (Destination d)
process k as p  = op (ProcessRunning k as p)
tap x           = dep (Tap x <$> user "dicioccio")
dhcp            = dep $ do
  deb "isc-dhcp-server"
  let br0 = "br0"
  let dhcpConf = "./rundir/dhcp.conf"
  fileCopy "./conf/dhcpcd-alt.conf" dhcpConf
  bridge "br0"
  process "dhcpcd" [ "-f", "-d", "-cf", dhcpConf, br0 ] "dhcpcd"

vm              = dep $ do
  let qemuImage = "./rundir/qemu.img"
  let tapIface  = "tap16"
  traverse deb ["qemu", "qemu-kvm", "qemu-system", "qemu-user", "qemu-utils"]
  forwarding >> nat >> tap tapIface `bridgedTo` bridge "br0" >> dhcp
  fileCopy "./imgdir/debian-7.6-x64.qcow2.HaLVM" qemuImage
  process "qemu-system-x86_64" [ "-enable-kvm", "-nographic"
                               , "-daemonize"
                               , "-m", "256"
                               , "-hda", qemuImage
                               , "-net", "nic,macaddr=52:54:11:22:33:16"
                               , "-net", "tap,ifname=" <> tapIface <> ",script=no,downscript=no"
                               ]
                               "qemu"

example = do
  let xs = vm
  let ((g,f1,f2),v) = evalGraph xs
  let indices = reverse $ topSort g
  forM_ indices $ \idx -> case (f1 idx) of
      ((CheckableOperation k c x), _, _)
        -> print k >> runCheck c >>= \r -> if r then print "skipped" else runAction x
      ((IdempotentOperation k x), _, _)
        -> print k >> runAction x
      ((VerificationOperation k c), _, _)
        -> print k >> runCheck c >>= \r -> if r then print "ok" else print "not verified"

      `catch` printException
      
   where printException :: SomeException -> IO ()
         printException = print
