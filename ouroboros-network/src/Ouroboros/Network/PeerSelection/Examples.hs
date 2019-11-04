{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}


module Ouroboros.Network.PeerSelection.Examples where

import           Data.Void (Void)
import           Data.Typeable (Typeable)
import           Data.List (nub)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer(..))

import           Control.Monad.IOSim
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.Governor

import           Test.QuickCheck


-- Things we might like to test...
--
-- * for even insane environments, there are no invariant violations or insane behaviour
-- * for vaguely stable envs, we do stablise at our target number of cold peers
-- * we stabilise without going insane even if the available nodes are fewer than the target
-- * time to stabilise after a change is not crazy
-- * time to find new nodes after a graph change is ok

--TODO: this doesn't make the targets or root peer set dynamic.

data GovernorMockEnvironment = GovernorMockEnvironment {
       peerGraph               :: PeerGraph,
       rootPeerSet             :: Map MockPeerAddr RootPeerInfo,
       targets                 :: PeerSelectionTargets,
       pickKnownPeersForGossip :: PickScript,
       pickColdPeersToForget   :: PickScript
     }
  deriving Show

newtype MockPeerAddr = MockPeerAddr Int
  deriving (Eq, Ord, Show)

newtype PeerGraph = PeerGraph [(GossipScript, MockPeerAddr, [MockPeerAddr])]
  deriving Show

data GossipScript = GossipScript
                      (Maybe [MockPeerAddr])
                      GossipTime
                      (Maybe GossipScript)
  deriving Show

data GossipTime = GossipTimeQuick | GossipTimeSlow | GossipTimeTimeout
  deriving Show

newtype PickScript = PickScript (NonEmpty [Int])
  deriving Show

mockPeerSelectionActions :: (MonadSTM m, MonadTimer m)
                         => GovernorMockEnvironment
                         -> m (PeerSelectionActions MockPeerAddr m)
mockPeerSelectionActions GovernorMockEnvironment {
                           peerGraph = PeerGraph adjacency,
                           rootPeerSet,
                           targets
                         } = do
    scriptVars <-
      Map.fromList <$>
      sequence [ (,) addr <$> newTVarM script
               | (script, addr, _) <- adjacency ]
    let requestPeerGossip addr =
            stepGossipScript scriptVar
          where
            Just scriptVar = Map.lookup addr scriptVars
    return PeerSelectionActions {
      readRootPeerSet          = return rootPeerSet,
      readPeerSelectionTargets = return targets,
      requestPeerGossip
    }
  where
    stepGossipScript scriptVar = do
      (res, time) <- atomically $ do
        GossipScript res time mscript' <- readTVar scriptVar
        case mscript' of
          Nothing      -> return ()
          Just script' -> writeTVar scriptVar script'
        return (res, time)
      threadDelay (interpretGossipTime time)
      case res of
        Nothing        -> fail "no peers"
        Just peeraddrs -> return peeraddrs

interpretGossipTime :: GossipTime -> DiffTime
interpretGossipTime GossipTimeQuick   = 1
interpretGossipTime GossipTimeSlow    = 5
interpretGossipTime GossipTimeTimeout = 25

mockPeerSelectionPolicy  :: MonadSTM m
                         => GovernorMockEnvironment
                         -> m (PeerSelectionPolicy MockPeerAddr m)
mockPeerSelectionPolicy GovernorMockEnvironment {
                          pickKnownPeersForGossip,
                          pickColdPeersToForget
                        } = do
    pickKnownPeersForGossipVar <- newTVarM pickKnownPeersForGossip
    pickColdPeersToForgetVar   <- newTVarM pickColdPeersToForget
    return PeerSelectionPolicy {
      policyPickKnownPeersForGossip = interpretPickScript pickKnownPeersForGossipVar,
      policyPickColdPeersToForget   = interpretPickScript pickColdPeersToForgetVar,
      policyMaxInProgressGossipReqs = 2,
      policyGossipRetryTime         = 3600, -- seconds
      policyGossipBatchWaitTime     = 3,    -- seconds
      policyGossipOverallTimeout    = 10    -- seconds
    }

interpretPickScript :: (MonadSTM m, Ord peeraddr)
                    => TVar m PickScript
                    -> Map peeraddr a
                    -> Int
                    -> STM m [peeraddr]
interpretPickScript scriptVar available pickNum
  | Map.size available <= pickNum
  = return (Map.keys available)

  | otherwise
  = do PickScript (offsets :| script') <- readTVar scriptVar
       case script' of
         []   -> return ()
         x:xs -> writeTVar scriptVar (PickScript (x :| xs))
       return (pickMapKeys available (take pickNum offsets))

pickMapKeys :: Ord a => Map a b -> [Int] -> [a]
pickMapKeys m is =
    nub [ fst (Map.elemAt i' m)
        | i <- is
        , let i' = i `mod` Map.size m
        ]

runGovernorInMockEnvironment :: GovernorMockEnvironment -> Trace Void
runGovernorInMockEnvironment mockEnv =
    runSimTrace $ do
      actions <- mockPeerSelectionActions mockEnv
      policy  <- mockPeerSelectionPolicy  mockEnv
      peerSelectionGovernor
        dynamicTracer
        actions
        policy

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

instance Arbitrary GovernorMockEnvironment where
  arbitrary = do
    peerGraph@(PeerGraph adjacency) <- arbitrary
    rootPeerSet <- undefined (length adjacency)
    targets <- arbitrary
    pickKnownPeersForGossip <- arbitrary
    pickColdPeersToForget   <- arbitrary
    return GovernorMockEnvironment{..}

instance Arbitrary PeerGraph where
  arbitrary = sized $ \sz -> do
    n <- choose (0, sz)
    let addrs = map MockPeerAddr [0..n-1]
    edges <- 

instance Arbitrary PeerSelectionTargets where
  arbitrary = undefined
{-
    PeerSelectionTargets {
       targetNumberOfKnownPeers       :: !Int,
       targetNumberOfEstablishedPeers :: !Int,
       targetNumberOfActivePeers      :: !Int,

       -- Expressed as intervals rather than frequencies
       targetChurnIntervalKnownPeers       :: !DiffTime,
       targetChurnIntervalEstablishedPeers :: !DiffTime,
       targetChurnIntervalActivePeers      :: !DiffTime
     }
-}

instance Arbitrary PickScript where
  arbitrary = undefined

