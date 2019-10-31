{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.KnownPeers (
    -- * Types
    KnownPeers,
    KnownPeerInfo(..),
    invariant,

    -- * Basic container operations
    empty,
    size,
--    insert,
    delete,
    toMap,

    -- * Special operations
    setTime,
    setGossipTime,
    availableForGossip,
    adjustRootSet,
  ) where

import qualified Data.Foldable as Foldable
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.PriorityQueue.FingerTree as PQueue
import           Data.PriorityQueue.FingerTree (PQueue)

import           Control.Monad.Class.MonadTime

import           Ouroboros.Network.PeerSelection.Types


-------------------------------
-- Known peer set representation
--

data KnownPeerInfo = KnownPeerInfo {
       knownPeerAdvertise :: !Bool,
       knownPeerOther     :: ()
     }
  deriving (Eq, Show)


data KnownPeers peeraddr = KnownPeers {

       -- | All the known peers.
       --
       knownPeersByAddr             :: !(Map peeraddr KnownPeerInfo),

       -- | The subset of known peers that we would be allowed to gossip with
       -- now. This is because we have not gossiped with them recently.
       --
       knownPeersAvailableForGossip :: !(Set peeraddr),

       -- | The subset of known peers that we cannot gossip with now. It keeps
       -- track of the next time we are allowed to gossip with them.
       --
       knownPeersNextGossipTimes    :: !(PQueue Time peeraddr)
     }

invariant :: Ord peeraddr => KnownPeers peeraddr -> Bool
invariant KnownPeers{..} =
    knownPeersAvailableForGossip
 <> Set.fromList (Foldable.toList knownPeersNextGossipTimes)
 ==
    Map.keysSet knownPeersByAddr

empty :: KnownPeers peeraddr
empty =
    KnownPeers {
      knownPeersByAddr             = Map.empty,
      knownPeersAvailableForGossip = Set.empty,
      knownPeersNextGossipTimes    = PQueue.empty
    }

toMap :: KnownPeers peeraddr -> Map peeraddr KnownPeerInfo
toMap = knownPeersByAddr

availableForGossip :: Ord peeraddr => KnownPeers peeraddr -> Map peeraddr KnownPeerInfo
availableForGossip KnownPeers {knownPeersByAddr, knownPeersAvailableForGossip} =
    Map.restrictKeys knownPeersByAddr knownPeersAvailableForGossip

delete :: [peeraddr] -> KnownPeers peeraddr -> KnownPeers peeraddr
delete = undefined

size :: KnownPeers peeraddr -> Int
size = Map.size . knownPeersByAddr


setTime :: Time -> KnownPeers peeraddr -> KnownPeers peeraddr
setTime _ _ = undefined

{-
pqueueTakeLessThan :: Ord a => a -> PQueue Time a -> ([a], PQueue Time a)
pqueueTakeLessThan = undefined

-}

setGossipTime :: [peeraddr]
              -> Time
              -> KnownPeers peeraddr
              -> KnownPeers peeraddr
setGossipTime _ _ = undefined


adjustRootSet :: RootPeerSet peeraddr
              -> RootPeerSet peeraddr
              -> KnownPeers peeraddr
              -> (KnownPeers peeraddr,
                  RootPeerSet peeraddr,
                  RootPeerSet peeraddr,
                  Set peeraddr)
adjustRootSet _rootPeers _rootPeers' _knownPeers = undefined
--    (knownPeers', added, changed, removed)
{-
  where
    -- Ephemeral peers that we'll be removing from the knownPeers set
    -- and from the established or active sets if necessary.
    vanishingEphemeralPeers =
      Map.filter rootPeerEphemeral $
      Map.difference rootPeers rootPeers'

    knownPeers' =
        -- and add all new root peers
        Map.unionWith mergeRootAndKnownPeer
                      (Map.map rootToKnownPeer rootPeers')
        -- remove old ephemeral root peers
      $ Map.difference knownPeers vanishingEphemeralPeers

    -- Keep the existing KnownPeer but override things specific to
    -- root peers
    mergeRootAndKnownPeer r k =
      k { knownPeerAdvertise = knownPeerAdvertise r }

    -- Root peer to a KnownPeer with minimal info, as if previously unknown.
    rootToKnownPeer RootPeerInfo{rootPeerAdvertise} =
      KnownPeerInfo {
        knownPeerAdvertise = rootPeerAdvertise,
        knownPeerOther     = ()
      }
-}



