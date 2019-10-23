{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Ouroboros.Consensus.Protocol.HardFork.CanHardFork (
    CanHardFork(..)
  ) where

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (HasHeader, SlotNo)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork.Config

{-------------------------------------------------------------------------------
  Class to hard fork between protocols
-------------------------------------------------------------------------------}

-- | Protocol specific functionality required to fork between protocols
class (OuroborosTag p1, OuroborosTag p2) => CanHardFork p1 p2 where
  -- | Should we fork (or already have forked) at this slot?
  shouldFork
    :: NodeConfig (p1 `HardForksTo` p2)
    -> LedgerView p1 -> SlotNo -> Bool

  -- | Convert the 'ChainState' from the old protocol to the new at the boundary
  chainStateAfterFork
    :: NodeConfig (p1 `HardForksTo` p2) -> ChainState p1 -> ChainState p2

  -- | Convert the 'LedgerView' from the old protocol to the new at the boundary
  ledgerViewAfterFork
    :: NodeConfig (p1 `HardForksTo` p2) -> LedgerView p1 -> LedgerView p2

  -- | Do we prefer this candidate?
  --
  -- We use this because it is tricky to define a generic version for any two
  -- protocols.
  --
  -- TODO: Describe what the difficulty is.
  hardForkPreferCandidate
    :: HasHeader hdr
    => NodeConfig (p1 `HardForksTo` p2)
    -> AnchoredFragment hdr -- ^ Our chain
    -> AnchoredFragment hdr -- ^ Candidate
    -> Bool

  -- | Which candidate do we prefer?
  --
  -- We use this because it is tricky to define a generic version for any two
  -- protocols.
  --
  -- TODO: Describe what the difficulty is.
  hardForkCompareCandidates
    :: HasHeader hdr
    => NodeConfig (p1 `HardForksTo` p2)
    -> AnchoredFragment hdr
    -> AnchoredFragment hdr
    -> Ordering
