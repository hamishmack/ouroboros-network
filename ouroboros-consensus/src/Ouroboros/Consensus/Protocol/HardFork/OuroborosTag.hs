{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The OuroborosTag instance itself
module Ouroboros.Consensus.Protocol.HardFork.OuroborosTag (
    HardForksTo
    -- * Classes
  , CanHardFork(..)
  , HardForkSupportedHeader(..)
  ) where

import           Control.Exception (assert)
import           Control.Monad.Trans.Except (Except, withExcept)
import           Data.Kind (Type)

import           Ouroboros.Network.Block (HasHeader (..))
import           Ouroboros.Network.Point (at)

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.HardFork.CanHardFork
import           Ouroboros.Consensus.Protocol.HardFork.ChainState
                     (AfterForkChainState (..))
import qualified Ouroboros.Consensus.Protocol.HardFork.ChainState as AFCS
import           Ouroboros.Consensus.Protocol.HardFork.Config
import           Ouroboros.Consensus.Protocol.HardFork.Forked
import           Ouroboros.Consensus.Protocol.HardFork.ProjectState

{-------------------------------------------------------------------------------
  SupportedHeader
-------------------------------------------------------------------------------}

class ( SupportedHeader p1 (ForkedBefore hdr)
      , SupportedHeader p2 (ForkedAfter hdr)
      , HasHeader (ForkedAfter hdr)
      ) => HardForkSupportedHeader p1 p2 hdr where

  type ForkedBefore hdr :: Type
  type ForkedAfter  hdr :: Type

  getForkedHeader
    :: NodeConfig (p1 `HardForksTo` p2)
    -> hdr -> Forked (ForkedBefore hdr) (ForkedAfter hdr)

{-------------------------------------------------------------------------------
  HardForksTo Combinator
-------------------------------------------------------------------------------}

-- | We need to run /both/ protocols, so need to have state for both
--
-- Since this is independent of the state of the chain, we store both states,
-- rather than first storing the former and later storing the latter.
type HardForkNodeState p1 p2 = (NodeState p1, NodeState p2)

instance CanHardFork p1 p2 => OuroborosTag (p1 `HardForksTo` p2) where
  type IsLeader        (p1 `HardForksTo` p2) = Forked (IsLeader      p1) (IsLeader      p2)
  type LedgerView      (p1 `HardForksTo` p2) = Forked (LedgerView    p1) (LedgerView    p2)
  type ValidationErr   (p1 `HardForksTo` p2) = Forked (ValidationErr p1) (ValidationErr p2)
  type ChainState      (p1 `HardForksTo` p2) = Forked (ChainState    p1) (AfterForkChainState p1 p2)
  type NodeState       (p1 `HardForksTo` p2) = HardForkNodeState       p1 p2
  type SupportedHeader (p1 `HardForksTo` p2) = HardForkSupportedHeader p1 p2

  -- Forward to 'CanHardFork'

  preferCandidate   = hardForkPreferCandidate
  compareCandidates = hardForkCompareCandidates

  -- We do not allow the security parameter to change

  protocolSecurityParam (HardForkCfg p1 p2) =
      assert (protocolSecurityParam p1 == protocolSecurityParam p2) $
        protocolSecurityParam p1

  -- Leader check

  checkIsLeader cfg@(HardForkCfg cfgP1 cfgP2) slotNo lview cstate =
      case forkedPair lview cstate of
        -- If we are after the hard fork, just delegate to p2
        Just (AfterFork (lviewP2, afcs)) -> runAfter $
          fmap AfterFork <$>
            checkIsLeader cfgP2 slotNo lviewP2 (afcsState afcs)

        -- If before the fork, and it's not yet time to switch, delegate to p1
        Just (BeforeFork (lviewP1, cstateP1))
          | not (shouldFork cfg lviewP1 slotNo) -> runBefore $
              fmap BeforeFork <$>
                checkIsLeader cfgP1 slotNo lviewP1 cstateP1

        -- If it's time to switch, however, we should produce a leader proof for
        -- protocol p2 rather than p1.
          | otherwise -> runAfter $ do
              let lviewP2  = ledgerViewAfterFork cfg lviewP1
                  cstateP2 = chainStateAfterFork cfg cstateP1
              fmap AfterFork <$>
                checkIsLeader cfgP2 slotNo lviewP2 cstateP2

        Nothing ->
          error "checkIsLeader: inconsistent ledger view and chain state"

  -- Apply header
  --
  -- Applying a header is the second place where we might detect that we have to
  -- switch; in 'checkIsLeader' we must check if /we/ should be responsible for
  -- initiating the hard fork, but here we might find that somebody else has.

  applyChainState cfg@(HardForkCfg cfgP1 cfgP2) lview hdr cstate =
      case (forkedPair lview (getForkedHeader cfg hdr), cstate) of
        -- Before the fork. Delegate to P1
        (Just (BeforeFork (lviewP1, hdrP1)), BeforeFork cstateP1) ->
          mapExcept BeforeFork BeforeFork $
            applyChainState cfgP1 lviewP1 hdrP1 cstateP1

        -- After the fork. Delegate to P2
        (Just (AfterFork (lviewP2, hdrP2)), AfterFork afcs) ->
          mapExcept AfterFork AfterFork $
            fmap (AFCS.dropSnapshotIfRedundant cfgP2 hdrP2) $
              AFCS.update afcs $ applyChainState cfgP2 lviewP2 hdrP2

        -- The ledger view is already updated, but the chain state hasn't yet
        -- We assume that a "premature fork" would be detected by the ledger.
        (Just (AfterFork (lviewP2, hdrP2)), BeforeFork cstateP1) -> do
          let afcs = AFCS.init cfg hdrP2 cstateP1
          mapExcept AfterFork AfterFork $
            AFCS.update afcs $ applyChainState cfgP2 lviewP2 hdrP2

        -- Error cases

        (Just (BeforeFork _), AfterFork _) ->
          -- We have a ledger view from /before/ the fork, but a chain state
          -- from /after/ the fork. This can't happen: we only construct the
          -- after-fork chain state when receiving an after-fork leger view.
          -- TODO: Think through what happens during rollback.
          error "applyChainState:  inconsistent ledger view and chain state"
        (Nothing, _) ->
          -- We have a before-fork header and an after-fork ledger view, or
          -- an after-fork header and a before-fork ledger view. Either way,
          -- this cannot happen, because we are getting the /updated/ ledger
          -- view, and that update would have failed in these cases.
          error "applyChainState: impossible updated ledger view"
    where
      mapExcept :: (e -> e') -> (a -> b) -> Except e a -> Except e' b
      mapExcept f g = withExcept f . fmap g

  -- Rewind chain state

  rewindChainState (HardForkCfg cfgP1 cfgP2) cstate mSlotNo =
    case cstate of
      -- Before the fork. Delegate to P1
      BeforeFork cstateP1 -> fmap BeforeFork $
        rewindChainState cfgP1 cstateP1 mSlotNo

      -- After the fork, rollback point /also/ after the fork
      AfterFork afcs
        | at (afcsSlotNo afcs) < mSlotNo -> fmap AfterFork $
            AFCS.update afcs $ \cstateP2 ->
              rewindChainState cfgP2 cstateP2 mSlotNo

      -- After the fork, roll back to /before/ the fork
        | otherwise -> fmap BeforeFork $ do
            cstateP1 <- afcsSnapshot afcs
            rewindChainState cfgP1 cstateP1 mSlotNo
