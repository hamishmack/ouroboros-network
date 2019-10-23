{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Protocol.HardFork.CanHardFork.MockPBftToPraos () where

import           Ouroboros.Consensus.Ledger.Mock
import qualified Ouroboros.Consensus.Ledger.Mock.Stake as Stake
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.HardFork.CanHardFork
import           Ouroboros.Consensus.Protocol.HardFork.Config
import           Ouroboros.Consensus.Protocol.PBFT hiding (pbftParams)
import           Ouroboros.Consensus.Protocol.Praos
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF

{-------------------------------------------------------------------------------
  Simple Hard Fork Instance
-------------------------------------------------------------------------------}

type MockPBft  = ExtNodeConfig (PBftLedgerView PBftMockCrypto) (PBft PBftMockCrypto)
type MockPraos = ExtNodeConfig AddrDist (Praos PraosMockCrypto)

instance CanHardFork MockPBft MockPraos where

  shouldFork _ _ _ = False -- TODO

  chainStateAfterFork _ _ = []

  ledgerViewAfterFork cfg _ = Stake.equalStakeDist addrDist
    where
      addrDist :: AddrDist
      addrDist = encNodeConfigExt (nodeConfigAfterFork cfg)

  -- We just use the defaults from OuroborosTag for chain selection

  hardForkPreferCandidate _ ours cand = AF.compareHeadBlockNo cand ours == GT
  hardForkCompareCandidates _ = AF.compareHeadBlockNo
