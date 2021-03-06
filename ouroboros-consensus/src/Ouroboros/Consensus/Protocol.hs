{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module Ouroboros.Consensus.Protocol (
    -- * Supported protocols
    ProtocolMockBFT
  , ProtocolMockPraos
  , ProtocolLeaderSchedule
  , ProtocolMockPBFT
  , ProtocolRealPBFT
    -- * Abstract over the various protocols
  , Protocol(..)
    -- * Evidence that we can run all the supported protocols
  , runProtocol
  , module X
  ) where

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract (NumCoreNodes)
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT ()
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos ()
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.BFT as X
import           Ouroboros.Consensus.Protocol.LeaderSchedule as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Protocol.Praos as X
import           Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  Supported protocols
-------------------------------------------------------------------------------}

type ProtocolMockBFT        = Bft BftMockCrypto
type ProtocolMockPraos      = Praos AddrDist PraosMockCrypto
type ProtocolLeaderSchedule = WithLeaderSchedule (Praos () PraosCryptoUnused)
type ProtocolMockPBFT       = PBft (PBftLedgerView PBftMockCrypto) PBftMockCrypto
type ProtocolRealPBFT       = PBft ByronConfig PBftCardanoCrypto

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol blk where
  -- | Run BFT against the mock ledger
  ProtocolMockBFT
    :: NumCoreNodes
    -> CoreNodeId
    -> SecurityParam
    -> Protocol (SimpleBftBlock SimpleMockCrypto BftMockCrypto)

  -- | Run Praos against the mock ledger
  ProtocolMockPraos
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> Protocol (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  ProtocolLeaderSchedule
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> LeaderSchedule
    -> Protocol (SimplePraosRuleBlock SimpleMockCrypto)

  -- | Run PBFT against the mock ledger
  ProtocolMockPBFT
    :: NumCoreNodes
    -> CoreNodeId
    -> PBftParams
    -> Protocol (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)

  -- | Run PBFT against the real ledger
  ProtocolRealPBFT
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
    -> Protocol ByronBlock

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol blk -> Dict (RunNode blk)
runProtocol ProtocolMockBFT{}        = Dict
runProtocol ProtocolMockPraos{}      = Dict
runProtocol ProtocolLeaderSchedule{} = Dict
runProtocol ProtocolMockPBFT{}       = Dict
runProtocol ProtocolRealPBFT{}       = Dict
