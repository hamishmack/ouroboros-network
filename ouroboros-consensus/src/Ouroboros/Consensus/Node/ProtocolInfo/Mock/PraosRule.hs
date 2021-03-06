-- | Test the Praos chain selection rule but with explicit leader schedule
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.PraosRule (
    protocolInfoPraosRule
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.Praos

protocolInfoPraosRule :: NumCoreNodes
                      -> CoreNodeId
                      -> PraosParams
                      -> LeaderSchedule
                      -> ProtocolInfo (SimplePraosRuleBlock SimpleMockCrypto)
protocolInfoPraosRule (NumCoreNodes numCoreNodes)
                      (CoreNodeId nid)
                      params
                      schedule =
    ProtocolInfo {
      pInfoConfig    = WLSNodeConfig
        { lsNodeConfigSchedule = schedule
        , lsNodeConfigP        = PraosNodeConfig
            { praosParams       = params
            , praosNodeId       = CoreId nid
            , praosSignKeyVRF   = NeverUsedSignKeyVRF
            , praosInitialEta   = 0
            , praosInitialStake = genesisStakeDist addrDist
            , praosVerKeys      = verKeys
            , praosExtConfig    = ()
            }
        , lsNodeConfigNodeId   = CoreNodeId nid
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState         = genesisSimpleLedgerState addrDist
        , ouroborosChainState = ()
        }
    , pInfoInitState  = ()
    }
  where
    addrDist = mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = IntMap.fromList [ (nd, (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                              | nd <- [0 .. numCoreNodes - 1]
                              ]
