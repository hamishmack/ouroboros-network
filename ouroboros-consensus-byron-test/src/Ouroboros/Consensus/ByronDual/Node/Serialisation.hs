{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.ByronDual.Node.Serialisation () where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.HeaderValidation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Dual
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Serialisation
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node.Serialisation ()
import           Ouroboros.Consensus.Byron.Protocol

import           Ouroboros.Consensus.ByronSpec.Ledger

import           Ouroboros.Consensus.ByronDual.Ledger

{-------------------------------------------------------------------------------
  HasNetworkProtocolVersion
-------------------------------------------------------------------------------}

pb :: Proxy ByronBlock
pb = Proxy

instance HasNetworkProtocolVersion DualByronBlock where
  type NodeToNodeVersion   DualByronBlock = NodeToNodeVersion   ByronBlock
  type NodeToClientVersion DualByronBlock = NodeToClientVersion ByronBlock

  supportedNodeToNodeVersions   _ = supportedNodeToNodeVersions   pb
  supportedNodeToClientVersions _ = supportedNodeToClientVersions pb
  mostRecentNodeToNodeVersion   _ = mostRecentNodeToNodeVersion   pb
  mostRecentNodeToClientVersion _ = mostRecentNodeToClientVersion pb
  nodeToNodeProtocolVersion     _ = nodeToNodeProtocolVersion     pb
  nodeToClientProtocolVersion   _ = nodeToClientProtocolVersion   pb

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance ImmDbSerialiseConstraints DualByronBlock
instance LgrDbSerialiseConstraints DualByronBlock
instance VolDbSerialiseConstraints DualByronBlock
instance SerialiseDiskConstraints  DualByronBlock

instance EncodeDisk DualByronBlock DualByronBlock where
  encodeDisk _ = encodeDualBlock encodeByronBlock
instance DecodeDisk DualByronBlock (Lazy.ByteString -> DualByronBlock) where
  decodeDisk ccfg = decodeDualBlock (decodeByronBlock epochSlots)
    where
      epochSlots = extractEpochSlots ccfg

instance EncodeDisk DualByronBlock (Header DualByronBlock) where
  encodeDisk ccfg = encodeDisk (dualCodecConfigMain ccfg) . dualHeaderMain
instance DecodeDisk DualByronBlock (Header DualByronBlock) where
  decodeDisk ccfg = DualHeader <$> decodeDisk (dualCodecConfigMain ccfg)
instance DecodeDisk DualByronBlock (Lazy.ByteString -> Header DualByronBlock) where
  decodeDisk ccfg = const <$> decodeDisk ccfg

instance EncodeDisk DualByronBlock (LedgerState DualByronBlock) where
  encodeDisk _ = encodeDualLedgerState encodeByronLedgerState
instance DecodeDisk DualByronBlock (LedgerState DualByronBlock) where
  decodeDisk _ = decodeDualLedgerState decodeByronLedgerState

-- | @'ConsensusState' ('BlockProtocol' 'DualByronBlock')@
instance EncodeDisk DualByronBlock (PBftState PBftByronCrypto) where
  encodeDisk _ = encodeByronConsensusState
-- | @'ConsensusState' ('BlockProtocol' 'DualByronBlock')@
instance DecodeDisk DualByronBlock (PBftState PBftByronCrypto) where
  decodeDisk ccfg = decodeByronConsensusState k
    where
      k = getByronSecurityParam $ dualCodecConfigMain ccfg

instance EncodeDisk DualByronBlock (AnnTip DualByronBlock) where
  encodeDisk ccfg = encodeDisk (dualCodecConfigMain ccfg)
                  . (castAnnTip :: AnnTip DualByronBlock -> AnnTip ByronBlock)
instance DecodeDisk DualByronBlock (AnnTip DualByronBlock) where
  decodeDisk ccfg = (castAnnTip :: AnnTip ByronBlock -> AnnTip DualByronBlock)
                <$> decodeDisk (dualCodecConfigMain ccfg)

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNodeConstraints DualByronBlock

-- | @'HeaderHash' 'DualByronBlock'@
instance SerialiseNodeToNode DualByronBlock ByronHash where
  encodeNodeToNode _ _ = encodeByronHeaderHash
  decodeNodeToNode _ _ = decodeByronHeaderHash

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode DualByronBlock DualByronBlock where
  encodeNodeToNode _    _ = wrapCBORinCBOR   (encodeDualBlock  encodeByronBlock)
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeDualBlock (decodeByronBlock epochSlots))
    where
      epochSlots = extractEpochSlots ccfg

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode DualByronBlock (Serialised DualByronBlock)
  -- Default instance

-- | Forward to the Byron instance.
instance SerialiseNodeToNode DualByronBlock (Header DualByronBlock) where
  encodeNodeToNode ccfg version =
        encodeNodeToNode (dualCodecConfigMain ccfg) version
      . dualHeaderMain
  decodeNodeToNode ccfg version =
          DualHeader
      <$> decodeNodeToNode (dualCodecConfigMain ccfg) version

-- | Forward to the Byron instance.
instance SerialiseNodeToNode DualByronBlock (Serialised (Header DualByronBlock)) where
  encodeNodeToNode ccfg version =
        encodeNodeToNode (dualCodecConfigMain ccfg) version
      . dualWrappedMain
  decodeNodeToNode ccfg version =
          rewrapMain
      <$> decodeNodeToNode (dualCodecConfigMain ccfg) version

instance SerialiseNodeToNode DualByronBlock (GenTx DualByronBlock) where
  encodeNodeToNode _ _ = encodeDualGenTx encodeByronGenTx
  decodeNodeToNode _ _ = decodeDualGenTx decodeByronGenTx

instance SerialiseNodeToNode DualByronBlock (GenTxId DualByronBlock) where
  encodeNodeToNode _ _ = encodeDualGenTxId encodeByronGenTxId
  decodeNodeToNode _ _ = decodeDualGenTxId decodeByronGenTxId

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClientConstraints DualByronBlock

-- | @'HeaderHash' 'DualByronBlock'@
instance SerialiseNodeToClient DualByronBlock ByronHash where
  encodeNodeToClient _ _ = encodeByronHeaderHash
  decodeNodeToClient _ _ = decodeByronHeaderHash

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient DualByronBlock DualByronBlock where
  encodeNodeToClient _    _ = wrapCBORinCBOR   (encodeDualBlock  encodeByronBlock)
  decodeNodeToClient ccfg _ = unwrapCBORinCBOR (decodeDualBlock (decodeByronBlock epochSlots))
    where
      epochSlots = extractEpochSlots ccfg

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient DualByronBlock (Serialised DualByronBlock)
  -- Default instance

instance SerialiseNodeToClient DualByronBlock (GenTx DualByronBlock) where
  encodeNodeToClient _ _ = encodeDualGenTx encodeByronGenTx
  decodeNodeToClient _ _ = decodeDualGenTx decodeByronGenTx

-- | @'ApplyTxErr' 'DualByronBlock'@
instance SerialiseNodeToClient DualByronBlock (DualGenTxErr ByronBlock ByronSpecBlock) where
  encodeNodeToClient _ _ = encodeDualGenTxErr encodeByronApplyTxError
  decodeNodeToClient _ _ = decodeDualGenTxErr decodeByronApplyTxError

instance SerialiseNodeToClient DualByronBlock (Some (Query DualByronBlock)) where
  encodeNodeToClient _ _ (Some q) = case q of {}
  decodeNodeToClient _ _          = error "DualByron: no query to decode"

instance SerialiseResult DualByronBlock (Query DualByronBlock) where
  encodeResult _ _ = \case {}
  decodeResult _ _ = \case {}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

extractEpochSlots :: CodecConfig DualByronBlock -> EpochSlots
extractEpochSlots = getByronEpochSlots . dualCodecConfigMain

-- | The headers for 'DualByronBlock' and 'ByronBlock' are identical, so we
-- can safely cast the serialised forms.
dualWrappedMain :: Serialised (Header DualByronBlock)
                -> Serialised (Header ByronBlock)
dualWrappedMain (Serialised bs) = Serialised bs

-- | Inverse of 'dualWrappedMain'.
rewrapMain :: Serialised (Header ByronBlock)
           -> Serialised (Header DualByronBlock)
rewrapMain (Serialised bs) = Serialised bs