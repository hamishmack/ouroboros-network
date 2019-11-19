{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Ouroboros.Consensus.Ledger.Shelley
  ()
where

import           BlockChain (BHBody (..), BHeader (..), Block (..), HashHeader,
                     bhHash, bhbody, bheader)
import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Ledger.Shelley.API
import           Cardano.Prelude (NoUnexpectedThunks (..), withExceptT)
import           Control.State.Transition
import           Data.Coerce (coerce)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable, typeRep)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block
import           Slot (BlockNo (..))
import qualified STS.Bbody as STS
import qualified STS.Bhead as STS

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash { unShelleyHash :: HashHeader TPraosStandardCrypto }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR) -- TODO , FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Condense ShelleyHash where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
newtype ShelleyBlock = ShelleyBlock
  { unShelleyBlock :: Block TPraosStandardCrypto
  }
  deriving (Eq, Show)

instance GetHeader ShelleyBlock where
  data Header ShelleyBlock = ShelleyHeader
    { shelleyHeader :: !(BHeader TPraosStandardCrypto)
      -- Cached hash
    , shelleyHeaderHash :: ShelleyHash
    } deriving (Eq, Show)

  getHeader (ShelleyBlock b) = ShelleyHeader
    { shelleyHeader     = bheader b
    , shelleyHeaderHash = ShelleyHash . bhHash . bheader $ b
    }

instance NoUnexpectedThunks (Header ShelleyBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ShelleyBlock))

-- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ShelleyHeader hdr _hash) =
    noUnexpectedThunks ctxt hdr

instance SupportedBlock ShelleyBlock

type instance HeaderHash ShelleyBlock = ShelleyHash

instance HasHeader ShelleyBlock  where
  blockHash      = blockHash . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      = blockSlot . getHeader
  blockNo        = blockNo . getHeader
  blockInvariant = const True

instance HasHeader (Header ShelleyBlock) where
  blockHash = shelleyHeaderHash

  blockPrevHash =
    BlockHash . ShelleyHash . bheaderPrev . bhbody . shelleyHeader

  blockSlot      = convertSlotNo . bheaderSlot . bhbody . shelleyHeader
  blockNo        = coerce . bheaderBlockNo . bhbody . shelleyHeader
  blockInvariant = const True

instance Measured BlockMeasure ShelleyBlock where
  measure = blockMeasure

instance StandardHash ShelleyBlock

{-------------------------------------------------------------------------------
  Ledger
-------------------------------------------------------------------------------}

data CombinedLedgerError =
    BHeadError (HeaderTransitionError TPraosStandardCrypto)
  | BBodyError (BlockTransitionError TPraosStandardCrypto)
  deriving (Eq, Show)

instance UpdateLedger ShelleyBlock where

  newtype LedgerState ShelleyBlock
    = ShelleyLedgerState (ShelleyState TPraosStandardCrypto)
    deriving (Eq, Show, Generic)

  type LedgerError ShelleyBlock = CombinedLedgerError

  -- TODO what config is needed for Shelley?
  newtype LedgerConfig ShelleyBlock = ShelleyLedgerConfig ()

-- TODO extract the needed node config
  ledgerConfigView = const $ ShelleyLedgerConfig ()

  applyLedgerBlock _cfg (ShelleyBlock blk) (ShelleyLedgerState ss) = do
    ss' <- withExceptT BHeadError
            $ applyHeaderTransition ss (bheader blk)
    ss'' <- withExceptT BBodyError
            $ applyBlockTransition ss' blk
    return $ ShelleyLedgerState ss''

-- TODO a correct instance here
instance NoUnexpectedThunks (LedgerState ShelleyBlock)

{-------------------------------------------------------------------------------
  Support for Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol ShelleyBlock = TPraos TPraosStandardCrypto

instance SignedHeader (Header ShelleyBlock) where
  type Signed (Header ShelleyBlock) = BHBody TPraosStandardCrypto
  headerSigned = bhbody . shelleyHeader

instance HeaderSupportsTPraos TPraosStandardCrypto (Header ShelleyBlock) where
  headerToBHeader _ (ShelleyHeader hdr _hash) = hdr

instance ProtocolLedgerView ShelleyBlock where

  protocolLedgerView _ (ShelleyLedgerState ss) = currentLedgerView ss

  -- | We can assume that the ledger view is valid for the epoch in which it is
  -- produced.
  anachronisticProtocolLedgerView
    _cfg
    (ShelleyLedgerState ss)
    (convertSlotWithOrigin -> slot)
    = futureLedgerView ss slot
