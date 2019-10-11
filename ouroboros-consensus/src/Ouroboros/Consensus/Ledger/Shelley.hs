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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Shelley
  ()
where

import           BaseTypes                      ( Nonce )
import           BlockChain                     ( Block(..)
                                                , BHeader(..)
                                                , BHBody(..)
                                                , HashHeader
                                                , bheader
                                                , bhHash
                                                , bhbody
                                                )
import           Slot                           ( BlockNo(..) )
import           Cardano.Binary                 ( ToCBOR(..)
                                                , FromCBOR(..)
                                                )
import           Cardano.Prelude                ( NoUnexpectedThunks(..) )
import           Control.State.Transition
import           Data.Coerce                    ( coerce )
import           Data.FingerTree.Strict         ( Measured(..) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                )
import           GHC.Generics                   ( Generic )
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block
import qualified STS.Bhead                     as STS
import qualified STS.Bbody                     as STS

{-------------------------------------------------------------------------------
  Crypto Aliases
-------------------------------------------------------------------------------}

type DSIGN = TPraosDSIGN TPraosStandardCrypto
type KES = TPraosKES TPraosStandardCrypto
type VRF = TPraosVRF TPraosStandardCrypto
-- Capitalised for consistency and to avoid conflict with other `Hash` types.
type HASH = TPraosHash TPraosStandardCrypto

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash { unShelleyHash :: HashHeader HASH DSIGN KES VRF }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR) -- TODO , FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Condense ShelleyHash where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
newtype ShelleyBlock = ShelleyBlock
  { unShelleyBlock :: Block HASH DSIGN KES VRF
  }
  deriving (Eq, Show)

instance GetHeader ShelleyBlock where
  data Header ShelleyBlock = ShelleyHeader
    { shelleyHeader :: !(BHeader HASH DSIGN KES VRF)
      -- Cached hash
    , shelleyHeaderHash :: ShelleyHash
    } deriving (Eq, Show)

  getHeader (ShelleyBlock b) = ShelleyHeader
    { shelleyHeader     = bheader b
    , shelleyHeaderHash = ShelleyHash . bhHash . bheader $ b
    }

instance NoUnexpectedThunks (Header ShelleyBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ShelleyBlock ))

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

-- | See note on @CombinedLedgerState@
data CombinedLedgerError =
    BHeadError (PredicateFailure (STS.BHEAD HASH DSIGN KES VRF))
  | BBodyError (PredicateFailure (STS.BBODY HASH DSIGN KES VRF))
  deriving (Eq, Show)

instance UpdateLedger ShelleyBlock where

  data LedgerState ShelleyBlock = ShelleyLedgerState
    { shelleyLedgerState :: CombinedLedgerState

    } deriving (Eq, Show, Generic)
  type LedgerError ShelleyBlock = CombinedLedgerError

  -- TODO what config is needed for Shelley?
  newtype LedgerConfig ShelleyBlock = ShelleyLedgerConfig ()

-- TODO extract the needed node config
  ledgerConfigView = undefined

  applyChainTick (ShelleyLedgerConfig _) slotNo (ShelleyLedgerState (CombinedLedgerState bhState _ _))
    = applySTS @STS.BHEAD $ TRC (env, bhState, slotNo)

{-------------------------------------------------------------------------------
  Support for Praos consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol ShelleyBlock
  = TPraos TPraosStandardCrypto

instance SignedHeader (Header ShelleyBlock) where
  type Signed (Header ShelleyBlock) = BHBody HASH DSIGN KES VRF
  headerSigned _ = bhbody . shelleyHeader

instance HeaderSupportsTPraos TPraosStandardCrypto (Header ShelleyBlock) where

  headerToBHeader _ (ShelleyHeader hdr _hash) = hdr
