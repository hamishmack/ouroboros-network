{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Ledger.Byron.Block (
    -- * Hash
    ByronHash(..)
  , mkByronHash
    -- * Block
  , ByronBlock(..)
  , mkByronBlock
  , annotateByronBlock
    -- * Header
  , Header(..)
  , mkByronHeader
  , byronBlockMatchesHeader
    -- * Serialisation
  , encodeByronBlock
  , decodeByronBlock
  , encodeByronHeader
  , decodeByronHeader
  , encodeByronHeaderHash
  , decodeByronHeaderHash
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR

import           Cardano.Binary
import           Cardano.Prelude (NoUnexpectedThunks (..))

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Byron.Aux
import           Ouroboros.Consensus.Ledger.Byron.Conversions
import           Ouroboros.Consensus.Ledger.Byron.Orphans ()
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ByronHash = ByronHash { unByronHash :: CC.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR, Condense)
  deriving anyclass NoUnexpectedThunks

mkByronHash :: ABlockOrBoundaryHdr ByteString -> ByronHash
mkByronHash = ByronHash . abobHdrHash

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Byron block
--
-- We cache two bits of information:
--
-- * We cache the slot number as this is not readily available for EBBs.
--   Having it cached allows us to e.g. give a 'HasHeader' instance.
-- * We cache the hash as this is expensive to compute and we need it often.
data ByronBlock = ByronBlock {
      byronBlockRaw    :: !(CC.ABlockOrBoundary ByteString)
    , byronBlockSlotNo :: !SlotNo
    , byronBlockHash   :: !ByronHash
    }
  deriving (Eq, Show)

instance Condense ByronBlock where
  condense = condense . byronBlockRaw

mkByronBlock :: CC.EpochSlots -> CC.ABlockOrBoundary ByteString -> ByronBlock
mkByronBlock epochSlots blk = ByronBlock {
      byronBlockRaw    = blk
    , byronBlockSlotNo = fromByronSlotNo $ abobHdrSlotNo epochSlots hdr
    , byronBlockHash   = mkByronHash hdr
    }
  where
    hdr = abobHdrFromBlock blk

-- | Construct Byron block from unannotated 'CC.Block'
--
-- This should be used only when forging blocks (not when receiving blocks
-- over the wire).
annotateByronBlock :: CC.EpochSlots -> CC.Block -> ByronBlock
annotateByronBlock es = mkByronBlock es . CC.ABOBBlock . reAnnotateBlock es

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

instance GetHeader ByronBlock where
  -- | Byron header
  --
  -- See 'ByronBlock' for comments on why we cache certain values.
  data Header ByronBlock = ByronHeader {
        byronHeaderRaw    :: !(ABlockOrBoundaryHdr ByteString)
      , byronHeaderSlotNo :: !SlotNo
      , byronHeaderHash   :: !ByronHash
      }
    deriving (Eq, Show, Generic)

  getHeader ByronBlock{..} = ByronHeader{
        byronHeaderRaw    = abobHdrFromBlock byronBlockRaw
      , byronHeaderSlotNo = byronBlockSlotNo
      , byronHeaderHash   = byronBlockHash
      }

instance Condense (Header ByronBlock) where
  condense = aBlockOrBoundaryHdr condense condense . byronHeaderRaw

instance NoUnexpectedThunks (Header ByronBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ByronBlock))

mkByronHeader :: CC.EpochSlots
              -> ABlockOrBoundaryHdr ByteString
              -> Header ByronBlock
mkByronHeader epochSlots hdr = ByronHeader {
      byronHeaderRaw    = hdr
    , byronHeaderSlotNo = fromByronSlotNo $ abobHdrSlotNo epochSlots hdr
    , byronHeaderHash   = mkByronHash hdr
    }

-- | Check if a block matches its header
byronBlockMatchesHeader :: Header ByronBlock -> ByronBlock -> Bool
byronBlockMatchesHeader hdr blk =
    abobMatchesBody (byronHeaderRaw hdr) (byronBlockRaw blk)

{-------------------------------------------------------------------------------
  HasHeader instances

  This doesn't do much more than pass to the instance for headers.
-------------------------------------------------------------------------------}

type instance HeaderHash ByronBlock = ByronHash
instance StandardHash ByronBlock

instance HasHeader ByronBlock where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance HasHeader (Header ByronBlock) where
  blockHash      = byronHeaderHash
  blockSlot      = byronHeaderSlotNo
  blockPrevHash  = fromByronPrevHash' . abobHdrPrevHash        . byronHeaderRaw
  blockNo        = fromByronBlockNo   . abobHdrChainDifficulty . byronHeaderRaw
  blockInvariant = const True

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

fromByronPrevHash' :: Maybe CC.HeaderHash -> ChainHash (Header ByronBlock)
fromByronPrevHash' = fromByronPrevHash ByronHash

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronHeaderHash :: HeaderHash ByronBlock -> Encoding
encodeByronHeaderHash = toCBOR

decodeByronHeaderHash :: Decoder s (HeaderHash ByronBlock)
decodeByronHeaderHash = fromCBOR

-- | Encode a block
--
-- Should be backwards compatible with legacy (cardano-sl) nodes.
--
-- Implementation note: the decoder uses 'CC.fromCBORABlockOrBoundary', which
-- has inverse 'CC.toCBORABlockOrBoundary'. This encoder is intended to be
-- binary compatible with 'CC.toCBORABlockOrBoundary', but does not use it and
-- instead takes advantage of the annotations (using 'encodePreEncoded').
encodeByronBlock :: ByronBlock -> Encoding
encodeByronBlock blk = mconcat [
      CBOR.encodeListLen 2
    , case byronBlockRaw blk of
        CC.ABOBBoundary b -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryAnnotation b
          ]
        CC.ABOBBlock b -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.blockAnnotation b
          ]
    ]

-- | Inverse of 'encodeByronBlock'
decodeByronBlock :: CC.EpochSlots -> Decoder s (Lazy.ByteString -> ByronBlock)
decodeByronBlock epochSlots =
    fillInByteString <$> CC.fromCBORABlockOrBoundary epochSlots
  where
    fillInByteString :: CC.ABlockOrBoundary ByteSpan
                     -> Lazy.ByteString
                     -> ByronBlock
    fillInByteString it theBytes = mkByronBlock epochSlots $
      Lazy.toStrict . slice theBytes <$> it

-- | Encode a header
--
-- Should be backwards compatible with legacy (cardano-sl) nodes.
--
-- This function should be inverse to 'decodeByronHeader'
-- (which uses 'fromCBORABlockOrBoundaryHdr').
encodeByronHeader :: Header ByronBlock -> Encoding
encodeByronHeader hdr = mconcat [
      CBOR.encodeListLen 2
    , case byronHeaderRaw hdr of
        ABOBBoundaryHdr h -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryHeaderAnnotation h
          ]
        ABOBBlockHdr h -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.headerAnnotation h
          ]
    ]

-- | Inverse of 'encodeByronHeader'
decodeByronHeader :: CC.EpochSlots
                  -> Decoder s (Lazy.ByteString -> Header ByronBlock)
decodeByronHeader epochSlots =
    fillInByteString <$> fromCBORABlockOrBoundaryHdr epochSlots
  where
    fillInByteString :: ABlockOrBoundaryHdr ByteSpan
                     -> Lazy.ByteString
                     -> Header ByronBlock
    fillInByteString it theBytes = mkByronHeader epochSlots $
      Lazy.toStrict . slice theBytes <$> it
