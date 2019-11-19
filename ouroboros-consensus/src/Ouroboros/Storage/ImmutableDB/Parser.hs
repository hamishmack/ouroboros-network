{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Ouroboros.Storage.ImmutableDB.Parser
  ( -- * EpochFileParser
    EpochFileError (..)
  , epochFileParser
  , epochFileParser'
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (($>))
import           Data.Word (Word64)

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import qualified Ouroboros.Consensus.Util.CBOR as Util.CBOR
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API (HasFS)

import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary (CRC)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Types

-- TODO move to Impl

-- TODO check for multiple EBBs in one epoch? If the hashes line up, this
-- should be impossible

{-------------------------------------------------------------------------------
  EpochFileParser
-------------------------------------------------------------------------------}

data EpochFileError hash =
    EpochErrRead Util.CBOR.ReadIncrementalErr

    -- | The previous hash of a block did not match the hash of the previous
    -- block.
  | EpochErrHashMismatch
      (WithOrigin hash)  -- ^ The hash of the previous block
      (WithOrigin hash)  -- ^ The previous hash of the block
  deriving (Eq, Show)

type BlockInfo hash = BinaryInfo (hash, WithOrigin hash, CRC, BlockOrEBB)

epochFileParser'
  :: forall m blk hash h. (IOLike m, Eq hash)
  => (blk -> SlotNo)
  -> (blk -> hash)
  -> (blk -> WithOrigin hash)  -- ^ Previous hash
  -> HasFS m h
  -> (forall s. Decoder s (BL.ByteString -> blk))
  -> (blk -> Maybe EpochNo)    -- ^ If an EBB, return the epoch number
  -> (blk -> BinaryInfo ())
  -> EpochFileParser
       (EpochFileError hash)
       m
       (Secondary.Entry hash, WithOrigin hash)
epochFileParser' getSlotNo getHash getPrevHash
                 hasFS decodeBlock isEBB getBinaryInfo = EpochFileParser $
      fmap (checkIfHashesLineUp . first (fmap extractEntry))
    . Util.CBOR.readIncrementalOffsets hasFS decoder
  where
    decoder :: forall s. Decoder s (BL.ByteString -> BlockInfo hash)
    decoder = (extractBlockInfo .) <$> decodeBlock

    -- | It is important that we don't first parse all blocks, storing them
    -- all in memory, and only /then/ extract the information we need. So
    -- make sure we don't create thunks refering to the whole block.
    extractBlockInfo :: blk -> BlockInfo hash
    extractBlockInfo blk =
        -- TODO CRC
        getBinaryInfo blk $> (hash, prevHash, Secondary.CRC, blockOrEBB)
      where
        !hash       = getHash blk
        !prevHash   = getPrevHash blk
        !blockOrEBB = case isEBB blk of
          Just epoch -> EBB epoch
          Nothing    -> Block (getSlotNo blk)

    checkIfHashesLineUp
      :: ([(Secondary.Entry hash, WithOrigin hash)],
          Maybe (Util.CBOR.ReadIncrementalErr, Word64))
      -> ([(Secondary.Entry hash, WithOrigin hash)],
          Maybe (EpochFileError hash, Word64))
    checkIfHashesLineUp (es, mbErr) = case es of
        []               -> ([], first EpochErrRead <$> mbErr)
        e@(entry, _):es' ->
          first (e:) $ go (At (Secondary.headerHash entry)) [] es'
      where
        -- TODO can we does this lazily with constant memory usage and without
        -- an accumulator?
        go hashOfPrevBlock acc = \case
          [] -> (reverse acc, first EpochErrRead <$> mbErr)
          e@(entry, prevHash):es'
            | prevHash == hashOfPrevBlock
            -> go (At (Secondary.headerHash entry)) (e:acc) es'
            | otherwise
            -> let err = EpochErrHashMismatch hashOfPrevBlock prevHash
                   offset = Secondary.unBlockOffset $ Secondary.blockOffset entry
               in (reverse acc, Just (err, offset))

    extractEntry
      :: (Word64, (Word64, BlockInfo hash))
      -> (Secondary.Entry hash, WithOrigin hash)
    extractEntry (offset, (_size, blockInfo)) = (entry, prevHash)
      where
        BinaryInfo
          { binaryBlob = (headerHash, prevHash, checksum, blockOrEBB)
          , headerOffset
          , headerSize
          } = blockInfo
        entry = Secondary.Entry
          { blockOffset  = Secondary.BlockOffset  offset
          , headerOffset = Secondary.HeaderOffset headerOffset
          , headerSize   = Secondary.HeaderSize   headerSize
          , checksum
          , headerHash
          , blockOrEBB
          }

-- | A version of 'epochFileParser'' for blocks that implement 'HasHeader'.
epochFileParser
  :: forall m blk h. (IOLike m, HasHeader blk)
  => HasFS m h
  -> (forall s. Decoder s (BL.ByteString -> blk))
  -> (blk -> Maybe EpochNo)  -- ^ If an EBB, return the epoch number
  -> (blk -> BinaryInfo ())
  -> EpochFileParser
       (EpochFileError (HeaderHash blk))
       m
       (Secondary.Entry (HeaderHash blk), WithOrigin (HeaderHash blk))
epochFileParser =
    epochFileParser' blockSlot blockHash (convertPrevHash . blockPrevHash)
  where
    convertPrevHash :: ChainHash blk -> WithOrigin (HeaderHash blk)
    convertPrevHash GenesisHash   = Origin
    convertPrevHash (BlockHash h) = At h
