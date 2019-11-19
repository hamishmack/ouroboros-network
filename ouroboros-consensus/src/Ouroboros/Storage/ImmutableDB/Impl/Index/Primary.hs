{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Primary Index
--
-- Intended for qualified import
-- > import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as PrimaryIndex
module Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
  ( -- * SecondaryOffset
    SecondaryOffset
    -- * PrimaryIndex
  , PrimaryIndex
  , currentVersionNumber
  , slots
  , secondaryOffsetSize
  , readOffset
  , readFirstFilledSlot
  , load
  , write
  , truncateToSlot
  , truncateToSlotFS
  , unfinalise
  , open
  , appendOffsets
  , lastOffset
  , lastSlot
  , containsSlot
  , offsetOfSlot
  , sizeOfSlot
  , isFilledSlot
  , nextFilledSlot
  , firstFilledSlot
  , filledSlots
  , lastFilledSlot
  , isPrefixOf
  , extendWithTrailingUnfilledSlotsFrom
  , backfill
    -- * Exported for testing purposes
  , mk
  , toSecondaryOffsets
  ) where

import           Control.Exception (assert)
import           Control.Monad (void, when)
import           Data.Binary (Get, Put)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Foreign.Storable (sizeOf)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Control.Monad.Class.MonadThrow hiding (onException)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Storage.Common (EpochNo, EpochSize)
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types (AbsOffset (..),
                     AllowExisting (..), OpenMode (..))
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..),
                     onException)

import           Ouroboros.Storage.ImmutableDB.Impl.Util (renderFile, runGet)
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Types (ImmutableDBError (..))

{------------------------------------------------------------------------------
  SecondaryOffset
------------------------------------------------------------------------------}

-- | TODO explain why Word32 is enough
type SecondaryOffset = Word32

getSecondaryOffset :: Get SecondaryOffset
getSecondaryOffset = Get.getWord32be

putSecondaryOffset :: SecondaryOffset -> Put
putSecondaryOffset = Put.putWord32be

-- | The size of each entry in the primary index file, i.e., the size of a
-- 'SecondaryOffset'.
secondaryOffsetSize :: Word64
secondaryOffsetSize = fromIntegral $ sizeOf (error "sizeOf" :: SecondaryOffset)
{-# INLINE secondaryOffsetSize #-}

{------------------------------------------------------------------------------
  PrimaryIndex
------------------------------------------------------------------------------}

-- | In-memory representation of the index file.
newtype PrimaryIndex = MkPrimaryIndex {
      getOffsets :: Vector SecondaryOffset
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Smart constructor: checks that the offsets are non-decreasing, there is
-- at least one offset, and that the first offset is 0.
mk :: [SecondaryOffset] -> Maybe PrimaryIndex
mk offsets@(0:_)
    | and $ zipWith (<=) offsets (drop 1 offsets)
    = Just $ MkPrimaryIndex $ V.fromList offsets
mk _
    = Nothing

-- | Return the 'SecondaryOffset's in the 'PrimaryIndex'.
toSecondaryOffsets :: PrimaryIndex -> [SecondaryOffset]
toSecondaryOffsets = V.toList . getOffsets

-- | Version number of the index format
currentVersionNumber :: Word8
currentVersionNumber = 0

-- | Return the number of slots in the primary index (the number of offsets - 1).
--
-- Note that the primary index will typically contain a slot for the EBB, so
-- for an for an epoch with 10 regular slots, this will function will return
-- 11.
slots :: PrimaryIndex -> EpochSize
slots (MkPrimaryIndex offsets) = fromIntegral $ V.length offsets - 1

readOffset
  :: forall m h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochNo
  -> RelativeSlot
  -> m (Maybe SecondaryOffset)
       -- ^ The offset in the secondary index file corresponding to the given
       -- slot. 'Nothing' when the slot is empty.
readOffset hasFS err epoch (RelativeSlot slot) =
    withFile hasFS primaryIndexFile ReadMode $ \pHnd -> do
      (secondaryOffset, nextSecondaryOffset) <-
        runGet err primaryIndexFile get =<<
        hGetExactlyAt hasFS pHnd nbBytes offset
      return $ if nextSecondaryOffset - secondaryOffset > 0
        then Just secondaryOffset
        else Nothing
  where
    primaryIndexFile = renderFile "primary" epoch
    offset           = AbsOffset $
      fromIntegral (sizeOf currentVersionNumber) +
      slot * secondaryOffsetSize
    nbBytes          = secondaryOffsetSize * 2

    get :: Get (SecondaryOffset, SecondaryOffset)
    get = (,) <$> getSecondaryOffset <*> getSecondaryOffset

-- TODO avoid parsing the whole file, read incrementally
readFirstFilledSlot
  :: forall m h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochNo
  -> m (Maybe RelativeSlot)
readFirstFilledSlot hasFS err epoch =
  firstFilledSlot <$> load hasFS err epoch

-- | Load a primary index file in memory.
load
  :: forall m h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochNo
  -> m PrimaryIndex
load hasFS err epoch =
    withFile hasFS primaryIndexFile ReadMode $ \pHnd ->
      hGetAll hasFS pHnd >>= runGet err primaryIndexFile get
  where
    primaryIndexFile = renderFile "primary" epoch

    -- TODO incremental?
    get :: Get PrimaryIndex
    get = Get.getWord8 >>= \versionNumber ->
      if versionNumber == currentVersionNumber
        then MkPrimaryIndex . V.fromList <$> go
        else fail $ "unknown version number: " <> show versionNumber
      where
        go = do
          isEmpty <- Get.isEmpty
          if isEmpty then return []
          else (:) <$> getSecondaryOffset <*> go

-- | Write a primary index to a file.
--
-- Property: for @hasFS@, @err@, @epoch@
--
-- > 'write' hasFS epoch primaryIndex
-- > primaryIndex' <- 'load' hasFS err epoch
--
-- Then it must be that:
--
-- > primaryIndex === primaryIndex'
--
write
  :: MonadThrow m
  => HasFS m h
  -> EpochNo
  -> PrimaryIndex
  -> m ()
write hasFS@HasFS { hTruncate } epoch (MkPrimaryIndex offsets) =
    withFile hasFS primaryIndexFile (AppendMode AllowExisting) $ \pHnd -> do
      -- NOTE: open it in AppendMode and truncate it first, otherwise we might
      -- just overwrite part of the data stored in the index file.
      hTruncate pHnd 0
      void $ hPut hasFS pHnd $ Put.execPut $
        -- The version number
        Put.putWord8 currentVersionNumber <>
        -- Hopefully the intermediary list is fused away
        foldMap putSecondaryOffset (V.toList offsets)
  where
    primaryIndexFile = renderFile "primary" epoch

-- | Truncate the primary index so that the given 'RelativeSlot'. will be the
-- last slot (filled or not) in the primary index, unless the primary index
-- didn't contain the 'RelativeSlot' in the first place.
truncateToSlot :: RelativeSlot -> PrimaryIndex -> PrimaryIndex
truncateToSlot slot primary@(MkPrimaryIndex offsets)
    | lastSlot primary <= slot
    = primary
    | otherwise
    = MkPrimaryIndex (V.take (fromIntegral (unRelativeSlot slot) + 2) offsets)

-- | On-disk variant of 'truncateToSlot'. The truncation is done without
-- reading the primary index from disk.
truncateToSlotFS
  :: MonadThrow m
  => HasFS m h
  -> EpochNo
  -> RelativeSlot
  -> m ()
truncateToSlotFS hasFS@HasFS { hTruncate, hGetSize } epoch (RelativeSlot slot) =
    withFile hasFS primaryIndexFile (AppendMode AllowExisting) $ \pHnd -> do
      size <- hGetSize pHnd
      when (offset < size) $ hTruncate pHnd offset
  where
    primaryIndexFile = renderFile "primary" epoch
    offset           = fromIntegral (sizeOf currentVersionNumber)
                     + (slot + 2) * secondaryOffsetSize

-- | Remove all trailing empty slots that were added during the
-- finalisation/backfilling of the primary index.
--
-- POSTCONDITION: the last slot of the primary index file will be filled,
-- unless the index itself is empty.
unfinalise
  :: MonadThrow m
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> EpochNo
  -> m ()
unfinalise hasFS err epoch = do
    -- TODO optimise so that we only need to open the file once
    primaryIndex <- load hasFS err epoch
    case lastFilledSlot primaryIndex of
      Nothing   -> return ()
      Just slot -> truncateToSlotFS hasFS epoch slot

-- | Open a primary index file for the given epoch and return a handle to it.
--
-- The file is opened with the given 'AllowExisting' value. When given
-- 'MustBeNew', the version number is written to the file.
open
  :: (HasCallStack, Monad m)
  => HasFS m h
  -> EpochNo
  -> AllowExisting
  -> m (Handle h)
open hasFS@HasFS { hOpen, hClose, hasFsErr } epoch allowExisting  = do
    -- TODO we rely on the fact that if the file exists, it already contains
    -- the version number and the first offset. What if that is not the case?
    pHnd <- hOpen primaryIndexFile (AppendMode allowExisting)
    flip (onException hasFsErr) (hClose pHnd) $ do
      case allowExisting of
        AllowExisting -> return ()
        -- If the file is new, write the version number and the first offset,
        -- i.e. 0.
        MustBeNew     -> void $ hPut hasFS pHnd $ Put.execPut $
          Put.putWord8 currentVersionNumber <>
          putSecondaryOffset 0
      return pHnd
  where
    primaryIndexFile = renderFile "primary" epoch

-- | TODO
appendOffsets
  :: (Monad m, Foldable f)
  => HasFS m h
  -> Handle h
  -> f SecondaryOffset
  -> m ()
appendOffsets hasFS pHnd offsets =
    void $ hPut hasFS pHnd $ Put.execPut $ foldMap putSecondaryOffset offsets

-- | Return the last 'SecondaryOffset' in the primary index file.
lastOffset :: PrimaryIndex -> SecondaryOffset
lastOffset (MkPrimaryIndex offsets)
  | V.null offsets = 0
  | otherwise = offsets ! (V.length offsets - 1)

-- | Return the last slot of the primary index (empty or not).
lastSlot :: PrimaryIndex -> RelativeSlot
lastSlot (MkPrimaryIndex offsets) = fromIntegral (V.length offsets - 2)

-- | Check whether the given slot is within the primary index.
containsSlot :: PrimaryIndex -> RelativeSlot -> Bool
containsSlot (MkPrimaryIndex offsets) (RelativeSlot slot) =
  slot < fromIntegral (V.length offsets) - 1

-- | Return the offset for the given slot.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
offsetOfSlot :: HasCallStack => PrimaryIndex -> RelativeSlot -> SecondaryOffset
offsetOfSlot (MkPrimaryIndex offsets) (RelativeSlot slot) =
  offsets ! fromIntegral slot

-- | Return the size of the given slot according to the primary index.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
sizeOfSlot :: HasCallStack => PrimaryIndex -> RelativeSlot -> Word32
sizeOfSlot (MkPrimaryIndex offsets) (RelativeSlot slot) = offsetAfter - offsetAt
  where
    i           = fromIntegral slot
    offsetAt    = offsets ! i
    offsetAfter = offsets ! (i + 1)

-- | Return 'True' when the given slot is filled.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
isFilledSlot :: HasCallStack => PrimaryIndex -> RelativeSlot -> Bool
isFilledSlot primary slot = sizeOfSlot primary slot /= 0

-- | Find the next filled (length > zero) slot after the given slot in the
-- primary index. If there is none, return 'Nothing'.
--
-- Precondition: the given slot must be within the primary index
-- ('containsSlot').
--
-- Example: given the primary index below and slot 1:
--
-- > slot:     0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┬────┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13 │
-- >         └───┴───┴───┴───┴───┴────┘
--
-- Return slot 4.
nextFilledSlot :: PrimaryIndex -> RelativeSlot -> Maybe RelativeSlot
nextFilledSlot (MkPrimaryIndex offsets) (RelativeSlot slot) =
    go (fromIntegral slot + 1)
  where
    len = V.length offsets
    go i
      | i + 1 >= len
      = Nothing
      | offsets ! i == offsets ! (i + 1)
      = go (i + 1)
      | otherwise
      = Just (fromIntegral i)

-- | Find the first filled (length > zero) slot in the primary index. If there
-- is none, return 'Nothing'.
--
-- Example: given the primary index below:
--
-- > slot:     0   1
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 0 │ 4 │
-- >         └───┴───┴───┘
--
-- Return slot 1.
firstFilledSlot :: PrimaryIndex -> Maybe RelativeSlot
firstFilledSlot (MkPrimaryIndex offsets) = go 1
  where
    len = V.length offsets
    go i
      | i >= len
      = Nothing
      | offsets ! i == 0
      = go (i + 1)
      | otherwise
      = Just (fromIntegral (i - 1))

-- | Return a list of all the filled (length > zero) slots in the primary
-- index.
filledSlots :: PrimaryIndex -> [RelativeSlot]
filledSlots primary = go (firstFilledSlot primary)
  where
    go Nothing     = []
    go (Just slot) = slot : go (nextFilledSlot primary slot)

-- | Return the last filled slot in the primary index.
lastFilledSlot :: HasCallStack => PrimaryIndex -> Maybe RelativeSlot
lastFilledSlot (MkPrimaryIndex offsets) = go (V.length offsets - 1)
  where
    go i
      | i < 1
      = Nothing
      | offsets ! i == offsets ! (i - 1)
      = go (i - 1)
      | otherwise
      = Just (fromIntegral i - 1)

-- | Check if the first 'PrimaryIndex' is a prefix of the second
-- 'PrimaryIndex'.
isPrefixOf :: PrimaryIndex -> PrimaryIndex -> Bool
isPrefixOf (MkPrimaryIndex pre) (MkPrimaryIndex offsets)
    | V.length pre > V.length offsets
    = False
    | otherwise
    = V.and $ V.zipWith (==) pre offsets

-- TODO remove if possible
-- | Add trailing unfilled slots from the second primary index to the end of
-- the first.
--
-- Precondition: the first primary index is non-empty and a prefix of the
-- second primary index.
--
-- Example: given the primary indices below:
--
-- > ┌───┬───┐
-- > │ 0 │ 1 │
-- > └───┴───┘
--
-- > ┌───┬───┬───┬───┐
-- > │ 0 │ 1 │ 1 │ 2 │
-- > └───┴───┴───┴───┘
--
-- Return the following primary index:
--
-- > ┌───┬───┬───┐
-- > │ 0 │ 1 │ 1 │
-- > └───┴───┴───┘
extendWithTrailingUnfilledSlotsFrom
  :: PrimaryIndex
  -> PrimaryIndex
  -> PrimaryIndex
extendWithTrailingUnfilledSlotsFrom index1 index2 =
    assert (not (V.null validOffsets)) $
    assert (validOffsets == prefix)    $
    MkPrimaryIndex (validOffsets <> trailingUnfilledSlots)
  where
    MkPrimaryIndex validOffsets              = index1
    MkPrimaryIndex withTrailingUnfilledSlots = index2

    (prefix, trailingSlots) =
      V.splitAt (V.length validOffsets) withTrailingUnfilledSlots
    trailingUnfilledSlots
      | V.null trailingSlots
      = V.empty
      | otherwise
      = V.takeWhile (== V.last validOffsets) trailingSlots

-- | Return the slots to backfill the primary index file with.
--
-- A situation may arise in which we \"skip\" some relative slots, and we
-- write into the DB, for example, every other relative slot. In this case, we
-- need to backfill the primary index file with offsets for the skipped
-- relative slots. Similarly, before we start a new epoch, we must backfill
-- the primary index file of the current epoch to indicate that the remaining
-- slots in the epoch are empty.
--
-- TODO update this
-- For example, say we have written \"a\" to relative slot 0 and \"bravo\" to
-- relative slot 1. We have the following primary index file:
--
-- > slot:     0   1   2
-- >         ┌───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │
-- >         └───┴───┴───┘
--
-- Now we want to store \"haskell\" in relative slot 4, skipping 2 and 3. We
-- first have to backfill the primary index by repeating the last offset for
-- the two missing slots:
--
-- > slot:     0   1   2   3   4
-- >         ┌───┬───┬───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │
-- >         └───┴───┴───┴───┴───┘
--
-- After backfilling (writing the offset 6 twice), we can write the next
-- offset:
--
-- > slot:     0   1   2   3   4   5
-- >         ┌───┬───┬───┬───┬───┬───┐
-- > offset: │ 0 │ 1 │ 6 │ 6 │ 6 │ 13│
-- >         └───┴───┴───┴───┴───┴───┘
--
-- For the example above, the output of this function would thus be: @[6, 6]@.
--
backfill
  :: RelativeSlot     -- ^ The slot to write to (>= next expected slot)
  -> RelativeSlot     -- ^ The next expected slot to write to
  -> SecondaryOffset  -- ^ The last 'SecondaryOffset' written to
  -> [SecondaryOffset]
backfill (RelativeSlot slot) (RelativeSlot nextExpected) offset =
    replicate gap offset
  where
    gap = fromIntegral $ slot - nextExpected

{------------------------------------------------------------------------------
  Helper for debugging
------------------------------------------------------------------------------}

(!) :: (HasCallStack, V.Unbox a) => Vector a -> Int -> a
v ! i
  | 0 <= i, i < V.length v
  = V.unsafeIndex v i
  | otherwise
  = error $
    "Index " <> show i <> " out of bounds (0, " <> show (V.length v - 1) <> ")"
{-# INLINE (!) #-}
