{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Storage.ImmutableDB.Impl.Iterator
  ( streamImpl
  , BlocksOrHeaders (..)
  ) where

import           Control.Monad (when)
import           Control.Monad.Except
import           Control.Monad.State.Strict (state)
import           Data.ByteString.Lazy (ByteString)
import           Data.List (find)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     allNoUnexpectedThunks)

import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)

import           Ouroboros.Storage.ImmutableDB.API
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockSize (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.State
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Layout

{------------------------------------------------------------------------------
  ImmutableDB Iterator Implementation
------------------------------------------------------------------------------}

-- | Internal handle to an iterator
data IteratorHandle hash m = forall h. IteratorHandle
  { itHasFS   :: !(HasFS m h)
    -- ^ Bundled HasFS instance allows to hide type parameters
  , itState   :: !(StrictTVar m (IteratorStateOrExhausted hash h))
    -- ^ The state of the iterator. If it is 'Nothing', the iterator is
    -- exhausted and/or closed.
  , itEnd     :: !EpochSlot
    -- ^ The end of the iterator: the last 'EpochSlot' it should return.
  , itEndHash :: !hash
    -- ^ The @hash@ of the last block the iterator should return.
  }

data IteratorStateOrExhausted hash h =
    IteratorStateOpen !(IteratorState hash h)
  | IteratorStateExhausted
  deriving (Generic, NoUnexpectedThunks)

iteratorStateIsOpen :: IteratorStateOrExhausted hash h -> Bool
iteratorStateIsOpen (IteratorStateOpen _)  = True
iteratorStateIsOpen IteratorStateExhausted = False

-- Existential type; we can't use generics
instance ( forall a. NoUnexpectedThunks (StrictTVar m a)
         , NoUnexpectedThunks hash
         ) => NoUnexpectedThunks (IteratorHandle hash m) where
  showTypeOf _ = "IteratorHandle"
  whnfNoUnexpectedThunks ctxt IteratorHandle{..} =
      allNoUnexpectedThunks [
          noUnexpectedThunks ctxt itHasFS
        , noUnexpectedThunks ctxt itState
        , noUnexpectedThunks ctxt itEnd
        , noUnexpectedThunks ctxt itEndHash
        ]

data IteratorState hash h = IteratorState
  { itEpoch        :: !EpochNo
    -- ^ The current epoch the iterator is streaming from.
  , itEpochHandle  :: !(Handle h)
    -- ^ A handle to the epoch file corresponding with 'itNext'.
  , itEpochEntries :: !(NonEmpty (Secondary.Entry hash, BlockSize))
    -- ^ The entries from the secondary index corresponding to the current
    -- epoch. The first entry in the list is the next one to stream.
    --
    -- Invariant: all the entries in this list must be included in the stream.
    -- In other words, entries corresponding to blocks after the end bound are
    -- not included in this list.
  }
  deriving (Generic, NoUnexpectedThunks)

data BlocksOrHeaders
  = Blocks
  | Headers

streamImpl
  :: forall m hash. (HasCallStack, IOLike m, Eq hash, NoUnexpectedThunks hash)
  => ImmutableDBEnv m hash
  -> BlocksOrHeaders
  -> Maybe (SlotNo, hash)
     -- ^ When to start streaming (inclusive).
  -> Maybe (SlotNo, hash)
     -- ^ When to stop streaming (inclusive).
  -> m (Either (WrongBoundError hash)
               (Iterator hash m ByteString))
streamImpl dbEnv blocksOrHeaders mbStart mbEnd =
    withOpenState dbEnv $ \hasFS OpenState{..} -> runExceptT $ do
      lift $ validateIteratorRange _dbErr _dbEpochInfo (fst <$> _currentTip)
        mbStart mbEnd

      -- TODO cache index files: we might open the same primary and secondary
      -- indices to validate the end bound as for the start bound

      case _currentTip of
        TipGen -> lift mkEmptyIterator
        Tip tip -> do
          (endEpochSlot, endHash)  <- fillInEndBound   hasFS tip mbEnd
          (secondaryOffset, start) <- fillInStartBound hasFS     mbStart

          lift $ do
            -- 'validateIteratorRange' will catch nearly all invalid ranges,
            -- except for one: streaming from the regular block to the EBB in
            -- the same slot. The EBB comes before the regular block, so these
            -- bounds are invalid. However, to distinguish the EBB from the
            -- regular block, as both have the same slot number, we need to
            -- look at the hashes. 'validateIteratorRange' doesn't have enough
            -- information to do that.
            let (startEpochSlot, _startHash) = start
            when (startEpochSlot > endEpochSlot) $ do
              startSlot <- epochInfoAbsolute _dbEpochInfo startEpochSlot
              endSlot   <- epochInfoAbsolute _dbEpochInfo endEpochSlot
              throwUserError _dbErr $ InvalidIteratorRangeError startSlot endSlot

            let EpochSlot startEpoch startRelSlot = startEpochSlot
                startIsEBB | startRelSlot == 0 = IsEBB
                           | otherwise         = IsNotEBB

            -- TODO avoid rereading the indices of the start epoch. We read
            -- from both the primary and secondary index in 'fillInStartBound'

            iteratorState <- iteratorStateForEpoch hasFS _dbErr _dbHashInfo
              endHash startEpoch secondaryOffset startIsEBB

            varIteratorState <- newTVarM $ IteratorStateOpen iteratorState

            mkIterator IteratorHandle
              { itHasFS   = hasFS
              , itState   = varIteratorState
              , itEnd     = endEpochSlot
              , itEndHash = endHash
              }
  where
    ImmutableDBEnv { _dbErr, _dbEpochInfo, _dbHashInfo } = dbEnv

    -- | Fill in the end bound: if 'Nothing', use the current tip. Otherwise,
    -- check whether the bound exists in the database and return the
    -- corresponding 'EpochSlot'.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    fillInEndBound
      :: HasCallStack
      => HasFS m h
      -> (BlockOrEBB, hash)    -- ^ Current tip
      -> Maybe (SlotNo, hash)  -- ^ End bound
      -> ExceptT (WrongBoundError hash) m (EpochSlot, hash)
    fillInEndBound hasFS currentTip = \case
      -- End bound given, check whether it corresponds to a regular block or
      -- an EBB. Convert the 'SlotNo' to an 'EpochSlot' accordingly.
      Just end -> snd <$> checkBound hasFS end

      -- No end bound given, use the current tip, but convert the 'BlockOrEBB'
      -- to an 'EpochSlot'.
      Nothing  -> lift $ flip overFst currentTip $ \case
        EBB epoch      -> return (EpochSlot epoch 0)
        Block lastSlot -> epochInfoBlockRelative _dbEpochInfo lastSlot

    overFst :: forall a b c f. Functor f => (a -> f c) -> (a, b) -> f (c, b)
    overFst f (a, b) = (, b) <$> f a

    -- | Fill in the start bound: if 'Nothing', use the first block in the
    -- database. Otherwise, check whether the bound exists in the database and
    -- return the corresponding 'EpochSlot' and 'SecondaryOffset'.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    fillInStartBound
      :: HasCallStack
      => HasFS m h
      -> Maybe (SlotNo, hash)  -- ^ Start bound
      -> ExceptT (WrongBoundError hash) m (SecondaryOffset, (EpochSlot, hash))
    fillInStartBound hasFS = \case
      -- Start bound given, check whether it corresponds to a regular block or
      -- an EBB. Convert the 'SlotNo' to an 'EpochSlot' accordingly.
      Just start -> checkBound hasFS start

      -- No start bound given, use the first block in the ImmutableDB as the
      -- start bound.
      Nothing -> lift $ findFirstFilledSlot 0
        where
          findFirstFilledSlot epoch =
            Primary.readFirstFilledSlot hasFS _dbErr epoch >>= \case
              -- We know the database is not empty, so this loop must end
              -- before we reach an epoch that doesn't yet exist (which would
              -- result in an error).
              Nothing      -> findFirstFilledSlot (epoch + 1)
              Just relSlot -> do
                  (Secondary.Entry { headerHash }, _) <-
                    Secondary.readEntry hasFS _dbErr _dbHashInfo epoch isEBB
                      secondaryOffset
                  return (secondaryOffset, (EpochSlot epoch relSlot, headerHash))
                where
                  -- The first entry in the secondary index file (i.e. the
                  -- first filled slot in the primary index) always starts at
                  -- 0.
                  secondaryOffset = 0
                  isEBB | relSlot == 0 = IsEBB
                        | otherwise    = IsNotEBB

    -- | Check whether the given bound exists in the ImmutableDB, otherwise a
    -- 'WrongBoundError' is returned. The 'SecondaryOffset' and 'EpochSlot'
    -- for the bound are returned.
    --
    -- The primary index is read to find out whether the slot is filled and
    -- what the 'SecondaryOffset' is for the slot. The secondary index is read
    -- to check th hash.
    --
    -- PRECONDITION: the bound is in the past.
    --
    -- PRECONDITION: the database is not empty.
    checkBound
      :: HasCallStack
      => HasFS m h
      -> (SlotNo, hash)  -- ^ Bound
      -> ExceptT (WrongBoundError hash) m (SecondaryOffset, (EpochSlot, hash))
    checkBound hasFS (slot, hash) = do
        epochSlot@(EpochSlot epoch relSlot) <- lift $
          epochInfoBlockRelative _dbEpochInfo slot
        let couldBeEBB = relSlot == 1

        primaryIndex <- lift $ Primary.load hasFS _dbErr epoch
        let isFilledSlot s =
              Primary.containsSlot primaryIndex s &&
              Primary.isFilledSlot primaryIndex s
            offsetOfSlot = Primary.offsetOfSlot primaryIndex

        -- The offsets to read in the secondary index file. The block /could/
        -- still correspond to an EBB, a regular block or both. We will know
        -- which one it is when we have the hashes from the secondary index
        -- file.
        toRead :: [(IsEBB, SecondaryOffset)] <- case isFilledSlot relSlot of
          True
            | couldBeEBB, isFilledSlot 0
            -> return [(IsEBB, offsetOfSlot 0), (IsNotEBB, offsetOfSlot relSlot)]
            | otherwise
            -> return [(IsNotEBB, offsetOfSlot relSlot)]
          False
            | couldBeEBB, isFilledSlot 0
            -> return [(IsEBB, offsetOfSlot 0)]
            | otherwise
            -> throwError $ EmptySlotError slot

        entries :: [Secondary.Entry hash] <- lift $ fmap fst <$>
          Secondary.readEntries hasFS _dbErr _dbHashInfo epoch toRead

        -- The entry from the secondary index file that matches the expected
        -- hash.
        (secondaryOffset, entry) :: (SecondaryOffset, Secondary.Entry hash) <-
          case find ((== hash) . Secondary.headerHash . snd)
                    (zip (map snd toRead) entries) of
            Just found -> return found
            Nothing    -> throwError $ WrongHashError slot hash hashes
              where
                hashes :: Those hash hash
                hashes = case zip (fst <$> toRead)
                                  (Secondary.headerHash <$> entries) of
                  [(IsEBB, h)]              -> This h
                  [(IsNotEBB, h)]           -> That h
                  [(IsEBB, h), (IsEBB, h')] -> These h h'
                  -- @entries@ will have the same structure as @toRead@
                  _                         -> error "impossible"

        -- Use the secondary index entry to determine whether the slot + hash
        -- correspond to an EBB or a regular block.
        return $ (secondaryOffset,) $ (, hash) $
          case Secondary.blockOrEBB entry of
            Block _ -> epochSlot
            EBB   _ -> EpochSlot epoch 0

    withNewIteratorID
      :: (IteratorID -> Iterator hash m ByteString)
      -> m (Iterator hash m ByteString)
    withNewIteratorID mkIter = modifyOpenState dbEnv $ \_hasFS ->
      state $ \st@OpenState { _nextIteratorID = itID } ->
        (mkIter (BaseIteratorID itID), st { _nextIteratorID = succ itID })

    mkEmptyIterator :: m (Iterator hash m ByteString)
    mkEmptyIterator = withNewIteratorID $ \itID -> Iterator
      { iteratorNext    = return IteratorExhausted
      , iteratorPeek    = return IteratorExhausted
      , iteratorHasNext = return False
      , iteratorClose   = return ()
      , iteratorID      = itID
      }

    mkIterator :: IteratorHandle hash m -> m (Iterator hash m ByteString)
    mkIterator ith = withNewIteratorID $ \itID -> Iterator
      { iteratorNext    = iteratorNextImpl dbEnv ith blocksOrHeaders True
      , iteratorPeek    = iteratorNextImpl dbEnv ith blocksOrHeaders False
      , iteratorHasNext = iteratorHasNextImpl    ith
      , iteratorClose   = iteratorCloseImpl      ith
      , iteratorID      = itID
      }

iteratorNextImpl
  :: forall m hash. (IOLike m, Eq hash)
  => ImmutableDBEnv m hash
  -> IteratorHandle hash m
  -> BlocksOrHeaders
  -> Bool  -- ^ Step the iterator after reading iff True
  -> m (IteratorResult hash ByteString)
iteratorNextImpl dbEnv it@IteratorHandle {itHasFS = hasFS :: HasFS m h, ..}
                 blocksOrHeaders step = do
    -- The idea is that if the state is not Nothing, then 'itNext' is always
    -- ready to be read. After reading it with 'readNext', 'stepIterator' will
    -- advance the iterator to the next valid epoch slot if @step@ is True.
    atomically (readTVar itState) >>= \case
      -- Iterator already closed
      IteratorStateExhausted -> return IteratorExhausted
      IteratorStateOpen iteratorState@IteratorState{..} ->
        withOpenState dbEnv $ \_ _ -> do
          let entryAndBlockSize@(entry, _) = NE.head itEpochEntries
              hash = Secondary.headerHash entry
          blob <- case blocksOrHeaders of
            Blocks  -> readNextBlock  itEpochHandle entryAndBlockSize
            Headers -> readNextHeader itEpochHandle entry
          when step $ stepIterator iteratorState
          return $ case Secondary.blockOrEBB entry of
            Block slot  -> IteratorResult slot  hash blob
            EBB   epoch -> IteratorEBB    epoch hash blob
  where
    ImmutableDBEnv { _dbErr, _dbEpochInfo, _dbHashInfo } = dbEnv
    HasFS { hClose } = hasFS

    -- TODO check checksum
    readNextBlock
      :: Handle h
      -> (Secondary.Entry hash, BlockSize)
      -> m ByteString
    readNextBlock eHnd (Secondary.Entry { blockOffset }, blockSize) =
        case (step, blockSize) of
          (True,  LastEntry)      -> hGetAll       hasFS eHnd
          (True,  BlockSize size) -> hGetExactly   hasFS eHnd (fromIntegral size)
          -- Don't advance the handle when we're not stepping the iterator
          (False, LastEntry)      -> hGetAllAt     hasFS eHnd                     offset
          (False, BlockSize size) -> hGetExactlyAt hasFS eHnd (fromIntegral size) offset
      where
        offset = AbsOffset $ Secondary.unBlockOffset blockOffset

    -- | We don't rely on the position of the handle, we always use
    -- 'hGetExactlyAt', i.e. @pread@ for reading from a given offset.
    readNextHeader
      :: Handle h
      -> Secondary.Entry hash
      -> m ByteString
    readNextHeader eHnd Secondary.Entry { blockOffset, headerOffset, headerSize } =
        hGetExactlyAt hasFS eHnd size offset
      where
        size   = fromIntegral $ Secondary.unHeaderSize headerSize
        offset = AbsOffset $
          (Secondary.unBlockOffset blockOffset) +
          fromIntegral (Secondary.unHeaderOffset headerOffset)

    -- | Move the iterator to the next position that can be read from,
    -- advancing epochs if necessary. If no next position can be found, the
    -- iterator is closed.
    stepIterator :: IteratorState hash h -> m ()
    stepIterator iteratorState@IteratorState {..} =
      case NE.nonEmpty (NE.tail itEpochEntries) of
        -- There are entries left in this epoch, so continue. See the
        -- invariant on 'itEpochEntries'
        Just itEpochEntries' -> atomically $ writeTVar itState $
          IteratorStateOpen iteratorState { itEpochEntries = itEpochEntries' }

        -- No more entries in this epoch, so open the next.
        Nothing -> do
          hClose itEpochHandle
          -- If this was the final epoch, close the iterator
          if itEpoch >= _epoch itEnd then
            iteratorCloseImpl it

          else openNextEpoch itEnd (itEpoch + 1) >>= \iteratorState' ->
            atomically $ writeTVar itState $ IteratorStateOpen iteratorState'

    openNextEpoch
      :: EpochSlot  -- ^ The end bound
      -> EpochNo    -- ^ The epoch to open
      -> m (IteratorState hash h)
    openNextEpoch end epoch =
      Primary.readFirstFilledSlot hasFS _dbErr epoch >>= \case
        -- This epoch is empty, look in the next one.
        --
        -- We still haven't encountered the end bound, so this loop must end
        -- when we reach the non-empty epoch containing the end bound. This
        -- cannot loop forever as an error would be thrown when opening the
        -- index file(s) of a non-existing epoch.
        Nothing      -> openNextEpoch end (epoch + 1)
        Just relSlot -> do
          -- Note that the only reason we actually open the primary index file
          -- is to see whether the first block in the epoch is an EBB or not.
          -- To see whether the epoch is empty, we could open the secondary
          -- index file directly and see whether it contains any blocks. The
          -- 'secondaryOffset' will be 0, as the first entry in the secondary
          -- index file always starts at offset 0. The same is true for
          -- 'findFirstFilledSlot'.
          let firstIsEBB | relSlot == 0 = IsEBB
                         | otherwise    = IsNotEBB
              secondaryOffset = 0

          iteratorStateForEpoch hasFS _dbErr _dbHashInfo itEndHash epoch
            secondaryOffset firstIsEBB

iteratorHasNextImpl
  :: (HasCallStack, IOLike m)
  => IteratorHandle hash m
  -> m Bool
iteratorHasNextImpl IteratorHandle { itState } =
    atomically $ iteratorStateIsOpen <$> readTVar itState

iteratorCloseImpl
  :: (HasCallStack, IOLike m)
  => IteratorHandle hash m
  -> m ()
iteratorCloseImpl IteratorHandle {..} = do
    atomically (readTVar itState) >>= \case
      -- Already closed
      IteratorStateExhausted -> return ()
      IteratorStateOpen IteratorState {..} -> do
        -- First set it to Nothing to indicate it is closed, as the call to
        -- 'hClose' might fail, which would leave the iterator open in an
        -- invalid state.
        atomically $ writeTVar itState IteratorStateExhausted
        hClose itEpochHandle
  where
    HasFS { hClose } = itHasFS

iteratorStateForEpoch
  :: (HasCallStack, IOLike m, Eq hash)
  => HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> HashInfo hash
  -> hash
     -- ^ Hash of the end bound
  -> EpochNo
  -> SecondaryOffset
     -- ^ Where to start in the secondary index
  -> IsEBB
     -- ^ Whether the first expected block will be an EBB or not.
  -> m (IteratorState hash h)
iteratorStateForEpoch hasFS err hashInfo endHash epoch secondaryOffset
                      firstIsEBB = do
    entries <- dropAfter ((== endHash) . Secondary.headerHash . fst) <$>
      Secondary.readAllEntries hasFS err hashInfo
      secondaryOffset epoch firstIsEBB

    -- Open the epoch file
    eHnd <- hOpen (renderFile "epoch" epoch) ReadMode

    -- If we don't close the handle if 'getSize' throws an exception, we
    -- leak a file handle, since this handle is not stored in a state
    -- that can be closed yet.
    epochFileSize <- onException hasFsErr err (hClose eHnd) $
      hGetSize eHnd

    case NE.nonEmpty (fillInLastBlockSize epochFileSize entries) of
      -- We still haven't encountered the end bound, so it cannot be
      -- that this non-empty epoch contains no entries <= the end bound.
      Nothing             -> error
        "impossible: there must be entries according to the primary index"

      Just itEpochEntries -> do
        let (nextEntry, _) NE.:| _ = itEpochEntries
            offset = fromIntegral $ Secondary.unBlockOffset $
              Secondary.blockOffset nextEntry

        -- Seek the handle so that we can read the first block to
        -- stream.
        onException hasFsErr err (hClose eHnd) $
          hSeek eHnd AbsoluteSeek offset

        return IteratorState
          { itEpoch       = epoch
          , itEpochHandle = eHnd
          , itEpochEntries
          }
  where
    HasFS { hOpen, hClose, hSeek, hGetSize, hasFsErr } = hasFS

-- | When reading the entries from the secondary index files, we can calculate
-- the block size corresponding to each entry based on the offset of the entry
-- and offset of the entry after it. We cannot do this for the last entry, as
-- there is no entry after it. We assign 'LastEntry' to that entry so that we
-- know that when we try to read the corresponding block, it is the last block
-- in the epoch file, which means we don't need to know the size of the block,
-- as we can simply read the epoch file until the end.
--
-- This works fine when we read the last block directly after obtaining its
-- corresponding entry. However, in the case of iterators, we read all
-- entries, but before we try to read the block corresponding to the last
-- entry, another block may have been appended to the same epoch file. In this
-- case, reading the epoch file until the end would mean that we read two
-- blocks instead of one!
--
-- For this reason, we try to replace the 'LastEntry' of the last block with
-- the actual block size using the current size of the epoch file.
--
-- It could be that the entry with 'LastEntry' was removed from the list
-- when dropping blocks that come after the end bound, in which case the
-- original input list is returned.
fillInLastBlockSize
  :: Word64  -- ^ The size of the epoch file
  -> [(Secondary.Entry hash, BlockSize)]
  -> [(Secondary.Entry hash, BlockSize)]
fillInLastBlockSize epochFileSize = go
  where
    go [] = []
    go [(e@Secondary.Entry { blockOffset }, LastEntry)] = [(e, BlockSize sz)]
      where
        sz = fromIntegral (epochFileSize - Secondary.unBlockOffset blockOffset)
    go (x:xs) = x:go xs

dropAfter :: forall a. (a -> Bool) -> [a] -> [a]
dropAfter p = go
  where
    go []                 = []
    go (x:xs) | p x       = [x]
              | otherwise = x : go xs
