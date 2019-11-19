{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
module Test.Ouroboros.Storage.ImmutableDB (tests) where

import qualified Codec.Serialise as S
import           Control.Tracer (nullTracer)
import           Data.Binary (get, put)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust, maybeToList)
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic (monadicIO, run)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Block (blockHash)

import           Ouroboros.Consensus.Block (IsEBB (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.ImmutableDB
import           Ouroboros.Storage.ImmutableDB.Impl
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary (PrimaryIndex)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
import           Ouroboros.Storage.ImmutableDB.Impl.Util (tryImmDB)
import           Ouroboros.Storage.ImmutableDB.Impl.Validation
                     (ShouldBeFinalised (..), reconstructPrimaryIndex)
import           Ouroboros.Storage.ImmutableDB.Layout
import           Ouroboros.Storage.ImmutableDB.Parser (epochFileParser)
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Util.FS.Sim.FsTree (FsTree (..))
import qualified Test.Util.FS.Sim.FsTree as FS
import           Test.Util.FS.Sim.MockFS (HandleMock, MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import qualified Test.Util.FS.Sim.STM as Sim

import qualified Test.Ouroboros.Storage.ImmutableDB.CumulEpochSizes as CumulEpochSizes
import qualified Test.Ouroboros.Storage.ImmutableDB.Primary as Primary
import qualified Test.Ouroboros.Storage.ImmutableDB.StateMachine as StateMachine
import           Test.Ouroboros.Storage.TestBlock
import           Test.Ouroboros.Storage.Util

{------------------------------------------------------------------------------
  The list of all tests
------------------------------------------------------------------------------}

tests :: HasCallStack => TestTree
tests = testGroup "ImmutableDB"
    [ testCase     "AppendToSlotInThePastError equivalence" test_AppendToSlotInThePastErrorEquivalence
    , testCase     "ReadFutureSlotError equivalence" test_ReadFutureSlotErrorEquivalence
    , testCase     "openDB with an empty index file" test_openDBEmptyIndexFileEquivalence
    , testCase     "closeDB is idempotent" test_closeDBIdempotentEquivalence
    , testCase     "appendBinaryBlob after closeDB throws a ClosedDBError" test_closeDBAppendBinaryBlobEquivalence
    , testProperty "reconstructPrimaryIndex" prop_reconstructPrimaryIndex
    , Primary.tests
    , StateMachine.tests
    , CumulEpochSizes.tests
    ]


fixedEpochSize :: EpochSize
fixedEpochSize = 10

type Hash = TestHeaderHash

-- Shorthand
openTestDB :: (HasCallStack, IOLike m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> m (ImmutableDB Hash m)
openTestDB = undefined
-- openTestDB hasFS err = openDB
--     S.decode
--     S.encode
--     hasFS
--     err
--     (fixedSizeEpochInfo fixedEpochSize)
--     ValidateMostRecentEpoch
--     parser
--     nullTracer
--   where
--     parser = epochFileParser hasFS (const <$> S.decode) isEBB
--     isEBB b = case testBlockIsEBB b of
--       IsEBB    -> Just (blockHash b)
--       IsNotEBB -> Nothing

-- Shorthand
withTestDB :: (HasCallStack, IOLike m)
           => HasFS m h
           -> ErrorHandling ImmutableDBError m
           -> (ImmutableDB Hash m -> m a)
           -> m a
withTestDB hasFS err = withDB (openTestDB hasFS err)

{------------------------------------------------------------------------------
  Equivalence tests between IO and MockFS
------------------------------------------------------------------------------}

-- Trying to append to a slot \"in the past\" should be an error
test_AppendToSlotInThePastErrorEquivalence :: HasCallStack => Assertion
test_AppendToSlotInThePastErrorEquivalence = undefined
  --   apiEquivalenceImmDB (expectUserError isAppendToSlotInThePastError) $ \hasFS err ->
  --     withTestDB hasFS err $ \db -> do
  --       appendBinaryBlob db 3 "test"
  --       appendBinaryBlob db 2 "haskell"
  -- where
  --   isAppendToSlotInThePastError AppendToSlotInThePastError {} = True
  --   isAppendToSlotInThePastError _                             = False

test_ReadFutureSlotErrorEquivalence :: HasCallStack => Assertion
test_ReadFutureSlotErrorEquivalence = undefined
  --   apiEquivalenceImmDB (expectUserError isReadFutureSlotError) $ \hasFS err ->
  --     withTestDB hasFS err $ \db -> do
  --       _ <- getBinaryBlob db 0
  --       return ()
  -- where
  --   isReadFutureSlotError ReadFutureSlotError {} = True
  --   isReadFutureSlotError _                      = False

test_openDBEmptyIndexFileEquivalence :: Assertion
test_openDBEmptyIndexFileEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= TipGen)) $ \hasFS@HasFS{..} err -> do
      -- Create an empty index file
      h1 <- hOpen (mkFsPath ["epoch-000.dat"]) (WriteMode MustBeNew)
      h2 <- hOpen (mkFsPath ["index-000.dat"]) (WriteMode MustBeNew)
      hClose h1
      hClose h2

      withTestDB hasFS err getTip

test_closeDBIdempotentEquivalence :: Assertion
test_closeDBIdempotentEquivalence =
    apiEquivalenceImmDB (expectImmDBResult (@?= ())) $ \hasFS err -> do
      db <- openTestDB hasFS err
      closeDB db
      closeDB db

test_closeDBAppendBinaryBlobEquivalence :: Assertion
test_closeDBAppendBinaryBlobEquivalence = undefined
  --   apiEquivalenceImmDB (expectUserError isClosedDBError) $ \hasFS err -> do
  --     db <- openTestDB hasFS err
  --     closeDB db
  --     appendBlock db 0 "foo"
  -- where
  --   isClosedDBError ClosedDBError {} = True
  --   isClosedDBError _                = False


{------------------------------------------------------------------------------
  reconstructPrimaryIndex
------------------------------------------------------------------------------}

prop_reconstructPrimaryIndex :: PrimaryIndex -> Property
prop_reconstructPrimaryIndex primaryIndex =
    counterexample ("blocksOrEBBs: " <> show blockOrEBBs)    $
    counterexample ("primaryIndex': " <> show primaryIndex') $
    reconstructedPrimaryIndex === primaryIndex'
  where
    reconstructedPrimaryIndex = runIdentity $
      reconstructPrimaryIndex epochInfo hashInfo ShouldNotBeFinalised
                              blockOrEBBs

    -- Remove empty trailing slots because we don't reconstruct them
    primaryIndex' = case Primary.lastFilledSlot primaryIndex of
      Just slot -> Primary.truncateToSlot slot primaryIndex
      -- Index is empty, use the minimal empty index without any trailing
      -- slots
      Nothing   -> fromJust $ Primary.mk [0]

    blockOrEBBs :: [BlockOrEBB]
    blockOrEBBs =
      [ if relSlot == 0 then EBB 0 else Block (coerce relSlot - 1)
      | relSlot <- Primary.filledSlots primaryIndex]

    -- Use maxBound as epoch size so that we can easily map from SlotNo to
    -- RelativeSlot and vice versa.
    epochInfo :: EpochInfo Identity
    epochInfo = fixedSizeEpochInfo (EpochSize maxBound)

    -- Only 'hashSize' is used. Note that 32 matches the hard-coded value in
    -- the 'PrimaryIndex' generator we use.
    hashInfo :: HashInfo ()
    hashInfo = HashInfo
      { hashSize = 32
      , getHash  = get
      , putHash  = put
      }
