{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Ouroboros.Storage.FS
  ( tests
  -- exported to silence warnings.
  , test_example
  ) where

import qualified Test.Ouroboros.Storage.FS.StateMachine as StateMachine
import           Test.Ouroboros.Storage.Util
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit

import           Ouroboros.Storage.FS.API

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS"
    [ StateMachine.tests tmpDir
    ]

{------------------------------------------------------------------------------
 The tests proper
-------------------------------------------------------------------------------}

-- | A unit test example.
test_example :: Assertion
test_example =
  apiEquivalenceFs (expectFsResult (@?= ())) $ \HasFS{..} _err ->
    return ()
