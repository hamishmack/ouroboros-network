{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Ouroboros.Network.PeerSelection.JobPool (
    JobPool,
    Job(..),
    withJobPool,
    forkJob,
    readSize,
    collect
  ) where

--import qualified Data.Set as Set
import           Data.Set (Set)

import           Control.Monad.Class.MonadSTM
import           Control.Exception (SomeException)
import           Control.Monad.Class.MonadFork



data JobPool m a = JobPool !(TVar m (Set (ThreadId m)))
                           !(TQueue m a)

data Job m a = SingleOutputJob (m a)
             | MultiOutputJob  ((a -> m ()) -> m a)

withJobPool :: MonadSTM m => (JobPool m a -> m b) -> m b
withJobPool = undefined

forkJob :: (MonadSTM m, MonadFork m) => JobPool m a -> Job m a -> m ()
forkJob = undefined

readSize :: MonadSTM m => JobPool m a -> STM m Int
readSize = undefined

collect :: MonadSTM m => JobPool m a -> STM m (Either SomeException a)
collect = undefined

{-
forkInThreadSet action =
    mask $ \unmask -> do
      tid <- fork $ do
               res <- try action

    mask $ \unmask -> do
      tid <- fork $ unmask $ do
               case action of
                 AsyncActionSingleCompletion a ->
               tid <- myThreadId
               atomically $ writeTQueue complet
      atomically $ modifyTVar' threadsVar (Set.insert tid)


    mask $ \unmask -> do
      tid <- fork $ unmask $ do
               case action of
                 AsyncActionSingleCompletion a ->
               tid <- myThreadId
               atomically $ writeTQueue complet
      atomically $ modifyTVar' threadsVar (Set.insert tid)
    aaction <- case action of
      AsyncActionSingleCompletion a ->
        async a

    -- return updated state
    return st'

forkInThreadSet action =
    mask $ \unmask -> do
      tid <- fork $ do
               res <- try action
-}

