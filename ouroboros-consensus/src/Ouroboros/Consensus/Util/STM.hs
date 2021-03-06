{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Util.STM (
    -- * Misc
    blockUntilChanged
  , onEachChange
  , runWhenJust
  , blockUntilJust
  , blockUntilAllJust
  , Fingerprint (..)
  , WithFingerprint (..)
    -- * Simulate various monad stacks in STM
  , Sim
  , simId
  , simReaderT
  , simWriterT
  , simStateT
  , simOuroborosStateT
  , simChaChaT
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.Coerce
import           Data.Void
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

-- | Wait until the TVar changed
blockUntilChanged :: forall m a b. (IOLike m, Eq b)
                  => (a -> b) -> b -> STM m a -> STM m (a, b)
blockUntilChanged f b getA = do
    a <- getA
    let b' = f a
    if b' == b
      then retry
      else return (a, b')

-- | Spawn a new thread that runs an action each time an STM value changes.
--
-- NOTE: STM does not guarantee that 'onEachChange' will /literally/ be called
-- on /every/ change: when the system is under heavy load, some updates may
-- be missed.
--
-- The thread will be linked to the registry.
onEachChange :: forall m a b. (IOLike m, Eq b, HasCallStack)
             => ResourceRegistry m
             -> (a -> b)  -- ^ Obtain a fingerprint
             -> Maybe b   -- ^ Optional initial fingerprint
                          -- If 'Nothing', the action is executed once
                          -- immediately to obtain the initial fingerprint.
             -> STM m a
             -> (a -> m ())
             -> m ()
onEachChange registry f mbInitB getA notify = do
    void $ forkLinkedThread registry body
  where
    body :: m Void
    body = do
        initB <- case mbInitB of
          Just initB -> return initB
          Nothing    -> do
            a <- atomically getA
            notify a
            return $ f a
        loop initB

    loop :: b -> m Void
    loop b = do
      (a, b') <- atomically $ blockUntilChanged f b getA
      notify a
      loop b'

-- | Spawn a new thread that waits for an STM value to become 'Just'
--
-- The thread will be linked to the registry.
runWhenJust :: IOLike m
            => ResourceRegistry m
            -> STM m (Maybe a)
            -> (a -> m ())
            -> m ()
runWhenJust registry getMaybeA action =
    void $ forkLinkedThread registry $
      action =<< atomically (blockUntilJust getMaybeA)

blockUntilJust :: IOLike m => STM m (Maybe a) -> STM m a
blockUntilJust getMaybeA = do
    ma <- getMaybeA
    case ma of
      Nothing -> retry
      Just a  -> return a

blockUntilAllJust :: IOLike m => [STM m (Maybe a)] -> STM m [a]
blockUntilAllJust = mapM blockUntilJust

-- | Simple type that can be used to indicate something in a @TVar@ is
-- changed.
newtype Fingerprint = Fingerprint Word64
  deriving stock    (Show, Eq, Generic)
  deriving newtype  (Enum)
  deriving anyclass (NoUnexpectedThunks)

-- | Store a value together with its fingerprint.
data WithFingerprint a = WithFingerprint
  { forgetFingerprint :: !a
  , getFingerprint    :: !Fingerprint
  } deriving (Show, Eq, Functor, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Simulate monad stacks
-------------------------------------------------------------------------------}

type Sim n m = forall a. n a -> STM m a

simId :: Sim (STM m) m
simId = id

simReaderT :: IOLike m => StrictTVar m st -> Sim n m -> Sim (ReaderT st n) m
simReaderT tvar k (ReaderT f) = do
    st <- readTVar tvar
    k (f st)

simWriterT :: IOLike m => StrictTVar m st -> Sim n m -> Sim (WriterT st n) m
simWriterT tvar k (WriterT f) = do
    (a, st') <- k f
    writeTVar tvar st'
    return a

simStateT :: IOLike m => StrictTVar m st -> Sim n m -> Sim (StateT st n) m
simStateT tvar k (StateT f) = do
    st       <- readTVar tvar
    (a, st') <- k (f st)
    writeTVar tvar st'
    return a

simOuroborosStateT :: IOLike m
                   => StrictTVar m s
                   -> Sim n m
                   -> Sim (NodeStateT_ s n) m
simOuroborosStateT tvar k n = do
    st       <- readTVar tvar
    (a, st') <- k (runNodeStateT n st)
    writeTVar tvar st'
    return a

simChaChaT :: (IOLike m, Coercible a ChaChaDRG)
           => StrictTVar m a
           -> Sim n m
           -> Sim (ChaChaT n) m
simChaChaT tvar k n = do
    st       <- readTVar tvar
    (a, st') <- k (runChaChaT n (coerce st))
    writeTVar tvar (coerce st')
    return a

-- | Example of composition
_exampleComposition :: IOLike m
                    => StrictTVar m r
                    -> StrictTVar m w
                    -> StrictTVar m s
                    -> Sim n m
                    -> Sim (ReaderT r (WriterT w (StateT s n))) m
_exampleComposition r w s k = simReaderT r $ simWriterT w $ simStateT s $ k
