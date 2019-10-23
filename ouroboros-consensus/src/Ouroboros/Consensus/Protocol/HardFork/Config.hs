{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Ouroboros.Consensus.Protocol.HardFork.Config (
    HardForksTo
  , NodeConfig(..)
  ) where

import           Data.Typeable (typeRep)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Protocol.Abstract

-- | A protocol that acts as @p1@ until a hard fork switches to @p2@
data HardForksTo p1 p2

-- Store configuration for both protocols
data instance NodeConfig (p1 `HardForksTo` p2) =
    HardForkCfg {
        nodeConfigBeforeFork :: NodeConfig p1
      , nodeConfigAfterFork  :: NodeConfig p2
      }
  deriving (Generic)

instance (OuroborosTag p1, OuroborosTag p2)
      => NoUnexpectedThunks (NodeConfig (p1 `HardForksTo` p2)) where
  showTypeOf = show . typeRep
