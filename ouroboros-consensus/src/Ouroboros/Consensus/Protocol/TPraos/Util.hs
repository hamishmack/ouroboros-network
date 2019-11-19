-- | Assorted utility functions for TPraos integration.
--
-- In particular, various things we need for integration with the `delegation`
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Protocol.TPraos.Util where

import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Point (WithOrigin(..))

import Slot (Slot(..))

-- | Convert a ouroboros-consensus `SlotNo` to a cardano-ledger-specs `Slot`.
convertSlot :: SlotNo -> Slot
convertSlot (SlotNo n) = Slot $ fromIntegral n

convertSlotWithOrigin :: WithOrigin SlotNo -> Slot
convertSlotWithOrigin (At n) = convertSlot n
convertSlotWithOrigin Origin = Slot 0

-- | Convert a cardano-ledger-specs `Slot` to an ouroboros-consensus `SlotNo`.
convertSlotNo :: Slot -> SlotNo
convertSlotNo (Slot n) = SlotNo $ fromIntegral n

-- | Verify whether a slot represents a change to a new epoch with regard to
-- some other slot.
isNewEpoch :: SlotNo -> WithOrigin SlotNo -> Bool
isNewEpoch = undefined
