{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE StrictData              #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

-- | Transitional Praos.
--
--   Transitional praos allows for the overlaying of Praos with an overlay
--   schedule determining slots to be produced by BFT
module Ouroboros.Consensus.Protocol.TPraos (
    TPraos
  , TPraosFields(..)
  , TPraosToSign(..)
  , TPraosParams(..)
  , TPraosProof(..)
  , TPraosIsCoreNode(..)
  , forgeTPraosFields
    -- * Tags
  , TPraosCrypto
  , TPraosStandardCrypto
  , TPraosMockCrypto
  , HeaderSupportsTPraos(..)
    -- * Type instances
  , NodeConfig(..)
  ) where

import           Control.Monad.Except (ExceptT(..))
import           Crypto.Random (MonadRandom)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Control.State.Transition (TRC(..), applySTS)
import           Cardano.Ledger.Shelley.API as API (LedgerView(..))
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..))
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.VRF.Class
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import qualified Ouroboros.Consensus.Protocol.TPraos.ChainState as ChainState
import           Ouroboros.Consensus.Protocol.TPraos.Crypto
import           Ouroboros.Consensus.Protocol.TPraos.Util
import           Ouroboros.Consensus.Util.Condense
import qualified Ouroboros.Consensus.Util.AnchoredFragment as AF

import BaseTypes (Nonce, UnitInterval)
import BlockChain (BHeader, HashHeader, mkSeed, seedEta, seedL)
import Delegation.Certificates (PoolDistr(..))
import Keys (DiscVKey(..), GenDelegs(..), KeyHash, hashKey)
import OCert (OCert(..))
import STS.Prtcl (PRTCL)
import qualified STS.Prtcl as STS

{-------------------------------------------------------------------------------
  Fields required by TPraos in the header
-------------------------------------------------------------------------------}

data TPraosFields c toSign = TPraosFields
  { tpraosSignature :: SignedKES (TPraosKES c) toSign
  , tpraosToSign :: TPraosToSign c
  }
  deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosFields c toSign)
deriving instance TPraosCrypto c => Show (TPraosFields c toSign)

-- | Fields arising from transitional praos execution which must be included in
-- the block signature.
data TPraosToSign c = TPraosToSign
  { -- | Verification key for the issuer of this block. Note that unlike in Classic/BFT
    --   where we have a key for the genesis delegate on whose behalf we are
    --   issuing this block, this key corresponds to the stake pool/core node
    --   actually forging the block.
    tptsIssuerVK :: VerKeyDSIGN (DSIGN c)
  , tptsVrfVk :: VerKeyVRF (VRF c)
    -- | Verifiable result containing the updated nonce value.
  , tptsEta :: CertifiedVRF (VRF c) Nonce
    -- | Verifiable proof of the leader value, used to determine whether the
    -- node has the right to issue a block in this slot.
    --
    -- We include a value here even for blocks forged under the BFT schedule. It
    -- is not required that such a value be verifiable (though by default it
    -- will be verifiably correct, but unused.)
  , tptsLeader :: CertifiedVRF (VRF c) UnitInterval
    -- Lightweight delegation certificate mapping the cold (DSIGN) key to the
    -- online KES key.
  , tptsOCert :: OCert c
  }
  deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosToSign c)
deriving instance TPraosCrypto c => Show (TPraosToSign c)

class ( HasHeader hdr
      , SignedHeader hdr
      , TPraosCrypto c
      , Cardano.Crypto.KES.Class.Signable (TPraosKES c) (Signed hdr)
      ) => HeaderSupportsTPraos c hdr where

  -- Because we are using the executable spec, rather than implementing the
  -- protocol directly here, we have a fixed header type rather than an
  -- abstraction. So we must introduce this method.
  headerToBHeader
    :: proxy (TPraos c)
    -> hdr
    -> BHeader c

forgeTPraosFields :: ( HasNodeState (TPraos c) m
                    , MonadRandom m
                    , TPraosCrypto c
                    , Cardano.Crypto.KES.Class.Signable (TPraosKES c) toSign
                    )
                 => NodeConfig (TPraos c)
                 -> TPraosProof c
                 -> (TPraosToSign c -> toSign)
                 -> m (TPraosFields c toSign)
forgeTPraosFields TPraosNodeConfig{..}  TPraosProof{..} mkToSign = do
    let icn@TPraosIsCoreNode{..} = tpraosIsCoreNode
        (DiscVKey issuerVK) = ocertVkCold tpraosIsCoreNodeOpCert
        signedFields = TPraosToSign {
          tptsIssuerVK = issuerVK
        , tptsVrfVk = deriveVerKeyVRF tpraosSignKeyVRF
        , tptsEta = tpraosEta
        , tptsLeader = tpraosLeader
        , tptsOCert = tpraosIsCoreNodeOpCert
        }
    m <- signedKES
          ()
          (fromIntegral (unSlotNo tpraosProofSlot))
          (mkToSign signedFields)
          tpraosIsCoreNodeSKSHot
    case m of
      Nothing                  -> error "mkOutoborosPayload: signedKES failed"
      Just (signature, newKey) -> do
        putNodeState . Just $ icn { tpraosIsCoreNodeSKSHot = newKey }
        return $ TPraosFields {
            tpraosSignature    = signature
          , tpraosToSign       = signedFields
          }

{-------------------------------------------------------------------------------
  TPraos specific types
-------------------------------------------------------------------------------}

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TPraosProof c
  = TPraosProof
    { tpraosEta       :: CertifiedVRF (VRF c) Nonce
    , tpraosLeader    :: CertifiedVRF (VRF c) UnitInterval
    , tpraosProofSlot :: SlotNo
    , tpraosIsCoreNode :: TPraosIsCoreNode c
    } deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosProof c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data TPraos c

-- | TPraos parameters that are node independent
data TPraosParams = TPraosParams {
      -- | Active slots coefficient. This parameter represents the proportion of
      -- slots in which blocks should be issued. This can be interpreted as the
      -- probability that a party holding all the stake will be elected as
      -- leader for a given slot.
      tpraosLeaderF       :: Double
    , tpraosSecurityParam :: SecurityParam
    } deriving Generic

instance NoUnexpectedThunks TPraosParams

data TPraosIsCoreNode c = TPraosIsCoreNode
  { -- | Online KES key used to sign blocks.GHC.Generics
    tpraosIsCoreNodeSKSHot :: SignKeyKES (KES c)
    -- | Certificate delegating rights from the stake pool cold key (or genesis
    -- stakeholder delegate cold key) to the online KES key.
  , tpraosIsCoreNodeOpCert :: OCert c
  } deriving Generic

instance TPraosCrypto c => NoUnexpectedThunks (TPraosIsCoreNode c)

instance TPraosCrypto c => OuroborosTag (TPraos c) where

  protocolSecurityParam = tpraosSecurityParam . tpraosParams

  type NodeState       (TPraos c) = Maybe (TPraosIsCoreNode c)
  type LedgerView      (TPraos c) = API.LedgerView c
  type IsLeader        (TPraos c) = TPraosProof c
  type ValidationErr   (TPraos c) = [[STS.PredicateFailure (PRTCL c)]]
  type CanValidate     (TPraos c) = HeaderSupportsTPraos c
  type CanSelect       (TPraos c) = HasHeader
  type ChainState      (TPraos c) = ChainState.TPraosChainState c

  checkIsLeader cfg@TPraosNodeConfig{..} slot lv cs =
    getNodeState >>= \case
        Nothing -> return Nothing
        Just icn@TPraosIsCoreNode{ tpraosIsCoreNodeOpCert} -> do
          let mkSeed' = mkSeed @c
              vkhCold = hashKey $ ocertVkCold tpraosIsCoreNodeOpCert
              t = leaderThreshold cfg lv vkhCold
              eta0 = prtclStateEpochNonce $ ChainState.toPRTCLState cs
              prevHash = prtclStateHash @c $ ChainState.toPRTCLState cs
              rho' = mkSeed' seedEta (convertSlot slot) eta0 prevHash
              y' = mkSeed' seedL (convertSlot slot) eta0 prevHash
          rho <- evalCertified () rho' tpraosSignKeyVRF
          y   <- evalCertified () y'   tpraosSignKeyVRF
          -- First, check whether we're in the overlay schedule
          case (Map.lookup (convertSlot slot) $ lvOverlaySched lv) of
            Nothing -> return $
              -- Slot isn't in the overlay schedule, so we're in Praos
              if fromIntegral (certifiedNatural y) < t
                  then Just TPraosProof {
                          tpraosEta       = coerce rho
                        , tpraosLeader    = coerce y
                        , tpraosProofSlot = slot
                        , tpraosIsCoreNode = icn
                        }
                  else Nothing
            Just Nothing ->
              -- This is a non-active slot; nobody may produce a block
              return Nothing
            Just (Just gkhash) ->
              -- The given genesis key has authority to produce a block in this
              -- slot. Check whether we're its delegate.
              let GenDelegs dlgMap = lvGenDelegs lv
              in do
                let verKey = ocertVkCold tpraosIsCoreNodeOpCert
                return $ case Map.lookup gkhash dlgMap of
                  Just dlgHash | dlgHash == hashKey verKey ->
                    Just TPraosProof
                        { tpraosEta = coerce rho
                          -- Note that this leader value is not checked for
                          -- slots in the overlay schedule, so we could set it
                          -- to whatever we want. We evaluate it as normal for
                          -- simplicity's sake.
                        , tpraosLeader = coerce y
                        , tpraosProofSlot = slot
                        , tpraosIsCoreNode = icn
                        }
                  _ -> Nothing

  applyChainState TPraosNodeConfig{..} lv b cs = do
    let slot = blockSlot b
        SecurityParam (fromIntegral -> k) = tpraosSecurityParam tpraosParams

    newCS <- ExceptT . return $ applySTS @(PRTCL c)
      $ TRC ( STS.PrtclEnv
                (lvProtParams lv)
                (lvOverlaySched lv)
                (lvPoolDistr lv)
                (lvGenDelegs lv)
                (convertSlot slot)
                (isNewEpoch slot (ChainState.lastSlot cs))
            , ChainState.toPRTCLState cs
            , headerToBHeader (Proxy :: Proxy (TPraos c)) b
            )

    return . ChainState.prune k $ ChainState.appendState newCS cs

  -- Rewind the chain state
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainState TPraosNodeConfig{..} cs rewindTo = ChainState.rewind rewindTo cs

  -- NOTE: We redefine `preferCandidate` but NOT `compareCandidates`
  -- NOTE: See note regarding clock skew.
  preferCandidate TPraosNodeConfig{..} ours cand =
      AF.forksAtMostKBlocks k ours cand &&
      AF.compareHeadBlockNo cand ours == GT
    where
      TPraosParams{..} = tpraosParams

      k :: Word64
      k = maxRollbacks tpraosSecurityParam

-- Use generic instance
instance (VRFAlgorithm (TPraosVRF c)) => NoUnexpectedThunks (NodeConfig (TPraos c))

data instance NodeConfig (TPraos c) = TPraosNodeConfig
  { tpraosParams        :: TPraosParams
  , tpraosSignKeyVRF    :: SignKeyVRF (TPraosVRF c)
  } deriving Generic


phi :: NodeConfig (TPraos c) -> Rational -> Double
phi TPraosNodeConfig{..} r = 1 - (1 - tpraosLeaderF) ** fromRational r
  where
    TPraosParams{..} = tpraosParams

leaderThreshold :: forall c. TPraosCrypto c
                => NodeConfig (TPraos c)
                -> API.LedgerView c
                -> KeyHash c -- ^ Key hash of the pool
                -> Double
leaderThreshold nc lv kh =
    let PoolDistr pd = lvPoolDistr lv
        a = maybe 0 fst $ Map.lookup kh pd
    in  2 ^ (byteCount (Proxy :: Proxy (TPraosHash c)) * 8) * phi nc a

prtclStateHash
  :: STS.State (PRTCL c)
  -> BlockChain.HashHeader c
prtclStateHash (STS.PrtclState _ h _ _ _ _) = h

prtclStateEpochNonce
  :: STS.State (PRTCL c)
  -> Nonce
prtclStateEpochNonce (STS.PrtclState _ _ _ e _ _) = e

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense c, Condense toSign, TPraosCrypto c)
  => Condense (TPraosFields c toSign) where
  -- TODO Nicer 'Condense' instance
  condense = show
