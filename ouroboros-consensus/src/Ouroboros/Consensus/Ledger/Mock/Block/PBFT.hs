{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Mock.Block.PBFT (
    SimplePBftBlock
  , SimplePBftHeader
  , SimplePBftExt(..)
  , SignedSimplePBft(..)
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Proxy
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (HasHeader (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Run
import           Ouroboros.Consensus.Protocol.ExtConfig
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit PBFT
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for PBFT
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type SimplePBftBlock c c' = SimpleBlock c (SimplePBftExt c c')

-- | Header for PBFT
type SimplePBftHeader c c' = SimpleHeader c (SimplePBftExt c c')

-- | Block extension required for PBFT
newtype SimplePBftExt c c' = SimplePBftExt {
      simplePBftExt :: PBftFields c' (SignedSimplePBft c c')
    }
  deriving (Generic, Condense, Show, Eq, NoUnexpectedThunks)

-- | Part of the block that gets signed
--
-- We just sign the standard header, i.e., without the PBFT extensions.
-- In particular, the signature does not cover the issuer.
--
-- The signature does not cover the body explicitly, but since the standard
-- header includes a hash of the body, the signature covers the body implicitly.
data SignedSimplePBft c c' = SignedSimplePBft {
      signedSimplePBft :: SimpleStdHeader c (SimplePBftExt c c')
    }
  deriving (Generic)

-- | PBFT requires the ledger view; for the mock ledger, this is constant
type instance BlockProtocol (SimplePBftBlock  c c') = ExtConfig (PBft c') (PBftLedgerView c')
type instance BlockProtocol (SimplePBftHeader c c') = BlockProtocol (SimplePBftBlock c c')

-- | Sanity check that block and header type synonyms agree
_simplePBftHeader :: SimplePBftBlock c c' -> SimplePBftHeader c c'
_simplePBftHeader = simpleHeader

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support PBFT
-------------------------------------------------------------------------------}

type instance Signed (SimplePBftHeader c c') = SignedSimplePBft c c'

instance SignedHeader (SimplePBftHeader c c') where
  headerSigned = SignedSimplePBft . simpleHeaderStd

instance (PBftCrypto c', Serialise (PBftVerKeyHash c'))
      => RunMockProtocol (ExtConfig (PBft c') ext) where
  mockProtocolMagicId  = const constructMockProtocolMagicId
  mockEncodeChainState = const CS.encodePBftChainState
  mockDecodeChainState = \cfg -> let k = pbftSecurityParam $
                                           pbftParams (extNodeConfigP cfg)
                                 in CS.decodePBftChainState k (pbftWindowSize k)

instance ( SimpleCrypto c
         , PBftCrypto c'
         , Signable (PBftDSIGN c') (SignedSimplePBft c c')
         , ConstructContextDSIGN ext c'
         , Serialise (PBftVerKeyHash c')
         ) => RunMockBlock (ExtConfig (PBft c') ext) c (SimplePBftExt c c') where
  forgeExt cfg isLeader SimpleBlock{..} = do
      ext :: SimplePBftExt c c' <- fmap SimplePBftExt $
        forgePBftFields
          (constructContextDSIGN (Proxy @c') (extNodeConfig cfg))
          isLeader
          SignedSimplePBft { signedSimplePBft = simpleHeaderStd }
      return SimpleBlock {
          simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
        , simpleBody   = simpleBody
        }
    where
      SimpleHeader{..} = simpleHeader

instance ( SimpleCrypto c
         , Signable MockDSIGN (SignedSimplePBft c PBftMockCrypto)
         ) => SupportedBlock (SimplePBftBlock c PBftMockCrypto) where
  validateView _     = pbftValidateRegular () (simplePBftExt . simpleHeaderExt)
  selectView   _ hdr = (blockNo hdr, IsNotEBB)

-- | The ledger view is constant for the mock instantiation of PBFT
-- (mock blocks cannot change delegation)
instance ( SimpleCrypto c
         , Signable MockDSIGN (SignedSimplePBft c PBftMockCrypto)
         ) => ProtocolLedgerView (SimplePBftBlock c PBftMockCrypto) where
  ledgerConfigView _ =
      SimpleLedgerConfig
  protocolLedgerView ExtNodeConfig{..} _ls =
      extNodeConfig
  anachronisticProtocolLedgerView ExtNodeConfig{..} _ _ =
      Right $ extNodeConfig

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance PBftCrypto c' => Serialise (SimplePBftExt c c') where
  encode (SimplePBftExt PBftFields{..}) = mconcat [
        encodeVerKeyDSIGN pbftIssuer
      , encodeVerKeyDSIGN pbftGenKey
      , encodeSignedDSIGN pbftSignature
      ]
  decode = do
      pbftIssuer    <- decodeVerKeyDSIGN
      pbftGenKey    <- decodeVerKeyDSIGN
      pbftSignature <- decodeSignedDSIGN
      return $ SimplePBftExt PBftFields{..}

instance SimpleCrypto c => Serialise (SignedSimplePBft c c')
instance (Typeable c', SimpleCrypto c) => ToCBOR (SignedSimplePBft c c') where
  toCBOR = encode
