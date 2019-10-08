{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    LeaderSchedule (..)
  , WithLeaderSchedule
  , NodeConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (Empty)
import           Ouroboros.Consensus.Util.Condense (Condense (..))

newtype LeaderSchedule = LeaderSchedule {getLeaderSchedule :: Map SlotNo [CoreNodeId]}
    deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (NoUnexpectedThunks)

instance Condense LeaderSchedule where
    condense (LeaderSchedule m) = condense $ Map.toList m

-- | Extension of protocol @p@ by a static leader schedule.
data WithLeaderSchedule p

instance OuroborosTag p => OuroborosTag (WithLeaderSchedule p) where

  type ChainState      (WithLeaderSchedule p) = ()
  type NodeState       (WithLeaderSchedule p) = ()
  type LedgerView      (WithLeaderSchedule p) = ()
  type ValidationErr   (WithLeaderSchedule p) = ()
  type IsLeader        (WithLeaderSchedule p) = ()
  type SupportedHeader (WithLeaderSchedule p) = Empty

  data NodeConfig (WithLeaderSchedule p) = WLSNodeConfig
    { lsNodeConfigSchedule :: !LeaderSchedule
    , lsNodeConfigP        :: !(NodeConfig p)
    , lsNodeConfigNodeId   :: !NodeId
    }
    deriving (Generic)

  preferCandidate       WLSNodeConfig{..} = preferCandidate       lsNodeConfigP
  compareCandidates     WLSNodeConfig{..} = compareCandidates     lsNodeConfigP
  protocolSecurityParam WLSNodeConfig{..} = protocolSecurityParam lsNodeConfigP

  checkIsLeader WLSNodeConfig{..} slot _ _ = return $
      case lsNodeConfigNodeId of
        RelayId _rid -> Nothing
        CoreId   cid -> case Map.lookup slot sched of
          Nothing             -> Nothing
          Just cids
            | cid `elem` cids -> Just ()
            | otherwise       -> Nothing
    where
      sched = getLeaderSchedule lsNodeConfigSchedule

  applyChainState _ _ _ _ = return ()
  rewindChainState _ _ _  = Just ()

instance OuroborosTag p => NoUnexpectedThunks (NodeConfig (WithLeaderSchedule p))
