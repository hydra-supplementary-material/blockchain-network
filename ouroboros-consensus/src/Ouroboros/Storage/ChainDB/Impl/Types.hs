{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
-- | Types used throughout the implementation: handle, state, environment,
-- types, trace types, etc.
module Ouroboros.Storage.ChainDB.Impl.Types (
    ChainDbHandle (..)
  , getEnv
  , getEnv1
  , getEnv2
  , getEnvSTM
  , ChainDbState (..)
  , ChainDbEnv (..)
    -- * Exposed internals for testing purposes
  , Internal (..)
  , intReopen
    -- * Iterator-related
  , IteratorKey (..)
    -- * Reader-related
  , ReaderKey (..)
  , ReaderHandle (..)
  , ReaderState (..)
  , ReaderRollState (..)
  , readerRollStatePoint
    -- * Invalid blocks
  , InvalidBlocks
  , InvalidBlockInfo (..)
    -- * Trace types
  , TraceEvent (..)
  , TraceAddBlockEvent (..)
  , TraceReaderEvent (..)
  , TraceCopyToImmDBEvent (..)
  , TraceGCEvent (..)
  , TraceValidationEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceOpenEvent (..)
  , TraceIteratorEvent (..)
  ) where

import           Control.Tracer
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict (Map)
import           Data.Time.Clock (DiffTime)
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack, callStack)

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..))

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (BlockNo, HasHeader, HeaderHash, Point,
                     SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (BlockProtocol, Header, IsEBB (..))
import           Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import           Ouroboros.Consensus.Ledger.Abstract (ProtocolLedgerView)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError)
import           Ouroboros.Consensus.Protocol.Abstract (NodeConfig)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (WithFingerprint)

import           Ouroboros.Storage.Common (EpochNo)
import           Ouroboros.Storage.EpochInfo (EpochInfo)

import           Ouroboros.Storage.ChainDB.API (ChainDbError (..),
                     InvalidBlockReason, StreamFrom, StreamTo, UnknownRange)

import           Ouroboros.Storage.ChainDB.Impl.ImmDB (ImmDB)
import qualified Ouroboros.Storage.ChainDB.Impl.ImmDB as ImmDB
import           Ouroboros.Storage.ChainDB.Impl.LgrDB (LgrDB)
import qualified Ouroboros.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Storage.ChainDB.Impl.VolDB (VolDB)

-- | A handle to the internal ChainDB state
newtype ChainDbHandle m blk = CDBHandle (StrictTVar m (ChainDbState m blk))

-- | Check if the ChainDB is open, if so, executing the given function on the
-- 'ChainDbEnv', otherwise, throw a 'CloseDBError'.
getEnv :: forall m blk r. (IOLike m, HasCallStack)
       => ChainDbHandle m blk
       -> (ChainDbEnv m blk -> m r)
       -> m r
getEnv (CDBHandle varState) f = atomically (readTVar varState) >>= \case
    ChainDbOpen    env -> f env
    ChainDbClosed _env -> throwM $ ClosedDBError callStack
    -- See the docstring of 'ChainDbReopening'
    ChainDbReopening   -> error "ChainDB used while reopening"

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 :: (IOLike m, HasCallStack)
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> m r)
        -> a -> m r
getEnv1 h f a = getEnv h (\env -> f env a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 :: (IOLike m, HasCallStack)
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> b -> m r)
        -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)


-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM :: forall m blk r. (IOLike m, HasCallStack)
          => ChainDbHandle m blk
          -> (ChainDbEnv m blk -> STM m r)
          -> STM m r
getEnvSTM (CDBHandle varState) f = readTVar varState >>= \case
    ChainDbOpen    env -> f env
    ChainDbClosed _env -> throwM $ ClosedDBError callStack
    -- See the docstring of 'ChainDbReopening'
    ChainDbReopening   -> error "ChainDB used while reopening"

data ChainDbState m blk
  = ChainDbOpen   !(ChainDbEnv m blk)
  | ChainDbClosed !(ChainDbEnv m blk)
    -- ^ Note: this 'ChainDbEnv' will only be used to reopen the ChainDB.
  | ChainDbReopening
    -- ^ The ChainDB is being reopened, this should not be performed
    -- concurrently with any other operations, including reopening itself.
    --
    -- This state can only be reached by the 'intReopen' function, which is an
    -- internal function only exposed for testing. During normal use of the
    -- 'ChainDB', it should /never/ be used.
  deriving (Generic, NoUnexpectedThunks)

data ChainDbEnv m blk = CDB
  { cdbImmDB           :: !(ImmDB m blk)
  , cdbVolDB           :: !(VolDB m blk)
  , cdbLgrDB           :: !(LgrDB m blk)
  , cdbChain           :: !(StrictTVar m (AnchoredFragment (Header blk)))
    -- ^ Contains the current chain fragment.
    --
    -- INVARIANT: the anchor point of this fragment is the tip of the
    -- ImmutableDB. This implies that this fragment never contains any blocks
    -- that are stored in the immutable DB.
    --
    -- Note that this fragment might be shorter than @k@ headers when the
    -- whole chain is shorter than @k@ or in case of corruption of the
    -- VolatileDB.
    --
    -- Note that this fragment might also be /longer/ than @k@ headers,
    -- because the oldest blocks from the fragment might not yet have been
    -- copied from the VolatileDB to the ImmutableDB.
    --
    -- The anchor point of this chain should be the most recent \"immutable\"
    -- block according to the protocol, i.e., a block that cannot be rolled
    -- back.
    --
    -- Note that the \"immutable\" block isn't necessarily at the tip of the
    -- ImmutableDB, but could temporarily still be on the in-memory chain
    -- fragment. When the background thread that copies blocks to the
    -- ImmutableDB has caught up, the \"immutable\" block will be at the tip
    -- of the ImmutableDB again.
    --
    -- Note that the \"immutable\" block might be less than @k@ blocks from
    -- our tip in case the whole chain is shorter than @k@ or in case of
    -- corruption of the VolatileDB.
    --
    -- Note that the \"immutable\" block will /never/ be /more/ than @k@
    -- blocks back, as opposed to the anchor point of 'cdbChain'.
  , cdbIterators       :: !(StrictTVar m (Map IteratorKey (m ())))
    -- ^ The iterators.
    --
    -- This maps the 'IteratorKey's of each open 'Iterator' to a function
    -- that, when called, closes the iterator. This is used when closing the
    -- ChainDB: the open file handles used by iterators can be closed, and the
    -- iterators themselves are closed so that it is impossible to use an
    -- iterator after closing the ChainDB itself.
  , cdbReaders         :: !(StrictTVar m (Map ReaderKey (ReaderHandle m blk)))
    -- ^ The readers.
    --
    -- A reader is open iff its 'ReaderKey' is this 'Map'.
    --
    -- INVARIANT: the 'readerPoint' of each reader is 'withinFragmentBounds'
    -- of the current chain fragment (retrieved 'cdbGetCurrentChain', not by
    -- reading 'cdbChain' directly).
  , cdbNodeConfig      :: !(NodeConfig (BlockProtocol blk))
  , cdbInvalid         :: !(StrictTVar m (WithFingerprint (InvalidBlocks blk)))
    -- ^ See the docstring of 'InvalidBlocks'.
    --
    -- The 'Fingerprint' changes every time a hash is added to the map, but
    -- not when hashes are garbage-collected from the map.
  , cdbNextIteratorKey :: !(StrictTVar m IteratorKey)
  , cdbNextReaderKey   :: !(StrictTVar m ReaderKey)
  , cdbCopyLock        :: !(StrictMVar m ())
    -- ^ Lock used to ensure that 'copyToImmDB' is not executed more than
    -- once concurrently.
    --
    -- Note that 'copyToImmDB' can still be executed concurrently with all
    -- others functions, just not with itself.
  , cdbTracer          :: !(Tracer m (TraceEvent blk))
  , cdbTraceLedger     :: !(Tracer m (LgrDB.LedgerDB blk))
  , cdbRegistry        :: !(ResourceRegistry m)
    -- ^ Resource registry that will be used to (re)start the background
    -- threads, see 'cdbBgThreads'.
  , cdbGcDelay         :: !DiffTime
    -- ^ How long to wait between copying a block from the VolatileDB to
    -- ImmutableDB and garbage collecting it from the VolatileDB
  , cdbKillBgThreads   :: !(StrictTVar m (m ()))
    -- ^ A handle to kill the background threads.
  , cdbEpochInfo       :: !(EpochInfo m)
  , cdbIsEBB           :: !(Header blk -> IsEBB)
  , cdbBlockchainTime  :: !(BlockchainTime m)
  , cdbFutureBlocks    :: !(StrictTVar m (Map SlotNo (NonEmpty (Header blk))))
    -- ^ Scheduled chain selections for blocks with a slot in the future.
    --
    -- When a block with slot @s@, which is > the current slot is added, we
    -- add its header to the front of the list of headers stored under its
    -- slot number. We prepend to the list, so the headers are in reverse
    -- order w.r.t. the order in which they were added.
    --
    -- INVARIANT: all slots in the map are > the current slot.
  } deriving (Generic)

-- | We include @blk@ in 'showTypeOf' because it helps resolving type families
-- (but avoid including @m@ because we cannot impose @Typeable m@ as a
-- constraint and still have it work with the simulator)
instance (IOLike m, ProtocolLedgerView blk)
      => NoUnexpectedThunks (ChainDbEnv m blk) where
    showTypeOf _ = "ChainDbEnv m " ++ show (typeRep (Proxy @blk))

{-------------------------------------------------------------------------------
  Exposed internals for testing purposes
-------------------------------------------------------------------------------}

data Internal m blk = Internal
  { intReopen_                 :: HasCallStack => Bool -> m ()
    -- ^ Reopen a closed ChainDB.
    --
    -- A no-op if the ChainDB is still open.
    --
    -- NOTE: not thread-safe, no other operation should be called on the
    -- ChainDB at the same time.
    --
    -- The 'Bool' arguments indicates whether the background tasks should be
    -- relaunched after reopening the ChainDB.
  , intCopyToImmDB             :: m (WithOrigin SlotNo)
    -- ^ Copy the blocks older than @k@ from to the VolatileDB to the
    -- ImmutableDB and update the in-memory chain fragment correspondingly.
    --
    -- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
    -- returned. This can be used for a garbage collection on the VolatileDB.
  , intGarbageCollect          :: SlotNo -> m ()
    -- ^ Perform garbage collection for blocks <= the given 'SlotNo'.
  , intUpdateLedgerSnapshots   :: m ()
    -- ^ Write a new LedgerDB snapshot to disk and remove the oldest one(s).
  , intScheduledChainSelection :: SlotNo -> m ()
    -- ^ Run the scheduled chain selections for the given 'SlotNo'.
  , intKillBgThreads           :: StrictTVar m (m ())
      -- ^ A handle to kill the background threads.
  }

-- | Wrapper around 'intReopen_' to guarantee HasCallStack
intReopen :: HasCallStack => Internal m blk -> Bool -> m ()
intReopen = intReopen_

{-------------------------------------------------------------------------------
  Iterator-related
-------------------------------------------------------------------------------}

-- | We use this internally to track iterators in a map ('cdbIterators') in
-- the ChainDB state so that we can remove them from the map when the iterator
-- is closed.
--
-- We store them in the map so that the ChainDB can close all open iterators
-- when it is closed itself.
newtype IteratorKey = IteratorKey Word
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Reader-related
-------------------------------------------------------------------------------}

-- Note: these things are not in the Reader module, because 'TraceEvent'
-- depends on them, 'ChainDbEnv.cdbTracer' depends on 'TraceEvent', and most
-- modules depend on 'ChainDbEnv'. Also, 'ChainDbEnv.cdbReaders' depends on
-- 'ReaderState'.

-- | We use this internally to track reader in a map ('cdbReaders') in the
-- ChainDB state so that we can remove them from the map when the reader is
-- closed.
--
-- We store them in the map so that the ChainDB can close all open readers
-- when it is closed itself and to update the readers in case we switch to a
-- different chain.
newtype ReaderKey = ReaderKey Word
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, NoUnexpectedThunks)

-- | Internal handle to a 'Reader' without an explicit @b@ (@blk@, @'Header'
-- blk@, etc.) parameter so 'Reader's with different' @b@s can be stored
-- together in 'cdbReaders'.
data ReaderHandle m blk = ReaderHandle
  { rhSwitchFork :: Point blk -> AnchoredFragment (Header blk) -> STM m ()
    -- ^ When we have switched to a fork, all open 'Reader's must be notified.
  , rhClose      :: m ()
    -- ^ When closing the ChainDB, we must also close all open 'Reader's, as
    -- they might be holding on to resources.
    --
    -- Call 'rhClose' will release the resources used by the 'Reader'.
    --
    -- NOTE the 'Reader' is not removed from 'cdbReaders'. (That is done by
    -- 'closeAllReaders').
  }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "ReaderHandle" (ReaderHandle m blk)

-- | @b@ corresponds to the 'BlockComponent' that is being read.
data ReaderState m blk b
  = ReaderInit
    -- ^ The 'Reader' is in its initial state. Its 'ReaderRollState' is
    -- @'RollBackTo' 'genesisPoint'@.
    --
    -- This is equivalent to having a 'ReaderInImmDB' with the same
    -- 'ReaderRollState' and an iterator streaming after genesis. Opening such
    -- an iterator has a cost (index files will have to be read). However, in
    -- most cases, right after opening a Reader, the user of the Reader will
    -- try to move it forward, moving it from genesis to a more recent point
    -- on the chain. So we incur the cost of opening the iterator while not
    -- even using it.
    --
    -- Therefore, we have this extra initial state, that avoids this cost.
    -- When the user doesn't move the Reader forward, an iterator is opened.
  | ReaderInImmDB !(ReaderRollState blk)
                  !(ImmDB.Iterator (HeaderHash blk) m (Point blk, b))
    -- ^ The 'Reader' is reading from the ImmutableDB.
    --
    -- Note that the iterator includes 'Point blk' in addition to @b@, as it
    -- is needed to keep track of where the iterator is.
  | ReaderInMem   !(ReaderRollState blk)
    -- ^ The 'Reader' is reading from the in-memory current chain fragment.
  deriving (Generic, NoUnexpectedThunks)

-- | Similar to 'Ouroboros.Network.MockChain.ProducerState.ReaderState'.
data ReaderRollState blk
  = RollBackTo      !(Point blk)
    -- ^ The reader should roll back to this point.
  | RollForwardFrom !(Point blk)
    -- ^ The reader should roll forward from this point.
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

-- | Get the point the 'ReaderRollState' should roll back to or roll forward
-- from.
readerRollStatePoint :: ReaderRollState blk -> Point blk
readerRollStatePoint (RollBackTo      pt) = pt
readerRollStatePoint (RollForwardFrom pt) = pt

{-------------------------------------------------------------------------------
  Invalid blocks
-------------------------------------------------------------------------------}

-- | Hashes corresponding to invalid blocks. This is used to ignore these
-- blocks during chain selection.
type InvalidBlocks blk = Map (HeaderHash blk) (InvalidBlockInfo blk)

-- | In addition to the reason why a block is invalid, the slot number of the
-- block is stored, so that whenever a garbage collection is performed on the
-- VolatileDB for some slot @s@, the hashes older or equal to @s@ can be
-- removed from this map.
data InvalidBlockInfo blk = InvalidBlockInfo
  { invalidBlockReason :: !(InvalidBlockReason blk)
  , invalidBlockSlotNo :: !SlotNo
  } deriving (Eq, Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

-- | Trace type for the various events of the ChainDB.
data TraceEvent blk
  = TraceAddBlockEvent     (TraceAddBlockEvent     blk)
  | TraceReaderEvent       (TraceReaderEvent       blk)
  | TraceCopyToImmDBEvent  (TraceCopyToImmDBEvent  blk)
  | TraceGCEvent           (TraceGCEvent           blk)
  | TraceInitChainSelEvent (TraceInitChainSelEvent blk)
  | TraceOpenEvent         (TraceOpenEvent         blk)
  | TraceIteratorEvent     (TraceIteratorEvent     blk)
  | TraceLedgerEvent       (LgrDB.TraceEvent (Point blk))
  | TraceLedgerReplayEvent (LgrDB.TraceLedgerReplayEvent blk)
  | TraceImmDBEvent        (ImmDB.TraceEvent       blk)
  deriving (Generic)

deriving instance
  ( HasHeader blk
  , Eq (Header blk)
  , ProtocolLedgerView blk
  ) => Eq (TraceEvent blk)
deriving instance
  ( HasHeader blk
  , Show (Header blk)
  , ProtocolLedgerView blk
  ) => Show (TraceEvent blk)

data TraceOpenEvent blk
  = OpenedDB
    { _immTip   :: Point blk
    , _chainTip :: Point blk
    }
    -- ^ The ChainDB was opened.
  | ClosedDB
    { _immTip   :: Point blk
    , _chainTip :: Point blk
    }
    -- ^ The ChainDB was closed.
  | ReopenedDB
    { _immTip   :: Point blk
    , _chainTip :: Point blk
    }
    -- ^ The ChainDB was successfully reopened.
  | OpenedImmDB
    { _immDbTip      :: Point blk
    , _immDbTipEpoch :: EpochNo
    }
    -- ^ The ImmutableDB was opened.
  | OpenedVolDB
    -- ^ The VolatileDB was opened.
  | OpenedLgrDB
    -- ^ The LedgerDB was opened.
  deriving (Generic, Eq, Show)

-- | Trace type for the various events that occur when adding a block.
data TraceAddBlockEvent blk
  = IgnoreBlockOlderThanK (Point blk)
    -- ^ A block with a 'BlockNo' more than @k@ back than the current tip was
    -- ignored.

  | IgnoreBlockAlreadyInVolDB (Point blk)
    -- ^ A block that is already in the Volatile DB was ignored.

  | IgnoreInvalidBlock (Point blk) (InvalidBlockReason blk)
    -- ^ A block that is know to be invalid was ignored.

  | BlockInTheFuture (Point blk) SlotNo
    -- ^ The block is from the future, i.e., its slot number is greater than
    -- the current slot (the second argument).

  | AddedBlockToVolDB    !(Point blk) !BlockNo !IsEBB
    -- ^ A block was added to the Volatile DB

  | TryAddToCurrentChain (Point blk)
    -- ^ The block fits onto the current chain, we'll try to use it to extend
    -- our chain.

  | TrySwitchToAFork     (Point blk) (NonEmpty (HeaderHash blk))
    -- ^ The block fits onto some fork, we'll try to switch to that fork (if
    -- it is preferable to our chain).

  | StoreButDontChange   (Point blk)
    -- ^ The block doesn't fit onto any other block, so we store it and ignore
    -- it.

  | SwitchedToChain
    { _prevChain :: AnchoredFragment (Header blk)
    , _newChain  :: AnchoredFragment (Header blk)
    }
    -- ^ We successfully installed a new chain.

  | ChainChangedInBg
    { _prevChain :: AnchoredFragment (Header blk)
    , _newChain  :: AnchoredFragment (Header blk)
    }
    -- ^ We have found a new chain, but the current chain has changed in the
    -- background such that our new chain is no longer preferable to the
    -- current chain.

  | AddBlockValidation (TraceValidationEvent blk)
    -- ^ An event traced during validating performed while adding a block.

  | ScheduledChainSelection (Point blk) SlotNo Word64
    -- ^ A chain selection was scheduled in the future for the given block (at
    -- its slot number). The current slot number and the total number of
    -- scheduled chain selections is included.

  | RunningScheduledChainSelection (NonEmpty (Point blk)) SlotNo Word64
    -- ^ Scheduled chain selections are executed for the blocks corresponding
    -- to the given points at the given current slot number. The total number
    -- of scheduled chain selections is included.
  deriving (Generic)

deriving instance
  ( HasHeader                 blk
  , Eq (Header                blk)
  , ProtocolLedgerView        blk
  ) => Eq (TraceAddBlockEvent blk)
deriving instance
  ( HasHeader                   blk
  , Show (Header                blk)
  , ProtocolLedgerView          blk
  ) => Show (TraceAddBlockEvent blk)

data TraceValidationEvent blk
  = InvalidBlock
    { _validationErr :: ExtValidationError blk
    , _invalidPoint  :: Point blk
    }
    -- ^ A point was found to be invalid.

  | InvalidCandidate
    { _candidate     :: AnchoredFragment (Header blk)
    }
    -- ^ A candidate chain was invalid.

  | ValidCandidate (AnchoredFragment (Header blk))
    -- ^ A candidate chain was valid.

  | CandidateExceedsRollback
    { _supportedRollback :: Word64
    , _candidateRollback :: Word64
    , _candidate         :: AnchoredFragment (Header blk)
    }
    -- ^ Candidate required rollback past what LedgerDB supported
    --
    -- This should only happen in exceptional circumstances (like after
    -- disk corruption).
  deriving (Generic)

deriving instance
  ( HasHeader                 blk
  , Eq (Header                blk)
  , ProtocolLedgerView        blk
  ) => Eq (TraceValidationEvent blk)
deriving instance
  ( Show (Header                blk)
  , ProtocolLedgerView          blk
  ) => Show (TraceValidationEvent blk)

data TraceInitChainSelEvent blk
  = InitChainSelValidation (TraceValidationEvent blk)
    -- ^ An event traced during validation performed while performing initial
    -- chain selection.
  deriving (Generic)

deriving instance
  ( HasHeader                     blk
  , Eq (Header                    blk)
  , ProtocolLedgerView            blk
  ) => Eq (TraceInitChainSelEvent blk)
deriving instance
  ( Show (Header                    blk)
  , ProtocolLedgerView              blk
  ) => Show (TraceInitChainSelEvent blk)


data TraceReaderEvent blk
  = NewReader
    -- ^ A new reader was created.

  | ReaderNoLongerInMem (ReaderRollState blk)
    -- ^ The reader was in the 'ReaderInMem' state but its point is no longer
    -- on the in-memory chain fragment, so it has to switch to the
    -- 'ReaderInImmDB' state.

  | ReaderSwitchToMem
    { _readerPoint      :: Point blk
    , _slotNoAtImmDBTip :: WithOrigin SlotNo
    }
    -- ^ The reader was in the 'ReaderInImmDB' state and is switched to the
    -- 'ReaderInMem' state.

  | ReaderNewImmIterator
    { _readerPoint      :: Point blk
    , _slotNoAtImmDBTip :: WithOrigin SlotNo
    }
    -- ^ The reader is in the 'ReaderInImmDB' state but the iterator is
    -- exhausted while the ImmutableDB has grown, so we open a new iterator to
    -- stream these blocks too.
  deriving (Generic, Eq, Show)


data TraceCopyToImmDBEvent blk
  = CopiedBlockToImmDB (Point blk)
    -- ^ A block was successfully copied to the ImmutableDB.
  | NoBlocksToCopyToImmDB
    -- ^ There are no block to copy to the ImmutableDB.
  deriving (Generic, Eq, Show)

data TraceGCEvent blk
  = ScheduledGC SlotNo DiffTime
    -- ^ A garbage collection for the given 'SlotNo' was scheduled to happen
    -- after the given delay.
  | PerformedGC SlotNo
    -- ^ A garbage collection for the given 'SlotNo' was performed.
  deriving (Generic, Eq, Show)

data TraceIteratorEvent blk
  = UnknownRangeRequested (UnknownRange blk)
    -- ^ An unknown range was requested, see 'UnknownRange'.
  | StreamFromVolDB
    { _streamFrom :: StreamFrom blk
    , _streamTo   :: StreamTo   blk
    , _hashes     :: [HeaderHash blk]
    }
    -- ^ Stream only from the VolatileDB.
  | StreamFromImmDB
    { _streamFrom :: StreamFrom blk
    , _streamTo   :: StreamTo   blk
    }
    -- ^ Stream only from the ImmutableDB.
  | StreamFromBoth
    { _streamFrom :: StreamFrom blk
    , _streamTo   :: StreamTo   blk
    , _hashes     :: [HeaderHash blk]
    }
    -- ^ Stream from both the VolatileDB and the ImmutableDB.
  | BlockMissingFromVolDB (HeaderHash blk)
    -- ^ A block is no longer in the VolatileDB because it has been garbage
    -- collected. It might now be in the ImmutableDB if it was part of the
    -- current chain.
  | BlockWasCopiedToImmDB (HeaderHash blk)
    -- ^ A block that has been garbage collected from the VolatileDB is now
    -- found and streamed from the ImmutableDB.
  | BlockGCedFromVolDB    (HeaderHash blk)
    -- ^ A block is no longer in the VolatileDB and isn't in the ImmutableDB
    -- either; it wasn't part of the current chain.
  | SwitchBackToVolDB
    -- ^ We have stream one or more blocks from the ImmutableDB that were part
    -- of the VolatileDB when initialising the iterator. Now, we have to look
    -- back in the VolatileDB again because the ImmutableDB doesn't have the
    -- next block we're looking for.
  deriving (Generic, Eq, Show)
