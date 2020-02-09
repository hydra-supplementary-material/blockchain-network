{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Ouroboros.Storage.VolatileDB.Types
    (
      module Ouroboros.Storage.VolatileDB.Types
    , module Ouroboros.Network.Block
    ) where

import           Control.Exception (Exception (..))
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import           Data.Word (Word16, Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block hiding (Tip, decodeTip, encodeTip)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (IsEBB)

import           Ouroboros.Storage.FS.API.Types

-- | The 'FileId' is the unique identifier of each file found in the db.
-- For example, the file @blocks-42.dat@ has 'FileId' @42@.
type FileId = Int

-- | For each @blockId@, we store the file in which we can find the block,
-- the offset, its size in bytes and its predecessor.
type ReverseIndex blockId = Map blockId (InternalBlockInfo blockId)

-- | For each block, we store the set of all blocks which have this block as
-- a predecessor (set of successors).
type SuccessorsIndex blockId = Map (WithOrigin blockId) (Set blockId)

-- | When block validation is enabled, the parser checks for each block a number
-- of properties and stops parsing if it finds any invalid blocks.
data BlockValidationPolicy =
      NoValidation
    | ValidateAll
    deriving Eq

-- | Errors which might arise when working with this database.
data VolatileDBError =
      UserError UserError
    -- ^ An error thrown because of incorrect usage of the VolatileDB
    -- by the user.
    | UnexpectedError UnexpectedError
    -- ^ An unexpected error thrown because something went wrong on a lower
    -- layer.
    deriving Show

data UserError =
      InvalidArgumentsError String
    | ClosedDBError
    deriving (Show, Eq)

data UnexpectedError =
      FileSystemError FsError
    deriving (Show)

instance Eq VolatileDBError where
    (==) = sameVolatileDBError

instance Exception VolatileDBError where
    displayException = show

-- | This needs not be an 'Exception' instance, since we recover and don't
-- throw such errors.
data ParserError blockId e =
    BlockReadErr e
  | BlockCorruptedErr blockId
  | DuplicatedSlot blockId FsPath FsPath
  deriving (Eq, Show)

sameVolatileDBError :: VolatileDBError
                    -> VolatileDBError
                    -> Bool
sameVolatileDBError e1 e2 = case (e1, e2) of
    (UserError ue1, UserError ue2)             -> ue1 == ue2
    (UnexpectedError ue1, UnexpectedError ue2) -> sameUnexpectedError ue1 ue2
    _                                          -> False

sameUnexpectedError :: UnexpectedError
                    -> UnexpectedError
                    -> Bool
sameUnexpectedError e1 e2 = case (e1, e2) of
    (FileSystemError fs1, FileSystemError fs2) -> sameFsError fs1 fs2

newtype FileSize  = FileSize {unFileSize :: Word64}
    deriving (Show, Generic, NoUnexpectedThunks)
newtype BlockSize = BlockSize {unBlockSize :: Word64}
    deriving (Show, Generic, NoUnexpectedThunks)

newtype Parser e m blockId = Parser {
    -- | Parse block storage at the given path.
    parse :: FsPath -> m (ParsedInfo blockId, Maybe (ParserError blockId e))
    }

-- | The offset of a slot in a file.
type SlotOffset = Word64

-- | Information returned by the parser about a single file.
type ParsedInfo blockId = [ParsedBlockInfo blockId]

-- | Information returned by the parser about a single block.
--
-- The parser returns for each block, its offset, its size and its 'BlockInfo'
type ParsedBlockInfo blockId = (SlotOffset, (BlockSize, BlockInfo blockId))

-- | The information that the user has to provide for each new block.
data BlockInfo blockId = BlockInfo {
      bbid          :: !blockId
    , bslot         :: !SlotNo
    , bpreBid       :: !(WithOrigin blockId)
    , bisEBB        :: !IsEBB
    , bheaderOffset :: !Word16
    , bheaderSize   :: !Word16
    } deriving (Show, Generic, NoUnexpectedThunks)

-- | The internal information the db keeps for each block.
data InternalBlockInfo blockId = InternalBlockInfo {
      ibFile         :: !FsPath
    , ibSlotOffset   :: !SlotOffset
    , ibBlockSize    :: !BlockSize
    , ibSlot         :: !SlotNo
    , ibPreBid       :: !(WithOrigin blockId)
    , ibIsEBB        :: !IsEBB
    , ibHeaderOffset :: !Word16
    , ibHeaderSize   :: !Word16
    } deriving (Show, Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  Tracing
------------------------------------------------------------------------------}

data TraceEvent e blockId
    = DBAlreadyClosed
    | DBAlreadyOpen
    | BlockAlreadyHere blockId
    | TruncateCurrentFile FsPath
    | Truncate (ParserError blockId e) FsPath SlotOffset
    | InvalidFileNames [FsPath]
  deriving (Eq, Generic, Show)
