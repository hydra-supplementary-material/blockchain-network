{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Protocol.Driver where

import Data.Text (Text)

import Protocol.Channel
import Protocol.Core
import Protocol.Codec

-- |
-- = Driving a Peer by was of a Duplex and Channel
--
-- A 'Duplex' allows for sending and receiving pieces of some concrete type.
-- In applications, this will probably be some sort of socket. In order to
-- use it to drive a typed protocol application (represented by a 'Peer'),
-- there must be a way to encode typed transitions of that protocol to the
-- concrete type, and to parse pieces of that concrete type incrementally into
-- a typed transition. This is defined by a 'Codec'.
--
-- A 'Codec' and a 'Duplex' alone is not enough to do encoding and decoding,
-- because the 'Codec' does not make any _decisions_ about the way in which
-- the protocol application progresses. It defines encodings for _all_ possible
-- transitions from a state, and an inverse for that encoder. It's the 'Peer'
-- term which decides which transitions to encode, thereby leading the 'Codec'
-- through a path in the protocol type.
--
-- Driving a 'Peer' in this way may give rise to an exception, given by
-- 'Unexpected :: Result t'.

-- | The outcome of a 'Peer' when driven by a 'Duplex' and 'Codec'.
-- It's possible that an unexpected transition was given, either because the
-- other end made a protocol error, or because the 'Codec' is not coherent
-- (decode is not inverse to encode). It's also possible that a decoder
-- fails because a transition was expected, but the 'Duplex' closed. This also
-- gives rise to an 'Unexpected' value, because the decoder will fail.
data Result t where
  Normal     :: t -> Result t
  -- | Unexpected data was given. This includes the case of an EOF.
  Unexpected :: Text -> Result t
  deriving (Show)

-- | Drive a 'Peer' using a 'Duplex', by way of a 'Codec' which describes
-- the relationship between the concrete representation understood by the
-- 'Duplex', and the typed transitions understood by the 'Peer'.
--
-- A failure to decode arises as an 'Unexpected :: Result t'.
useCodecWithDuplex
 :: forall m concreteSend concreteRecv p tr status init end t .
    ( Monad m )
 => Duplex m m concreteSend concreteRecv
 -> Codec m concreteSend concreteRecv tr init
 -> Peer p tr (status init) end m t
 -> m (Result t)
useCodecWithDuplex = go Nothing
  where
  -- Tracks leftovers from the duplex.
  go :: forall status state .
        Maybe concreteRecv
     -> Duplex m m concreteSend concreteRecv
     -> Codec m concreteSend concreteRecv tr state
     -> Peer p tr (status state) end m t
     -> m (Result t)
  go leftovers duplex codec peer = case peer of
    PeerDone t -> pure $ Normal t
    PeerLift m -> m >>= go leftovers duplex codec
    -- Encode the transition, dump it to the duplex, and continue with the
    -- new codec and duplex.
    PeerYield exc next -> do
      let enc = runEncoder (encode codec) (exchangeTransition exc)
          codec' = encCodec enc
      duplex' <- send duplex (representation enc)
      go leftovers duplex' codec' next
    -- Awaiting is more complex than yielding, because we need to deal with
    -- the possibility of leftovers.
    -- Alternatively, we could redefine the 'Duplex' type so that it has a
    -- way to push leftovers back onto the channel.
    PeerAwait k -> runDecoder (decode codec) >>= startDecoding leftovers duplex k

  startDecoding
    :: forall state .
       Maybe concreteRecv
    -> Duplex m m concreteSend concreteRecv
    -> (forall inter . tr state inter -> Peer p tr (ControlNext (TrControl p state inter) Awaiting Yielding Finished inter) end m t)
    -> DecoderStep m concreteRecv (Decoded tr state (Codec m concreteSend concreteRecv tr))
    -> m (Result t)
  startDecoding leftovers duplex k step = case step of
    Fail _ txt -> pure $ Unexpected txt
    -- We just started decoding. We haven't fed any input yet. And still, the
    -- decoder is done. That's bizarre but not necessarily wrong. It _must_ be
    -- the case that `leftovers'` is empty, otherwise the decoder conjured
    -- some leftovers from nothing.
    Done leftovers' (Decoded tr codec') -> go leftovers duplex codec' (k tr)
    -- Typically, the decoder will be partial. We start by passing the
    -- leftovers if any. NB: giving `Nothing` to `l` means there's no more
    -- input.
    Partial l -> case leftovers of
      Just piece -> runDecoder (l (Just piece)) >>= decodeFromDuplex duplex k
      Nothing -> decodeFromDuplex duplex k step

  decodeFromDuplex
    :: forall state .
       Duplex m m concreteSend concreteRecv
    -> (forall inter . tr state inter -> Peer p tr (ControlNext (TrControl p state inter) Awaiting Yielding Finished inter) end m t)
    -> DecoderStep m concreteRecv (Decoded tr state (Codec m concreteSend concreteRecv tr))
    -> m (Result t)
  decodeFromDuplex duplex k step = case step of
    Fail _ txt -> pure $ Unexpected txt
    -- Leftovers are given as 'Just' even if they are empty.
    -- Not ideal but shouldn't be a problem in practice.
    Done leftovers (Decoded tr codec') -> go leftovers duplex codec' (k tr)
    -- Read from the duplex and carry on. A premature end-of-input will
    -- result in a Fail and therefore an Unexpected.
    Partial l -> recv duplex >>= \next -> case next of
      (mPiece, duplex') ->
        runDecoder (l mPiece) >>= decodeFromDuplex duplex' k