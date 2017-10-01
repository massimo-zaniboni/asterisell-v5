{-# LANGUAGE UnboxedTuples, FlexibleInstances, ScopedTypeVariables, BangPatterns, ExistentialQuantification, RankNTypes  #-}

{- $LICENZE 2017
 * Copyright (C) 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 ofstributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
-}

-- | Manage collaborating threads.
--
module Asterisell.Process (
   Chan
 , readChan
 , writeChan
 , writeEOFChan
 , newChan
 , isEOFChan
 , processChan
 , safeBracket
 , writeStreamToChan
 , streamReadN
 , stream_connectWithMapAndFoldM
 ) where

import Asterisell.Error
import Asterisell.Utils

import GHC.Generics (Generic)
import Debug.Trace
import Data.List as L

import Control.Monad as M (mapM, mapM_, when)
import qualified Control.Concurrent.Chan.Unagi.Bounded as UChan
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Control.DeepSeq as DeepSeq
import Data.Vector as V hiding ((++))

import System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import System.IO.Streams.Attoparsec
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S

-- ----------------------------------
-- Safe channels

-- | A channel of fully evaluated (NFData) values.
--   The channel is bounded so RAM is not wasted in case of producer faster than consumer.
--   The channel is Unagi, for avoiding strange deadlocks in case of default bounded channels.
--   Unagi channels are faster, but they are not portable outside x86 architecture.
newtype Chan a = Chan (UChan.InChan (V.Vector a), UChan.OutChan (V.Vector a))

-- | A channel bounded to number of cores * 4, in order to give some buffering.
--   The channel is meant to include chunks of values, and not single values.
--   An empty chunk signal that the channel will be closed, and there will be no any more values.
newChan
    ::  Maybe Int
    -- ^ the bound of the channel. By default it is the number of cores * 4
    -> IO (Chan a)
newChan maybeBound = do
  cores <- getNumCapabilities
  let dim = case maybeBound of
              Nothing -> cores * 4
              Just r -> r
  Chan <$> UChan.newChan dim

{-# INLINE writeChan #-}
-- | Fully evaluate the batched values, and write them on the channel.
--   Empty vectors will not sent, because they are the EOF signal.
--   For closing a channel use `writeEOFChan`.
writeChan :: (NFData a) => Chan a -> V.Vector a -> IO ()
writeChan (Chan (inCh, _)) v
  = case V.null v of
      True -> return ()
      False -> UChan.writeChan inCh (DeepSeq.force v)

{-# INLINE writeEOFChan #-}
-- | When the producer send this command, it informs consumers that there are no any more
--   interesting values.
--   NOTE: use explicitil
writeEOFChan :: Chan a -> IO ()
writeEOFChan (Chan (inCh, _)) = UChan.writeChan inCh (V.empty)

{-# INLINE readChan #-}
-- | Read the next batched values.
--   An empty batch, implies that no new values should be put/expected on the channel.
readChan :: Chan a -> IO (V.Vector a)
readChan (Chan (_, outCh)) = UChan.readChan outCh

{-# INLINE isEOFChan #-}
-- | True if the element is the last element (null) of the chan.
isEOFChan :: (V.Vector a) -> Bool
isEOFChan = V.null

-- | Execute the action on all elements of the chan.
processChan :: Chan a -> (V.Vector a -> IO ()) -> IO ()
processChan ch act = do
  v <- readChan ch
  case isEOFChan v of
    True -> return ()
    False -> do act v
                processChan ch act

-- --------------------------------
-- Safe resource management

-- | Fully evaluate the returned result, intercepting and rethrowing all exceptions.
--   If during exception management, the resource `release` produce errors,
--   then only the parent exception of the `compute` function is rethrowned.
--   NOTE: returning a fully evaluated value assures that the resources can be
--   released correctly, because it is not any more needed.
safeBracket
  ::  forall m a b c . (MonadMask m, NFData c)
  => m a
  -- ^ acquire the resource
  -> (Maybe SomeException -> a -> m b)
  -- ^ release the resource at the end of normal result computation, or in case of an exception.
  -> (a -> m c)
  -- ^ compute the result
  -> m c
safeBracket acquire release compute = do
  !resource <- acquire
  withException
    (do !r <- DeepSeq.force <$> compute resource
        release Nothing resource
        return r)
    (\exc -> do release (Just exc) resource
                throw exc)

-- ------------------------------
-- Streams

-- | Read the first elements from the stream.
streamReadN :: S.InputStream a -> Int -> IO [a]
streamReadN inS maxElem = do
  f maxElem []
 where

    f 0 r = return r

    f i r = do
      maybeR1 <- S.read inS
      case maybeR1 of
        Nothing -> return r
        Just r1 -> f (i - 1) (r ++ [r1])

-- | Connect two streams transforming the values, and returning a final state/result.
stream_connectWithMapAndFoldM
  :: (s -> a -> IO (s, [b]))
  -- ^ transform a state and input value to a new state, and a new list of result values
  -> s
  -- ^ the initial state
  -> S.InputStream a
  -- ^ the input stream with the values to process
  -> S.OutputStream b
  -- ^ the stream where result values are sent
  -> Bool
  -- ^ True for closing the output stream at the end of the processing
  -> IO s
  -- ^ the final state

stream_connectWithMapAndFoldM f s1 sIn sOut closeSOut
  = processAll s1
 where

   processAll s = do
     maybeA1 <- S.read sIn
     case maybeA1 of
       Nothing -> do when closeSOut (S.write Nothing sOut)
                     return s
       Just a1 -> do (s2, bs) <- f s a1
                     M.mapM_ (\b2 -> S.write (Just b2) sOut) bs
                     processAll s2

-- | Write the content of the stream to the channel, without sending EOF signal.
writeStreamToChan :: (NFData a) => S.InputStream a -> Chan a -> IO ()
writeStreamToChan stream chan = do
  mv <- S.read stream
  case mv of
    Nothing
      -> do return ()
    Just v
      -> do writeChan chan (V.singleton v)
            writeStreamToChan stream chan
