{-# Language BangPatterns, OverloadedStrings, QuasiQuotes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Manage `ar_params`
--
module Asterisell.Params (
  defaultParams_unbilledCallsFrom
) where

import Asterisell.Trie
import Asterisell.Utils
import Asterisell.DB
import Asterisell.Error

import qualified Data.Text as Text
import qualified Data.ByteString as BS

import qualified System.IO as SIO
import Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import qualified Data.IntMap.Strict as IMap
import Data.List as L

import GHC.Generics
import Control.DeepSeq

import Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import Text.Heredoc
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)


-- | Return the `ar_params.official_calldate`
defaultParams_unbilledCallsFrom :: DB.MySQLConn -> String -> IO CallDate
defaultParams_unbilledCallsFrom conn errLabel = do
  (_, inS) <- DB.query_ conn "SELECT official_calldate FROM ar_params WHERE is_default"
  s <- S.toList inS
  case s of
    [[v]] -> return $ fromDBLocalTime v
    []  -> throwIO $ AsterisellException (errLabel' ++ "There are no default settings/params to use. Configure them in PARAMS.")
    _   -> throwIO $ AsterisellException (errLabel' ++ "There should be only one default settings/params to use. Delete records in excess from ar_params table.")
 where

   errLabel' = "(ERR " ++ errLabel ++ ") "
