{-# Language OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Import info about Rate Categories
--
module Asterisell.RateCategories(
    RateCategoryId
  , RateCategoryCode
  , RateCategories
  , rateCategories_load
  , rateCategories_code
  , rateCategories_empty
  , rateCategories_create
) where

import Asterisell.Error
import Asterisell.DB
import Asterisell.Trie
import Asterisell.Utils

import System.IO
import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), pure)

import qualified Data.Text as Text
import Data.Map.Strict as Map
import Data.Maybe
import Data.List as List
import Data.Time.LocalTime
import Data.Vector as V (length)

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

type RateCategoryId = Int

type RateCategoryCode = Text.Text

type RateCategories 
  = (Map.Map RateCategoryCode RateCategoryId
    ,Map.Map RateCategoryId RateCategoryCode) 

rateCategories_empty :: RateCategories
rateCategories_empty = (Map.empty, Map.empty)

rateCategories_create :: Map.Map RateCategoryCode RateCategoryId -> RateCategories
rateCategories_create map1
  = let map2 = Map.fromList $ List.map (\(i,j) -> (j,i)) $ Map.toList map1
    in (map1, map2)

rateCategories_code :: RateCategories -> RateCategoryId -> RateCategoryCode
rateCategories_code (_, map2) id
  = fromJust $ Map.lookup id map2

rateCategories_load
  :: DB.MySQLConn
  -> Bool
  -> IO RateCategories

rateCategories_load conn isDebugMode = do
  let q1 = [str| SELECT
               |   id
               | , internal_name
               | FROM ar_rate_category
               | WHERE internal_name IS NOT NULL
               |]

  (_, inS) <- DB.query_ conn q1
  categories <- S.foldM importCategory Map.empty inS

  return $ rateCategories_create categories

 where

   importCategory
     map1
     [ id'
     , internal_name'
     ] = do 
       let id = fromDBInt id'
       internal_name <- nn "rate_category" id fromDBText internal_name'

       return $ Map.insert internal_name id map1

   importCategory _ _ = throwIO $ AsterisellException "err 1600 in code: unexpected DB format for ar_rate_category"


