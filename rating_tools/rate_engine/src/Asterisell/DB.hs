{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

{- $LICENZE 2013, 2014, 2015, 2016, 2017
 * Copyright (C) 2013-2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

-- | Manage DB connections.
--
module Asterisell.DB (
  DBConf(..),
  openDBConnection,
  toDBBool,
  toDBInt64,
  toMaybeInt64,
  toDBMaybeString,
  toDBByteString,
  toDBMaybeText,
  toDBText,
  toDBLocalTime,
  toDBMaybeLocalTime,
  fromDBInt,
  fromDBBool,
  fromDBLocalTime,
  fromMaybeDBValue,
  fromDBText,
  fromDBByteString,
  nn
 ) where

import Asterisell.Error
import Asterisell.Utils

import qualified Data.ByteString as BS
import Data.Time.LocalTime
import qualified Data.Text as Text
import Control.Exception.Safe (throw)
import Database.MySQL.Base as DB

data DBConf
   = DBConf {
       dbConf_dbName :: BS.ByteString
     , dbConf_user :: BS.ByteString
     , dbConf_password :: BS.ByteString
     } deriving (Eq, Show)

openDBConnection :: DBConf -> Bool -> IO DB.MySQLConn
openDBConnection dbConf writeTransaction
     = do

          db <- DB.connect dbParams
          -- MAYBE not necessary DB.query db "SET NAMES 'utf8' COLLATE 'utf8_bin'"
          DB.execute_ db "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED"
            -- set this level because in TokuDB it is the only level for SELECT, not involving LOCKS and SNAPSHOOTS,
            -- it is faster than others,
            -- and it prevents many conflicts and long transactions lock timeouts.
            --
            -- NOTE: this is still safe in Asterisell because:
            -- * there is only one write transaction at a time processing CDRs
            -- * in case there are transactions changing configuration data on the web-side, then the "recalc-flag" is activated by triggers,
            --   and CDRs are recalculated in any case, so data at the end of recalculation is never in an inconsistent state
            -- * new errors are written on a temporary table not visible to users, and the content is copied at the end of CDR processing

          DB.execute_ db "SET autocommit = 0"

          case writeTransaction of
            True ->  do DB.execute_ db "START TRANSACTION"
                        return ()
            False -> do return ()
          return db
 where

   dbParams = DB.defaultConnectInfoMB4 {
                     ciUser = dbConf_user dbConf
                   , ciPassword = dbConf_password dbConf
                   , ciDatabase = dbConf_dbName dbConf
                   }

-- --------------------------------------------
-- Convert DB Values to Haskell  types.
-- This is useful because sometimes is not clear witch particular DBValue
-- a query will return.

fromDBInt :: DB.MySQLValue -> Int
fromDBInt (MySQLInt32 v) = fromIntegral v
fromDBInt (MySQLInt64 v) = fromIntegral v
fromDBInt (MySQLInt32U v) = fromIntegral v
fromDBInt (MySQLInt64U v) = fromIntegral v
fromDBInt (MySQLInt8 v) = fromIntegral v
fromDBInt (MySQLInt8U v) = fromIntegral v
fromDBInt (MySQLInt16 v) = fromIntegral v
fromDBInt (MySQLInt16U v) = fromIntegral v
fromDBInt v = error $ "fromDBInt: unsupported MySQL value" ++ show v
{-# INLINE fromDBInt #-}

fromDBText :: DB.MySQLValue -> Text.Text
fromDBText (MySQLText v) = v
fromDBText (MySQLBytes v) = fromByteStringToText v
fromDBText v = error $ "fromDBText: unsupported MySQL value " ++ show v
{-# INLINE fromDBText #-}

fromDBByteString :: DB.MySQLValue -> BS.ByteString
fromDBByteString (MySQLText v) = fromTextToByteString v
fromDBByteString (MySQLBytes v) = v
fromDBByteString v = error $ "fromDBByteString: unsupported MySQL value " ++ show v
{-# INLINE fromDBByteString #-}

fromDBLocalTime :: DB.MySQLValue -> LocalTime
fromDBLocalTime (MySQLDateTime v) = v
fromDBLocalTime (MySQLTimeStamp v) = v
fromDBLocalTime v = error $ "fromDBLocalTime: unsupported MySQL value " ++ show v
{-# INLINE  fromDBLocalTime #-}

fromDBBool :: DB.MySQLValue -> Bool
fromDBBool v = case fromDBInt v of
                 1 -> True
                 0 -> False
{-# INLINE fromDBBool #-}

fromMaybeDBValue :: (DB.MySQLValue -> a) -> DB.MySQLValue -> Maybe a
fromMaybeDBValue  _ (MySQLNull) = Nothing
fromMaybeDBValue f v = Just $ f v
{-# INLINE fromMaybeDBValue #-}

-- | Used during importing of data, for signaling values that are not NULL.
nn :: String -> Int -> (DB.MySQLValue -> a) -> DB.MySQLValue -> IO a
nn table id f x
   = case fromMaybeDBValue f x of
       Just a -> return a
       Nothing -> throw $ AsterisellException $ "Unexpected field with NULL value in " ++ table ++ " with id " ++ show id
{-# INLINE nn #-}

-- ---------------------------------------------------
-- Low Level Utils for Converting values to DB values

toDBBool :: Bool -> MySQLValue
toDBBool True = MySQLInt8 1
toDBBool False = MySQLInt8 0
{-# INLINE toDBBool #-}

toDBInt64 :: Int -> MySQLValue
toDBInt64 i =  MySQLInt64 $ fromInteger $ toInteger i
{-# INLINE toDBInt64 #-}


toMaybeInt64 :: Maybe Int -> DB.MySQLValue
toMaybeInt64 Nothing = MySQLNull
toMaybeInt64 (Just s) = toDBInt64 s
{-# INLINE toMaybeInt64 #-}

toDBMaybeString :: Maybe BS.ByteString -> DB.MySQLValue
toDBMaybeString Nothing = MySQLNull
toDBMaybeString (Just s) = MySQLBytes s
{-# INLINE toDBMaybeString #-}


toDBByteString :: BS.ByteString -> DB.MySQLValue
toDBByteString s = MySQLBytes s
{-# INLINE toDBByteString #-}

toDBMaybeText :: Maybe Text.Text -> DB.MySQLValue
toDBMaybeText Nothing = MySQLNull
toDBMaybeText (Just s) = toDBText s
{-# INLINE toDBMaybeText #-}

toDBText :: Text.Text -> DB.MySQLValue
toDBText s = MySQLBytes $ fromTextToByteString s
{-# INLINE toDBText #-}

toDBLocalTime :: LocalTime -> DB.MySQLValue
toDBLocalTime l = MySQLDateTime l
-- NOTE: use MySQLDateTime instead of MySQLTimeStamp, so MySQL store it without messing with UTC conversion
{-# INLINE toDBLocalTime #-}

toDBMaybeLocalTime :: Maybe LocalTime -> DB.MySQLValue
toDBMaybeLocalTime Nothing = MySQLNull
toDBMaybeLocalTime (Just t) = toDBLocalTime t
{-# INLINE toDBMaybeLocalTime #-}
