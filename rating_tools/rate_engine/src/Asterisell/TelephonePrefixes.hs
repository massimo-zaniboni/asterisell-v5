{-# Language BangPatterns, OverloadedStrings, QuasiQuotes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Import Telephone Prefixes.
--
--   A telephone prefix is only used for describing a call in the call report.
--
module Asterisell.TelephonePrefixes (
  TelephonePrefixes,
  telephonePrefixes_load,
  telephonePrefixes_loadRatingCodes,
  telephonePrefixes_update,
  TelephonePrefixRecord(..),
  RatingCodes,
  IdToTelephonePrefix,
  TelephonePrefix(..)
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
import qualified Data.Set as Set
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

type RatingCode = Text.Text

type RatingCodes = Set.Set RatingCode

-- | Info about a telephone prefix.
--   DEV-NOTE: do not pack, because the many repeated Text entries are shared
data TelephonePrefix
       = TelephonePrefix {
           tp_id :: !Int
         , tp_ratingCode :: !RatingCode
         , tp_operatorType :: !Text.Text
         , tp_geographicLocation :: !Text.Text
         }
 deriving (Show, Eq, Ord)

-- | Associate a telephone prefix to its id and rating code
type TelephonePrefixes = Trie TelephonePrefix

type IdToTelephonePrefix = IMap.IntMap TelephonePrefix

telephonePrefixes_load
  :: DB.MySQLConn
  -> Bool
  -> IO (TelephonePrefixes, IdToTelephonePrefix, RatingCodes)

telephonePrefixes_load conn isDebugMode = do
  let q1 = [str| SELECT
               |   id
               | , prefix
               | , match_only_numbers_with_n_digits
               | , rating_code
               | , geographic_location
               | , operator_type
               | FROM ar_telephone_prefix
               | ORDER BY prefix
               |]

  sdict :: IOStringDictionary Text.Text <- d_empty
  (_, inS) <- DB.query_ conn q1
  S.foldM (importPrefix sdict) (trie_empty, IMap.empty, Set.empty) inS

 where

  importPrefix
    sdict
    (map1, idToTelephonePrefix1, set1)
    [ id'
    , prefix'
    , match_only_numbers_with_n_digits'
    , rating_code'
    , geographic_location'
    , operator_type'
    ] = do

        let id = fromDBInt id'
        rating_code <- d_get sdict $ fromDBText rating_code'
        prefix <- nn "telephone_prefix" id fromDBByteString prefix'
        let match_n_digits = fromMaybeDBValue fromDBInt match_only_numbers_with_n_digits'
        let suffix = case match_n_digits of
                       Nothing
                         -> Nothing
                       Just 0
                         -> Nothing
                       Just n1
                         -> Just $ n1 - (BS.length prefix)

        geographic_location <- d_get sdict $ nnn "telephone_prefix" id fromDBText geographic_location'
        operator_type <- d_get sdict $ nnn "telephone_prefix" id fromDBText operator_type'

        let tp = TelephonePrefix {
                   tp_id = id
                 , tp_ratingCode = rating_code
                 , tp_geographicLocation = geographic_location
                 , tp_operatorType = operator_type
                 }

        case trie_insertUnique map1 (prefix, suffix) tp of
          Nothing
            -> throwIO $ AsterisellException $ "The telephone prefix \"" ++ (fromByteStringToString prefix) ++ "\", is in conflict with another prefix. Correct the value in the telephone_prefix table. "
          Just map2
            -> return (map2, IMap.insert id tp idToTelephonePrefix1, Set.insert rating_code set1)

  importPrefix _ _ _ = throwIO $ AsterisellException "err 1601 in code: unexpected DB format for ar_telephone_prefix"

-- | A relatively fast query for retrieving only (the usually few) rating codes.
telephonePrefixes_loadRatingCodes
  :: DB.MySQLConn
  -> Bool
  -> IO RatingCodes

telephonePrefixes_loadRatingCodes conn isDebugMode = do
  let q1 = "SELECT DISTINCT rating_code FROM ar_telephone_prefix"
  (_, inS) <- DB.query_ conn q1
  S.foldM importPrefix Set.empty inS

 where

  importPrefix set1 [rating_code'] = do
        let rating_code = fromDBText rating_code'
        return $ Set.insert rating_code set1

  importPrefix _ _ = throwIO $ AsterisellException "err 1602 in code: unexpected DB format for ar_telephone_prefix"

data TelephonePrefixRecord
       = TelephonePrefixRecord {
              tpr_prefix :: Text.Text
            , tpr_matchOnlyExactDigits :: Maybe Int
            , tpr_name :: Text.Text
            , tpr_geographic_location :: Text.Text
            , tpr_operator_type :: Text.Text
            , tpr_display_priority :: Int
            , tpr_rating_code :: BS.ByteString
            }
         deriving (Eq, Generic, NFData)


-- | Add/update telephone prefixes table, with new entries.
--   NOTE: it is not necessary using a transaction because usually they are very few values to add.
--   NOTE: changes must be written only if there is something of new, otherwise a global rerating event is issued,
--   because ar_telephone_prefix is a table affecting the generic rating.
telephonePrefixes_update :: DB.MySQLConn -> IO (S.OutputStream TelephonePrefixRecord)
telephonePrefixes_update conn = do
  let q1 = [str| REPLACE INTO ar_telephone_prefix
               |   (prefix,
               |    match_only_numbers_with_n_digits,
               |    name,
               |    geographic_location,
               |    operator_type,
               |    display_priority_level,
               |    rating_code)
               | VALUES(?,?,?,?,?,?,?)
               |]
  updateStmtId <- DB.prepareStmt conn q1

  let q2 = [str| SELECT prefix
               | FROM ar_telephone_prefix
               | WHERE prefix = ?
               | AND match_only_numbers_with_n_digits = ?
               | AND name = ?
               | AND geographic_location = ?
               | AND operator_type = ?
               | AND display_priority_level = ?
               | AND rating_code = ?
               |]
  checkStmtId <- DB.prepareStmt conn q2

  S.makeOutputStream (insertRecord checkStmtId updateStmtId)

 where

   insertRecord _ _  Nothing = return ()
   insertRecord checkStmtId updateStmtId (Just tpr) = do

     let values
           = [ toDBText $ tpr_prefix tpr
             , toMaybeInt64 $ tpr_matchOnlyExactDigits tpr
             , toDBText $ tpr_name tpr
             , toDBText $ tpr_geographic_location tpr
             , toDBText $ tpr_operator_type tpr
             , toDBInt64 $ tpr_display_priority tpr
             , toDBByteString $ tpr_rating_code tpr
             ]

     (_, checkS) <- DB.queryStmt conn checkStmtId values
     isUpdate <- S.fold (\_ _ -> False) True checkS

     case (isUpdate) of
       True -> do _ <- DB.executeStmt conn updateStmtId values
                  return ()
       False -> return ()
