{-# Language BangPatterns, OverloadedStrings, QuasiQuotes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

{- $LICENSE 2013, 2014, 2015, 2016, 2017
 * Copyright (C) 2013-2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
-}

-- | Import Telephone Prefixes.
--
--   A telephone prefix is only used for describing a call in the call report.
--
module Asterisell.TelephonePrefixes (
  TelephonePrefixes,
  telephonePrefixes_load,
  telephonePrefixes_update,
  TelephonePrefixRecord(..)
) where

import Asterisell.Trie
import Asterisell.Utils
import Asterisell.DB
import Asterisell.Error

import qualified Data.Text as Text

import qualified System.IO as SIO
import Data.Map.Strict as Map
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

-- | Associate a telephone prefix to its id.
type TelephonePrefixes = Trie Int

telephonePrefixes_load
  :: DB.MySQLConn
  -> Bool
  -> IO TelephonePrefixes

telephonePrefixes_load conn isDebugMode = do
  let q1 = [str| SELECT
               |   id
               | , prefix
               | , match_only_numbers_with_n_digits
               | FROM ar_telephone_prefix
               | ORDER BY prefix
               |]

  (_, inS) <- DB.query_ conn q1
  S.foldM importPrefix trie_empty inS

 where

  importPrefix
    map1
    [ id'
    , prefix'
    , match_only_numbers_with_n_digits'
    ] = do

        let id = fromDBInt id'
        prefix'' <- nn "telephone_prefix" id fromDBText prefix'
        let match_n_digits = fromMaybeDBValue fromDBInt match_only_numbers_with_n_digits'
        let prefix = case match_n_digits of
                       Nothing
                         -> Text.append prefix'' "X*"
                       Just 0
                         -> Text.append prefix'' "X*"
                       Just n1
                         -> let n2 = n1 - (Text.length prefix'')
                            in  if n2 > 0
                                then (Text.append prefix'' (Text.replicate n2 "X"))
                                else prefix''

        case trie_addUniqueExtensionCode map1 (Text.unpack prefix) id of
          Nothing
            -> throw $ AsterisellException $ "The telephone prefix \"" ++ (Text.unpack prefix'') ++ "\", is in conflict with another prefix. Correct the value in the telephone_prefix table. "
          Just map2
            -> let code = Text.unpack prefix''
                   codeM = extensionCode_toCharsMatch code
               in case charsMatch_valid codeM of
                    Just err
                      -> throw $ AsterisellException $ "Telephone prefix \"" ++ code ++ "\" has an invalid format: " ++ err
                    Nothing
                      -> return map2

  importPrefix _ _ = throw $ AsterisellException "err 1601 in code: unexpected DB format for ar_telephone_prefix"

data TelephonePrefixRecord
       = TelephonePrefixRecord {
              tpr_prefix :: Text.Text
            , tpr_matchOnlyExactDigits :: Maybe Int
            , tpr_name :: Text.Text
            , tpr_geographic_location :: Text.Text
            , tpr_operator_type :: Text.Text
            , tpr_display_priority :: Int
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
               |    display_priority_level)
               | VALUES(?,?,?,?,?,?)
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
             ]

     (_, checkS) <- DB.queryStmt conn checkStmtId values
     isUpdate <- S.fold (\_ _ -> False) True checkS

     case (isUpdate) of
       True -> do _ <- DB.executeStmt conn updateStmtId values
                  return ()
       False -> return ()
