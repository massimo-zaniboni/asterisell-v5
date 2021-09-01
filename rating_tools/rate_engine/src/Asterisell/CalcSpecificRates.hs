{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2020 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

module Asterisell.CalcSpecificRates (
    calcSpecificRates
  , calcSpecificRatesUsingFiles
) where

import Asterisell.DB
import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.ParseRatePlan
import Asterisell.ApplyRatePlan
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.CustomerSpecificImporters
import Asterisell.CustomerSpecificRates

import Data.List as List
import Data.Maybe
import Data.Ord as Ord
import Control.Monad as M
import Control.Monad.Except
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Database.MySQL.Base as DB
import Text.Heredoc
import GHC.Generics
import Control.DeepSeq
import Control.Exception.Safe
import qualified Data.Vector as V
import qualified Data.Csv as CSV
import qualified Data.Trie.BigEndianPatricia.Base as Trie
import qualified Data.Trie.BigEndianPatricia.Internal as TrieInternal
import qualified Data.Trie.BigEndianPatricia.Convenience as Trie
import qualified System.IO.Streams as S
import qualified System.Directory as S

-- ---------------------------------
-- Main API

calcSpecificRates
  :: CmdParams
  -> Int
  -- ^ currency precision digits
  -> Int
  -- ^ the ar_specific_rate_calc.id to process
  -> IO ()
  --  ^ errors are signaled with an exception
calcSpecificRates params currencyPrecision calcId =
  withResource
    (db_openConnection (cparams_toDBConf params) False)
    (\conn -> do
       -- MAYBE delete fields and signal it is under processing and commit and open new transaction,
       -- or manage this on the PHP side in case there are errors

       db_openTransaction conn

       let q0 = [str| SELECT brf.internal_name
                    |      , br.source_data_file
                    |      , br.id
                    |      , br.internal_name
                    |      , c.mediumtext_specific_rate_in_match_all
                    |      , c.mediumtext_specific_rate_in_match_exact
                    |      , c.price_category_name
                    |      , c.specific_rate_name
                    | FROM ar_specific_rate_calc AS c
                    | LEFT JOIN ar_rate AS br
                    | ON br.id = c.ar_rate_id
                    | LEFT JOIN ar_rate_format AS brf
                    | ON brf.id = br.ar_rate_format_id
                    | WHERE c.id = ?
                    | LIMIT 1
                    |]

       -- DEV-NOTE: ar_rate are binary blob (i.e. ByteStrings) while ar_specific_rate_calc are text blobs (i.e. Text)

       (_, inS0) <- DB.query conn (DB.Query q0) [toDBInt64 calcId]
       maybeR <- S.read inS0

       [ baseRateFormatNameAsMySQL
         , baseRateContentAsMySQL
         , baseRateIdAsMySQL
         , baseRateNameAsMySQL
         , matchAllPrefixesRateAsMySQL
         , matchExactPrefixesRateAsMySQL
         , priceCategoryNameAsMySQL
         , specificRateNameAsMySQL] <-
         case maybeR of
              Just r -> return r
              Nothing -> throwIO $ AsterisellException $ "missing ar_specific_rate.id = " ++ show calcId

       S.skipToEof inS0

       (baseRateFormatName, baseRateId) <-
         case ( fromMaybeDBValue fromDBText baseRateFormatNameAsMySQL
              , fromMaybeDBValue fromDBInt baseRateIdAsMySQL) of
           (Just c1, Just c2) -> return (c1, c2)
           _ -> throwIO $ AsterisellException ("The base rate has no specified format.")

       baseRateNormalizer <-
         case configuredRatePlanNormalizer_get configuredRateParsers baseRateFormatName of
            Nothing
              -> throwIO $ AsterisellException
                           ("The rate format \"" ++ (Text.unpack baseRateFormatName) ++ "\" used for the definition of rate with id " ++ show baseRateId ++ "\" is unknown, or it can not be normalized to a format with a cost-by-minute and a cost-on-call, and the rate specification can not be parsed. Use a different rate format. ")

            Just r -> return r

       baseRateContent <-
         case fromMaybeDBValue fromDBByteString baseRateContentAsMySQL of
           Nothing
             -> throwIO $ AsterisellException ("The base rate with id  \"" ++ show baseRateId ++ "\" has no content. Add the content of the rate.")
           Just c
             -> return c

       baseRate <-
         case baseRateNormalizer baseRateContent of
           Left err -> throwIO $ AsterisellException $ "Error during parsing of rate id " ++ show baseRateId ++ ": " ++ err
           Right r -> return r

       specificRateNormalizer <-
         case configuredRatePlanNormalizer_get configuredRateParsers specificRateFormat of
            Nothing -> throwIO $ AsterisellException
                           ("The rate format \"" ++ (Text.unpack baseRateFormatName) ++ "\" is missing. Contact the assistance because this is an error in the code.")
            Just r -> return r

       let parseSpecificRate rateName rateContentAsMySQL =
             case specificRateNormalizer (fromTextToByteString $ fromDBText rateContentAsMySQL) of
               Left err -> throwIO $ AsterisellException $ "Error during parsing of specific rate " ++ rateName ++ ": " ++ err ++ ". Fix the rate content."
               Right r -> return r

       matchAllPrefixesRate <- parseSpecificRate "matching all prefixes" matchAllPrefixesRateAsMySQL

       matchExactPrefixesRate <- parseSpecificRate "matching exact prefixes" matchExactPrefixesRateAsMySQL

       let specificRate = normalizedRates_fromSpecificCosts baseRate matchAllPrefixesRate matchExactPrefixesRate

       let rateInfo = normalizedRates_info baseRate specificRate

       let baseRateDiffs = normalizedRates_toDiffRate baseRate specificRate

       let baseRateName = fromDBText baseRateNameAsMySQL

       let priceCategoryName = fromDBText priceCategoryNameAsMySQL

       let specificRateName = fromDBText specificRateNameAsMySQL

       let ratePlanOut :: Text.Text =
            Text.concat [
              "      rate {\n",
              "        id: ", specificRateName , " \n",
              "        match-price-category: ", priceCategoryName, "\n",
              "        rate {\n",
              "          id: custom\n",
              "          # Specific rate, overriding the standard/fallback rate ", baseRateName, "\n",
              "          # NOTE: every short prefix of this rate (e.g. \"12\") overrides also longer prefixes on fallback rate (e.g. \"1234\")\n",
              "          use: ", specificRateName, "\n",
              "          set-cost-on-call: external\n",
              "          set-cost-for-minute: external\n",
              "        } else {\n",
              "          rate {\n",
              "            id: standard\n",
              "            # Standard/fallback rate\n",
              "            use: ", baseRateName, "\n",
              "            set-cost-on-call: external\n",
              "            set-cost-for-minute: external\n",
              "          }\n",
              "        }\n",
              "      }\n"]

       let q1 = [str| UPDATE ar_specific_rate_calc
                    | SET is_recalc = 1
                    | ,   mediumtext_specific_rate_out = ?
                    | ,   rate_plan_out = ?
                    | ,   mediumtext_base_rate_diff = ?
                    | ,   calc_info = ?
                    | ,   calc_error = ?
                    | WHERE id = ?
                    |]

       _ <- DB.execute
              conn
              (DB.Query q1)
              [    toDBText $ fromByteStringToText $ LBS.toStrict $ normalizedRates_toSpecificRateCSV specificRate
                ,  toDBText ratePlanOut
                ,  toDBText $ fromByteStringToText $ LBS.toStrict $ normalizedDiffRates_toCSV baseRateDiffs
                ,  toDBText $ normalizedRates_info baseRate specificRate
                ,  toDBText ""
                ,  toDBInt64 calcId
              ]

       return ())
  db_releaseResource
  (\exc -> SomeException $ AsterisellException $ "Error during processing of ar_specific_rate_calc with id " ++ show calcId ++ ". " ++ displayException exc)


calcSpecificRatesUsingFiles
  :: String
  -> String
  -> String
  -> String
  -> IO ()
  --  ^ errors are signaled with an exception

calcSpecificRatesUsingFiles
  baseRateFileName
  matchAllRateFileName
  matchExactRateFileName
  resultFileName = do

       baseRate <- readRate baseRateFileName
       matchAllRate <- readRate matchAllRateFileName
       matchExactRate <- readRate matchExactRateFileName

       let specificRate = normalizedRates_fromSpecificCosts baseRate matchAllRate matchExactRate
       let rateInfo = normalizedRates_info baseRate specificRate

       LBS.writeFile resultFileName $ normalizedRates_toSpecificRateCSV specificRate

       putStrLn $ "Base rate " ++ baseRateFileName ++ ", matcha all " ++ matchAllRateFileName ++ ", match exact " ++ matchExactRateFileName ++ " --> " ++ resultFileName
       putStrLn $ Text.unpack rateInfo

 where

   readRate :: String -> IO NormalizedRateTrie
   readRate rateFileName = do
     rateNormalizer <-
       case configuredRatePlanNormalizer_get configuredRateParsers specificRateFormat of
         Nothing -> throwIO $ AsterisellException ("Error 7075377 in the code.")
         Just r -> return r

     isThereFile <- S.doesFileExist rateFileName
     case isThereFile of
       True -> return ()
       False -> throwIO $ AsterisellException ("Missing file " ++ rateFileName)

     rateContent <- BS.readFile rateFileName
     case rateNormalizer rateContent of
       Left err -> throwIO $ AsterisellException $ "Error during parsing of rate " ++ rateFileName ++ ": " ++ err
       Right r -> return r
