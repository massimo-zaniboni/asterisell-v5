{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

{- $LICENSE 2013, 2014, 2015, 2016
 * Copyright (C) 2013-2016 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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


-- | Define rate plans specific for customers/operators.
--   Usually they are CSV files specifying rates in a compact way.
--   Whenever possibile do not use a specific rate, but use a configurable rate.
--   After definition, define in Main the custom rate.
--
module Asterisell.CustomerSpecificRates (
  parse_twtNngFormat,
  parse_twtNngFormat5ColPrefixFirst,
  parse_gammaFormat9Col,
  parse_gammaItemRentalFormat6Col,
  parse_csvWith3ColsPDR,
  parse_digitelNNGFormat,
  parse_test,
  tt_specificRates
) where

import Asterisell.Cdr
import Asterisell.Error
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.RatePlan
import Asterisell.CSVFileRatePlan
import Asterisell.CustomerSpecificImporters

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text.Lazy as LazyParser
import Data.Attoparsec.Combinator
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as LazyIO
import Data.Maybe
import Data.Map.Strict as Map
import Control.Monad (when)
import Debug.Trace
import Text.Heredoc
import Data.List as List

import qualified Test.HUnit as HUnit

-- | Parse a CSV file in TWT NNG Format.
--   Something like
--
--   > Operator Name,Operator Type,Location,Prefix,Cost by Minute
--   > Italy,Fixed,Italy,TollFree - Italy Fixed -,0.012
--   > Italy,Fixed,Italy,TollFree - Italy OLO ,0.012
--
parse_twtNngFormat
  :: UseHeader
  -> DecimalSeparator
  -> EnvParams
  -> LazyParser.Parser MainRatePlan

parse_twtNngFormat useHeader decimalSeparator env
  = do case useHeader of
         True -> do manyTill (do anyChar
                                 return ())
                             (endOfLine <|> endOfInput)
                    return ()
         False -> return ()

       trie <- parseAndBuildTrie

       let rateParams = RateParams {
                            rate_userId = ""
                          , rate_match = deriveRateMatchFun trie
                          }

       let ratePlan = RatePlan {
                          rate_systemId = 0
                        , rate_params = rateParams
                        , rate_children = Right []
                        , rate_elsePart = []
                        }

       return    mainRatePlan_empty {
                   mainRatePlan_bundleRates = []
                 , mainRatePlan_normalRates = [ratePlan]
                 }

 where

  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "c1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 (prefix, calcParams)
    = trie_addExtensionCode trie1 prefix calcParams

  parseAndBuildTrie
    = List.foldl' buildTrie trie_empty <$> many parseLine

  parseLine
    = do skipCSVField ','
         char ','

         skipCSVField ','
         char ','

         skipCSVField ','
         char ','

         prefix1 <- Text.unpack <$> parseCSVString ','
         let prefix = prefix1 ++ "*"
         char ','

         costByMinute <- parseCost decimalSeparator ','

         (endOfLine <|> endOfInput)

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   }

         return $ (("incoming-toll-free-" ++ prefix), calcParams)

-- | Parse a CSV file in TWT Format, with prefix first. Something like:
--
--   > CODES,DESTINATION,RATE,VALIDITY,COMMENT
--   > 355,Albania,0.066,22/02/2010,decrease
--   > 35538,Albania,0.066,22/02/2010,decrease
--
parse_twtNngFormat5ColPrefixFirst
  :: UseHeader
  -> DecimalSeparator
  -> EnvParams
  -> LazyParser.Parser MainRatePlan

parse_twtNngFormat5ColPrefixFirst useHeader decimalSeparator env
  = do case useHeader of
         True -> do manyTill (do anyChar
                                 return ())
                             (endOfLine <|> endOfInput)
                    return ()
         False -> return ()

       trie <- parseAndBuildTrie

       let rateParams = RateParams {
                            rate_userId = ""
                          , rate_match = deriveRateMatchFun trie
                          }

       let ratePlan = RatePlan {
                         rate_systemId = 0
                      ,  rate_params = rateParams
                      ,  rate_children = Right []
                      ,  rate_elsePart = []
                      }

       return mainRatePlan_empty {
                  mainRatePlan_bundleRates = []
                , mainRatePlan_normalRates = [ratePlan]
              }

 where

  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "d1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 (prefix, calcParams)
    = trie_addExtensionCode trie1 prefix calcParams

  parseAndBuildTrie
    = List.foldl' buildTrie trie_empty <$> many parseLine

  parseLine
    = do prefix1 <- Text.unpack <$> parseCSVString ','
         let prefix = prefix1 ++ "*"
         char ','

         skipCSVField ','
         char ','

         costByMinute <- parseCost decimalSeparator ','
         char ','

         skipCSVField ','
         char ','

         skipCSVField ','

         (endOfLine <|> endOfInput)

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   }

         return (prefix, calcParams)

-- | Parse a CSV file in Gamma Format.
--   The code must be keept in synchro with the CDR importer, because it manage time-band in a custom way.
parse_gammaFormat9Col
  :: EnvParams
  -> LazyParser.Parser MainRatePlan

parse_gammaFormat9Col env
  = do manyTill (do anyChar
                    return ())
                (endOfLine <|> endOfInput)

       trie <- parseAndBuildTrie

       let rateParams = RateParams {
                            rate_userId = ""
                          , rate_match = deriveRateMatchFun trie
                          }

       let ratePlan = RatePlan {
                         rate_systemId = 0
                      ,  rate_params = rateParams
                      ,  rate_children = Right []
                      ,  rate_elsePart = []
                      }

       return mainRatePlan_empty {
                  mainRatePlan_bundleRates = []
                , mainRatePlan_normalRates = [ratePlan]
              }

 where

  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "d1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 codes
    = trie_addExtensionCodes trie1 codes

  parseAndBuildTrie
    = List.foldl' buildTrie trie_empty <$> many parseLine

  parseLine
    = do prefix <- Text.unpack <$> parseCSVString ','
         char ','

         let timeBandAndChargeCode timeBand = concat [prefix, "----time-band-", timeBand, "-*"]
         -- NOTE: the imported rates must be imported using the same schema
         -- NOTE: use first chargeCode, in order to use a more compact telephone prefix table.

         -- add info about the peak time-band
         let prefix1 = timeBandAndChargeCode "1"
         let prefix2 = timeBandAndChargeCode "2"
         let prefix3 = timeBandAndChargeCode "3"

         -- skip descrition
         skipCSVField ','
         char ','

         costByMinute1 <- parseCost '.' ','
         char ','

         costByMinute2 <- parseCost '.' ','
         char ','

         costByMinute3 <- parseCost '.' ','
         char ','

         setupCost1 <- parseCost '.' ','
         char ','

         setupCost2 <- parseCost '.' ','
         char ','

         setupCost3 <- parseCost '.' ','
         char ','

         minimumCharge <- parseCost '.' ','

         (endOfLine <|> endOfInput)

         let calcParams costByMinute setupCost
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   ,   calcParams_costOnCall = Just $ RateCost_cost setupCost
                   ,   calcParams_minCostOfCall = Just $ Just $ minimumCharge
                   }

         return [(prefix1, calcParams costByMinute1 setupCost1)
                ,(prefix2, calcParams costByMinute2 setupCost2)
                ,(prefix3, calcParams costByMinute3 setupCost3)]

-- | Parse a CSV file in Gamma Format for Item Rental.
--   The code must be keept in synchro with the CDR importer, because it manage time-band in a custom way.
parse_gammaItemRentalFormat6Col
  :: EnvParams
  -> LazyParser.Parser MainRatePlan

parse_gammaItemRentalFormat6Col env
  = do manyTill (do anyChar
                    return ())
                (endOfLine <|> endOfInput)

       trie <- parseAndBuildTrie

       let rateParams = RateParams {
                            rate_userId = ""
                          , rate_match = deriveRateMatchFun trie
                          }

       let ratePlan = RatePlan {
                         rate_systemId = 0
                      ,  rate_params = rateParams
                      ,  rate_children = Right []
                      ,  rate_elsePart = []
                      }

       return mainRatePlan_empty {
                  mainRatePlan_bundleRates = []
                , mainRatePlan_normalRates = [ratePlan]
              }

 where

  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "d1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 codes
    = trie_addExtensionCodes trie1 codes

  parseAndBuildTrie
    = List.foldl' buildTrie trie_empty <$> many parseLine

  parseLine
    = do skipCSVField ','
         char ','

         prefix1 <- Text.unpack <$> parseCSVString ','
         char ','
         -- NOTE: despite other rates, in this case the prefix must be matched exactly, so no implicit "*" is added at the end of prefix
         -- add info about the peak time-band

         frequency <- parseCSVString ','
         char ','

         prefix
           <- case frequency of
                   "Rental Per (1 Month)" -> return $ (Text.unpack gamma_ItemRental_rental) ++ prefix1
                   "Rental Per (Month)" -> return $ (Text.unpack gamma_ItemRental_rental) ++ prefix1
                   "Single Charge" -> return $ (Text.unpack gamma_ItemRental_connection) ++ prefix1
                   _ -> fail $ "\"" ++ Text.unpack frequency ++ "\" is not a valid frequency value."

         -- skip rules field
         skipCSVField ','
         char ','

         cost <- parseCost '.' ','
         char ','

         -- skip applied field
         skipCSVField ','

         (endOfLine <|> endOfInput)

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just 0
                   ,   calcParams_costOnCall = Just $ RateCost_cost cost
                   }

         return [(prefix, calcParams)]

--
-- Test Parsing
--

-- | Test some parsing before shipping in production.
--   Activable using "--test-rate-parsing " option of main file.
parse_test :: LazyParser.Parser [String]

parse_test
  = do manyTill (do anyChar
                    return ())
                (endOfLine <|> endOfInput)

       trie <- parseAndBuildTrie

       return $ trie

 where

  parseAndBuildTrie
    = List.foldl' (++) [] <$> many parseLine

  parseLine
    = do skipCSVField ','
         char ','

         prefix1 <- Text.unpack <$> parseCSVString ','
         char ','
         -- NOTE: despite other rates, in this case the prefix must be matched exactly, so no implicit "*" is added at the end of prefix
         -- add info about the peak time-band

         frequency <- parseCSVString ','
         char ','

         prefix
           <- case frequency of
                   "Rental Per (1 Month)" -> return $ (Text.unpack gamma_ItemRental_rental) ++ prefix1
                   "Rental Per (Month)" -> return $ (Text.unpack gamma_ItemRental_rental) ++ prefix1
                   "Single Charge" -> return $ (Text.unpack gamma_ItemRental_connection) ++ prefix1
                   _ -> fail $ "\"" ++ Text.unpack frequency ++ "\" is not a valid frequency value."

         -- skip rules field
         skipCSVField ','
         char ','

         cost <- parseCost '.' ','
         char ','

         -- skip applied field
         skipCSVField ','

         (endOfLine <|> endOfInput)

         return [prefix]

-- | Parse a CSV file in standad format, with prefix first. Something like:
--
--   > CODES,DESTINATION,RATE
--   > 355,Albania,0.066
--   > 35538,Albania,0.066
--
parse_csvWith3ColsPDR
  :: UseHeader
  -> Char
  -> DecimalSeparator
  -> EnvParams
  -> LazyParser.Parser MainRatePlan

parse_csvWith3ColsPDR useHeader fieldSeparator decimalSeparator env
  = do case useHeader of
         True -> do manyTill (do anyChar
                                 return ())
                             (endOfLine <|> endOfInput)
                    return ()
         False -> return ()

       trie <- parseAndBuildTrie

       let rateParams = RateParams {
                            rate_userId = ""
                          , rate_match = deriveRateMatchFun trie
                          }

       let ratePlan = RatePlan {
                         rate_systemId = 0
                      ,  rate_params = rateParams
                      ,  rate_children = Right []
                      ,  rate_elsePart = []
                      }

       return mainRatePlan_empty {
                  mainRatePlan_bundleRates = []
                , mainRatePlan_normalRates = [ratePlan]
              }

 where

  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "d1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 (prefix, calcParams)
    = trie_addExtensionCode trie1 prefix calcParams

  parseAndBuildTrie
    = List.foldl' buildTrie trie_empty <$> many parseLine

  parseLine
    = do prefix1 <- Text.unpack <$> parseCSVString fieldSeparator
         let prefix = prefix1 ++ "*"
         char fieldSeparator

         skipCSVField fieldSeparator
         char fieldSeparator

         costByMinute <- parseCost decimalSeparator fieldSeparator

         (endOfLine <|> endOfInput)

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   }

         return (prefix, calcParams)

-- | Parse a CSV file in Digitel NNG Format.
--
--   > "Numerazione","Operatore-Servizio","Peak","Off-Peak","Scatto","Durata Minima"
--   > 112,"Carabinieri",0,0,0,0
--   > 113,"Soccorso Pubblica Emergenza",0,0,0,0
--
--   The code must be keept in synchro with the CDR importer, because it manage time-band in a custom way.
parse_digitelNNGFormat
  :: EnvParams
  -> LazyParser.Parser MainRatePlan

parse_digitelNNGFormat env
  = do manyTill (do anyChar
                    return ())
                (endOfLine <|> endOfInput)

       trie <- parseAndBuildTrie

       let rateParams = RateParams {
                            rate_userId = ""
                          , rate_match = deriveRateMatchFun trie
                          }

       let ratePlan = RatePlan {
                         rate_systemId = 0
                      ,  rate_params = rateParams
                      ,  rate_children = Right []
                      ,  rate_elsePart = []
                      }

       return mainRatePlan_empty {
                  mainRatePlan_bundleRates = []
                , mainRatePlan_normalRates = [ratePlan]
              }

 where
  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "d1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 codes
    = trie_addExtensionCodes trie1 codes

  freeCall = calcParams_empty

  -- Start with a default free enumaration
  initialTrie
    = trie_addExtensionCodes 
        trie_empty
        [(makePrefix True "800", freeCall)
        ,(makePrefix False "800", freeCall)
        ]

  parseAndBuildTrie
    = List.foldl' buildTrie initialTrie <$> many parseLine

  makePrefix isPeak prefix
    = Text.unpack $
        Text.append
          (Text.append
             (const_digitelNNGTimeBandPrefix isPeak)
             (Text.append "39" prefix))
        "*"
 
  --   > "Numerazione","Operatore-Servizio","Peak","Off-Peak","Scatto","Durata Minima"
  --   > 112,"Carabinieri",0,0,0,0
  --   > 113,"Soccorso Pubblica Emergenza",0,0,0,0
  parseLine
    = do prefix <- parseCSVString ','
         char ','

         let prefixT = makePrefix True prefix
         let prefixF = makePrefix False prefix
         
         -- skip descrition
         skipCSVField ','
         char ','

         costByMinute1 <- parseCost ',' ','
         char ','

         costByMinute2 <- parseCost ',' ','
         char ','

         initialCost <- parseCost ',' ','
         char ','

         minimuLen <- parseInt

         (endOfLine <|> endOfInput)

         let calcParams costByMinute 
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   ,   calcParams_atLeastXSeconds = Just $ minimuLen
                   ,   calcParams_costOnCall = Just $ RateCost_cost initialCost
                   }

         return [(prefixT, calcParams costByMinute1 )
                ,(prefixF, calcParams costByMinute2)
                ] 

--
-- Utils Functions
--

parseCost decimalSeparator fieldSeparator
    = do cost1 <- parseCSVString fieldSeparator
         let cost2 = if decimalSeparator == '.'
                     then cost1
                     else Text.map (\c -> if c == decimalSeparator then '.' else c) cost1

         case fromTextToRational cost2 of
           Nothing -> fail $ "\"" ++ Text.unpack cost1 ++ "\" is not a valid monetary value using \"" ++ show decimalSeparator ++ "\" as decimal separator."
           Just v -> return v

--
-- Unit Tests
--

tt_specificRates = []
