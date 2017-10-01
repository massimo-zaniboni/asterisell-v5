{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

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


-- | Define rate plans specific for customers/operators.
--   Usually they are CSV files specifying rates in a compact way.
--   Whenever possibile do not use a specific rate, but use a configurable rate.
--   After definition, define in Main the custom rate.
--
module Asterisell.CustomerSpecificRates (
  configuredRateParsers,
  parse_twtNngFormat,
  parse_twtNngFormat5ColPrefixFirst,
  parse_gammaFormat9Col,
  parse_gammaItemRentalFormat6Col,
  parse_csvWith3ColsPDR,
  parse_digitelNNGFormat,
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
import Asterisell.CustomerSpecificImporters
import Asterisell.MainRatePlan

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text as Parser
import Data.Attoparsec.Combinator
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Data.Map.Strict as Map
import Control.Monad (when)
import Debug.Trace
import Text.Heredoc
import Data.List as List

import qualified Test.HUnit as HUnit

-- -------------------------------------------
-- CSV Parsing 

-- | Parse a line of a CSV file, and return the prefixes recognized
--   (with specified explicitely the `*` and `X` generic match parts)
--   and the associated calc params.
--   The parser is in UTF8 format.
--   The new line must be not consumed/recognized from this parser.
--   NOTE: if possible do not use slow AttoParsec combinators.
type CSVLineParser = RatingParams -> Parser.Parser [(Text.Text, CalcParams)]

createRatePlanParserFromCSVLineParser :: CSVLineParser -> Bool -> [(Text.Text, CalcParams)] -> RatePlanParser
createRatePlanParserFromCSVLineParser lineParser skipHeader initialRates envParams rateContent
  = case Text.decodeUtf8' rateContent of
      Left err
        -> Left $ "The rate content is not in UTF8 format: " ++ show err ++ ". Convert the rate in UTF8 format, and update it in Asterisell database."
      Right rateContentT
        -> parseOnly mainParser rateContentT
 where

   isNotNL c = c /= '\n' && c /= '\r'

   skipLine = return () <$> Parser.takeWhile isNotNL *> endOfLine

   mainParser 
     = do l1 <- case skipHeader of
                  True -> do skipLine
                             return 2
                  False -> return 1

          trie <- parseAllLines l1 (trie_update trie_empty initialRates)

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

          return mainRatePlan_empty {
                   mainRatePlan_bundleRates = []
                 , mainRatePlan_normalRates = [ratePlan]
                 }

   parseAllLines :: Int -> Trie CalcParams -> Parser (Trie CalcParams)
   parseAllLines lineCount trie1
     = (do endOfInput
           return trie1) <|>
       (do prefixesAndParams <- lineParser envParams <?> "at line " ++ show lineCount
           let trie2 = trie_update trie1 prefixesAndParams
           endOfInput <|> endOfLine
           parseAllLines (lineCount + 1) trie2)

   trie_update trie1 prefixesAndParams
     = List.foldl' (\trie (prefix, params) -> trie_addExtensionCode trie (Text.unpack prefix) params) trie1 prefixesAndParams

   deriveRateMatchFun trie env cdr
     = case trie_getMatch trie_getMatch_initial trie (Text.unpack $ fromJust1 "c1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
         Nothing
           -> Nothing
         Just ((m, _), calcParams)
           -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

-- | Use a shorter name.
createP :: CSVLineParser -> Bool -> RatePlanParser
createP lp useHeader = createRatePlanParserFromCSVLineParser lp useHeader []

-- | Parse a CSV file in a StdFormat with cost-by-minute.
parse_csvStdFormat
  :: FieldSeparator
  -> DecimalSeparator
  -> Int
  -- ^ how many initial fields to ignore on the left
  -> Int
  -- ^ how many ending fields to ignore on the right
  -> Bool
  -- ^ True if the telephone prefix field contains an implicit "*" suffix. In this way "321" match "321*" telephone numbers.
  --   False if the telephone numbers are specified using explicit prefixes like "321XXX" in the Asterisell format.
  -> Bool
  -- ^ True if there is after, the cost-for-minute, there is the field "minimum billable seconds"
  -> Bool
  -- ^ True if there is after, the field "cost-on-call"
  -> Bool
  -- ^ True if there is after the field "when round to next minute"
  -> CSVLineParser

parse_csvStdFormat fieldSeparator decimalSeparator rowsToIgnoreStart rowsToIgnoreEnd useNormalPrefixes thereIsMinimumBillableSeconds thereIsCostOnCall thereIsWhenRoundNextMinute en = do
  count rowsToIgnoreStart $ do
    skipCSVField fieldSeparator
    char fieldSeparator

  prefix1 <- parseCSVString fieldSeparator
  let prefix = if useNormalPrefixes then (Text.snoc prefix1 '*') else prefix1
  char fieldSeparator
  costByMinute <- parseCost decimalSeparator fieldSeparator
  minimumBillableSeconds
    <- case thereIsMinimumBillableSeconds of
         True
           -> do char fieldSeparator
                 v <- parseInt
                 return $ Just v
         False
           -> do return Nothing
  costOnCall
    <- case thereIsCostOnCall of
         False
           -> do return Nothing
         True
           -> do char fieldSeparator
                 v <- parseCost decimalSeparator fieldSeparator
                 return $ Just $ RateCost_cost v

  whenRoundNextMinute
    <- case thereIsWhenRoundNextMinute of
         False
           -> return Nothing
         True
           -> do char fieldSeparator
                 v <- parseInt
                 return $ Just $ Just v

  count rowsToIgnoreEnd $ do
    char fieldSeparator
    skipCSVField fieldSeparator

  let calcParams
        = calcParams_empty {
              calcParams_costForMinute = Just costByMinute
            , calcParams_costOnCall = costOnCall
            , calcParams_atLeastXSeconds = minimumBillableSeconds
            , calcParams_durationDiscreteIncrements = whenRoundNextMinute
          }

  return [(prefix, calcParams)]


parseCost decimalSeparator fieldSeparator
    = do cost1 <- parseCSVString fieldSeparator
         let cost2 = if decimalSeparator == '.'
                     then cost1
                     else Text.map (\c -> if c == decimalSeparator then '.' else c) cost1

         case fromTextToRational cost2 of
           Nothing -> fail $ "\"" ++ Text.unpack cost1 ++ "\" is not a valid monetary value using \"" ++ show decimalSeparator ++ "\" as decimal separator."
           Just v -> return v

-- -------------------------------------------
-- Implement Specific Rates

-- | Parse a CSV file in TWT NNG Format.
--   Something like
--
--   > Operator Name,Operator Type,Location,Prefix,Cost by Minute
--   > Italy,Fixed,Italy,TollFree - Italy Fixed -,0.012
--   > Italy,Fixed,Italy,TollFree - Italy OLO ,0.012
--
parse_twtNngFormat
  :: DecimalSeparator
  -> CSVLineParser

parse_twtNngFormat decimalSeparator env
    = do skipCSVField ','
         char ','

         skipCSVField ','
         char ','

         skipCSVField ','
         char ','

         prefix1 <- parseCSVString ','
         char ','

         costByMinute <- parseCost decimalSeparator ','

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   }

         return $ [(Text.concat ["incoming-toll-free-", prefix1, "*"], calcParams)]

-- | Parse a CSV file in TWT Format, with prefix first. Something like:
--
--   > CODES,DESTINATION,RATE,VALIDITY,COMMENT
--   > 355,Albania,0.066,22/02/2010,decrease
--   > 35538,Albania,0.066,22/02/2010,decrease
--
parse_twtNngFormat5ColPrefixFirst
  :: DecimalSeparator
  -> CSVLineParser

parse_twtNngFormat5ColPrefixFirst decimalSeparator env
    = do prefix1 <- parseCSVString ','
         let prefix = Text.snoc prefix1 '*'
         char ','

         skipCSVField ','
         char ','

         costByMinute <- parseCost decimalSeparator ','
         char ','

         skipCSVField ','
         char ','

         skipCSVField ','

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   }

         return [(prefix, calcParams)]

-- | Parse a CSV file in Gamma Format.
--   The code must be keept in synchro with the CDR importer, because it manage time-band in a custom way.
parse_gammaFormat9Col :: CSVLineParser

parse_gammaFormat9Col env
    = do prefix <- parseCSVString ','
         char ','

         let timeBandAndChargeCode timeBand = Text.concat [prefix, "----time-band-", timeBand, "-*"]
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
parse_gammaItemRentalFormat6Col :: CSVLineParser

parse_gammaItemRentalFormat6Col env
    = do skipCSVField ','
         char ','

         prefix1 <- parseCSVString ','
         char ','
         -- NOTE: despite other rates, in this case the prefix must be matched exactly, so no implicit "*" is added at the end of prefix
         -- add info about the peak time-band

         frequency <- parseCSVString ','
         char ','

         prefix
           <- case frequency of
                   "Rental Per (1 Month)" -> return $ Text.append gamma_ItemRental_rental prefix1
                   "Rental Per (Month)" -> return $ Text.append gamma_ItemRental_rental prefix1
                   "Single Charge" -> return $ Text.append gamma_ItemRental_connection prefix1
                   _ -> fail $ (Text.unpack frequency) ++ " is not a valid frequency value."

         -- skip rules field
         skipCSVField ','
         char ','

         cost <- parseCost '.' ','
         char ','

         -- skip applied field
         skipCSVField ','

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just 0
                   ,   calcParams_costOnCall = Just $ RateCost_cost cost
                   }

         return [(prefix, calcParams)]

-- | Parse a CSV file in standad format, with prefix first. Something like:
--
--   > CODES,DESTINATION,RATE
--   > 355,Albania,0.066
--   > 35538,Albania,0.066
--
parse_csvWith3ColsPDR
  :: Char
  -> DecimalSeparator
  -> CSVLineParser

parse_csvWith3ColsPDR fieldSeparator decimalSeparator env
    = do prefix1 <- parseCSVString fieldSeparator
         let prefix = Text.snoc prefix1 '*'
         char fieldSeparator

         skipCSVField fieldSeparator
         char fieldSeparator

         costByMinute <- parseCost decimalSeparator fieldSeparator

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   }

         return [(prefix, calcParams)]

-- | Parse a CSV file in Digitel NNG Format.
--
--   > "Numerazione","Operatore-Servizio","Peak","Off-Peak","Scatto","Durata Minima"
--   > 112,"Carabinieri",0,0,0,0
--   > 113,"Soccorso Pubblica Emergenza",0,0,0,0
--
--   The code must be keept in synchro with the CDR importer, because it manage time-band in a custom way.
parse_digitelNNGFormat :: CSVLineParser
parse_digitelNNGFormat env
    = do prefix <- parseCSVString ','
         char ','

         let prefixT = makeDigitelNNGPrefix True prefix
         let prefixF = makeDigitelNNGPrefix False prefix
         
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

         let calcParams costByMinute 
                 = calcParams_empty {
                       calcParams_costForMinute = Just costByMinute
                   ,   calcParams_atLeastXSeconds = Just $ minimuLen
                   ,   calcParams_costOnCall = Just $ RateCost_cost initialCost
                   }

         return [(prefixT, calcParams costByMinute1 )
                ,(prefixF, calcParams costByMinute2)
                ] 

freeCall = calcParams_empty

makeDigitelNNGPrefix :: Bool -> Text.Text -> Text.Text
makeDigitelNNGPrefix isPeak prefix
    = Text.concat [const_digitelNNGTimeBandPrefix isPeak, "39", prefix, "*"]
 
-- -------------------------------------------
-- Define Supported Rates

-- | The default rate parsers to use for importing rates.
--   NOTE: the parsers are configured on a separate module for each type of parser.
configuredRateParsers :: ConfiguredRatePlanParsers
configuredRateParsers
  = Map.fromList $
      [ ("rate-plan-specification", mainRatePlanParser)
      , ("csv-header-3col", createP (parse_csvStdFormat ',' '.' 1 0 True False False False ) True)
      , ("csv-header-3col-last-descr", createP (parse_csvStdFormat ',' '.' 0 1 True False False False) True )
      , ("csv-header-4col", createP (parse_csvStdFormat ',' '.' 2 0 True False False False ) True )
      , ("csv-header-3col-italian", createP (parse_csvStdFormat ',' ',' 1 0 True False False False ) True )
      , ("csv-header-4col-costOnCall", createP (parse_csvStdFormat ',' '.' 1 0 True False True False ) True )
      , ("csv-header-5col-costOnCall", createP (parse_csvStdFormat ',' '.' 2 0 True False True False ) True )
      , ("csv-header-6col-costOnCall", createP (parse_csvStdFormat ',''.' 3 0 True False True False ) True )
      , ("csv-twt-header-7col", createP (parse_csvStdFormat ',' '.' 1 4 True False False False ) True )
      , ("csv-twt-header-7col-italian", createP (parse_csvStdFormat ';' ',' 1 4 True False False False ) True )
      , ("csv-twt-no-header-7col", createP (parse_csvStdFormat ',' '.' 1 4 True False False False ) True )
      , ("csv-twt-nng-5col", createP (parse_twtNngFormat '.') True)
      , ("csv-twt-header-5col", createP (parse_twtNngFormat5ColPrefixFirst '.') True)
      , ("csv-twt-no-header-5col", createP (parse_twtNngFormat5ColPrefixFirst '.') False)
      , ("csv-gamma-header-9col", createP parse_gammaFormat9Col True)
      , ("csv-gamma-item-rental-6col", createP parse_gammaItemRentalFormat6Col True)
      , ("csv-header-3col-pref-descr-rate-it", createP (parse_csvWith3ColsPDR ';' ',') True)
      , ("csv-header-3col-pref-descr-rate", createP (parse_csvWith3ColsPDR ',' '.') True)
      , ("csv-digitel-nng", createRatePlanParserFromCSVLineParser
                              parse_digitelNNGFormat
                              True
                              [(makeDigitelNNGPrefix True "800", freeCall)
                              ,(makeDigitelNNGPrefix False "800", freeCall)])
      ]

-- --------------------------------
-- Unit Tests

tt_specificRates = []
