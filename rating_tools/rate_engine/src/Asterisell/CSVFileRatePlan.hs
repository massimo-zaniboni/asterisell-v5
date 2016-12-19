{-# Language OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

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


-- | Support CSV based external-rate plan.
--
module Asterisell.CSVFileRatePlan (
    DecimalSeparator
  , UseHeader
  , parse_csvStdFormat

) where

import Asterisell.Trie
import Asterisell.Utils
import Asterisell.RatePlan
import Asterisell.Cdr

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
import Control.DeepSeq
import Data.List as List

-- | The character used as decimal separator.
type DecimalSeparator = Char

type FieldSeparator = Char

type UseHeader = Bool

-- | Parse a CSV file in a StdFormat with cost-by-minute.
parse_csvStdFormat
  :: UseHeader
  -> FieldSeparator
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
  -> EnvParams
  -> LazyParser.Parser MainRatePlan

parse_csvStdFormat useHeader fieldSeparator decimalSeparator rowsToIgnoreStart rowsToIgnoreEnd useNormalPrefixes thereIsMinimumBillableSeconds thereIsCostOnCall thereIsWhenRoundNextMinute env
  = do case useHeader of
         True -> do skipWhile (\c -> not $ isEndOfLine c)
                    try $ (do endOfInput
                              return ()) <|>
                          (do endOfLine
                              return ())
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

       return mainRatePlan_empty {
                mainRatePlan_bundleRates = []
              , mainRatePlan_normalRates = [ratePlan]
              }

 where

  deriveRateMatchFun trie env cdr
    = case trie_getMatch 0 trie (Text.unpack $ fromJust1 "d1" $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
        Nothing
          -> Nothing
        Just (m, calcParams)
          -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)

  buildTrie trie1 (prefix, calcParams)
    = trie_addExtensionCode trie1 prefix calcParams

  parseAndBuildTrie
    = List.foldl' buildTrie trie_empty <$> many' parseLine

  parseLine
    = do count rowsToIgnoreStart $ do
           skipCSVField fieldSeparator
           char fieldSeparator

         prefix1 <- Text.unpack <$> parseCSVString fieldSeparator
         let prefix = if useNormalPrefixes then prefix1 ++ "*" else prefix1
         char fieldSeparator
         costByMinute <- parseCost
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
                        v <- parseCost
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

         (endOfLine <|> endOfInput)

         let calcParams
               = calcParams_empty {
                     calcParams_costForMinute = Just costByMinute
                   , calcParams_costOnCall = costOnCall
                   , calcParams_atLeastXSeconds = minimumBillableSeconds
                   , calcParams_durationDiscreteIncrements = whenRoundNextMinute
                 }

         return (prefix, calcParams)

  parseCost
    = do cost1 <- parseCSVString fieldSeparator
         let cost2 = if decimalSeparator == '.'
                     then cost1
                     else Text.map (\c -> if c == decimalSeparator then '.' else c) cost1

         case fromTextToRational cost2 of
           Nothing -> fail $ "\"" ++ Text.unpack cost1 ++ "\" is not a valid monetary value using \"" ++ show decimalSeparator ++ "\" as decimal separator."
           Just v -> return v
