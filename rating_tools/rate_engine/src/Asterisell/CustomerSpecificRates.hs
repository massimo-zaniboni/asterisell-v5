{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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
import Asterisell.ParseRatePlan
import Asterisell.Holiday

import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Monad (void)
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import Text.Megaparsec.Error as P
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as Text
import Data.Maybe
import Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (when)
import Debug.Trace
import Text.Heredoc
import Data.List as List
import qualified Data.Csv as CSV
import GHC.Generics (Generic, Generic1)
import qualified Data.Trie.BigEndianPatricia.Base as Trie
import qualified Data.Trie.BigEndianPatricia.Internal as TrieInternal
import qualified Data.Trie.BigEndianPatricia.Convenience as Trie

import qualified Test.HUnit as HUnit

-- -------------------------------------------
-- CSV Parsing.

parser_skipLine :: Parser ()
parser_skipLine = do
   let isNotNL c = c /= '\n' && c /= '\r'
   _ <- P.takeWhileP Nothing isNotNL
   (P.eof <|> parse_nl)
   return ()

parser_cost :: Char -> Char -> Parser MonetaryValue
parser_cost decimalSeparator fieldSeparator
    = do cost1 <- parseCSVString fieldSeparator
         let cost2 = if decimalSeparator == '.'
                     then cost1
                     else Text.map (\c -> if c == decimalSeparator then '.' else c) cost1

         case fromTextToRational cost2 of
           Nothing -> fail $ "\"" ++ Text.unpack cost1 ++ "\" is not a valid monetary value using \"" ++ show decimalSeparator ++ "\" as decimal separator."
           Just v -> return v

parse_nl :: Parser ()
parse_nl = do _ <- P.eol
              return ()
{-#INLINE parse_nl #-}

parse_knownIdentifier :: String -> Set.Set Text.Text -> Char -> Parser Text.Text
parse_knownIdentifier idType allowedIds fieldSeparator = do
  s <- parseCSVString fieldSeparator
  case Set.member s allowedIds of
    True -> return s
    False -> fail $ "Unrecognized " ++ idType ++ " " ++ Text.unpack s
                      ++ ". Expected values are: " ++ List.concat (List.intersperse ", " (List.map Text.unpack $ Set.toList allowedIds))

parse_communicationChannel :: RatingParams -> Char -> Parser Int
parse_communicationChannel env fieldSeparator = do
  s <- parse_knownIdentifier "communication channel" (Map.keysSet $ params_channelTypes env) fieldSeparator
  return $ fromJust $ Map.lookup s (params_channelTypes env) 

parse_ratingCode :: RatingParams -> Char -> Parser Text.Text
parse_ratingCode env fieldSeparator = do
  parse_knownIdentifier "prefix table rating code" (params_ratingCodes env) fieldSeparator

parse_peakCode :: RatingParams -> Char -> Parser Text.Text
parse_peakCode env fieldSeparator = do
  parse_knownIdentifier "holiday table peak-code" (params_peakCodes env) fieldSeparator

-- | Build a `RatePlanParser`, ensuring that all the content of the rate is parsed.
parser_ratePlan :: (RatingParams -> Parser MainRatePlan) -> RatePlanParser
parser_ratePlan rateParser env rateContent
  = case Text.decodeUtf8' rateContent of
      Left err
        -> Left $ "The rate content is not in UTF8 format: " ++ show err ++ ". Convert the rate in UTF8 format, and update it in Asterisell database."
      Right rateContentT
        -> let pr = runParser
                     (do r <- rateParser env
                         P.eof
                         return r
                     ) "" rateContentT
           in case pr of
                Right prr -> Right prr
                Left err -> Left $ P.errorBundlePretty err 

-- | Parse a line of a CSV file, and return the text to use as input for the matching-fun,
 --  and the calc params to use in case the CDR match this text.
--   The text can be a telephone prefix, an operator-code, a peak-code, and so on.
--
--   It is used for specifying only the relevant part of parsers with a common structure,
--   and then passing as parameter a `CSVLineParser` function.
--
--   The parser is in UTF8 format.
--   The new line must be not consumed/recognized from this parser.
--   NOTE: if possible do not use slow MegaParsec combinators.
type CSVLineParser = RatingParams -> Parser [(Text.Text, CalcParams)]

createRatePlanParserFromCSVLineParserWithMatchFun
    :: CSVLineParser
    -> Bool
    -> [(Text.Text, CalcParams)]
    -> (Trie CalcParams -> RatingParams -> CDR -> Maybe (MatchStrenght, CalcParams))
       -- ^ the match fun to use for building the rate
    -> RatePlanParser

createRatePlanParserFromCSVLineParserWithMatchFun lineParser skipHeader initialRates deriveRateMatchFun
  = parser_ratePlan mainParser

 where

   mainParser envParams
     = do when skipHeader parser_skipLine

          trie <- parseAllLines envParams (trie_update trie_empty initialRates)

          let ratePlan = RatePlan {
                           rate_systemId = 0
                         , rate_parentSystemId = Nothing
                         , rate_userId = ""
                         , rate_matchBeforeUse = deriveRateMatchFun trie
                         , rate_children = []
                         , rate_elsePart = []
                         , rate_use = Nothing
                         , rate_bundleParams = Nothing
                         }

          return mainRatePlan_empty {
                   mainRatePlan_bundleRates = []
                 , mainRatePlan_normalRates = [ratePlan]
                 }

   parseAllLines :: RatingParams -> Trie CalcParams -> Parser (Trie CalcParams)
   parseAllLines envParams trie1
     = (do P.eof 
           return trie1) <|>
       (do prefixesAndParams <- lineParser envParams 
           let trie2 = trie_update trie1 prefixesAndParams
           P.eof <|> parse_nl 
           parseAllLines envParams trie2)

   trie_update trie1 prefixesAndParams
     = List.foldl' (\trie (prefix, params) -> trie_insertExtension trie (fromTextToByteString prefix) params) trie1 prefixesAndParams

-- | Like `createRatePlanParserFromCSVLineParserWithMatchFun` using a match fun on telephone prefix.
createRatePlanParserOnTelephonePrefixFromCSVLineParser :: CSVLineParser -> Bool -> [(Text.Text, CalcParams)] -> RatePlanParser
createRatePlanParserOnTelephonePrefixFromCSVLineParser lineParser skipHeader initialRates
  =  let deriveRateMatchFun trie env cdr
           = case trie_match trie (fromTextToByteString $ fromJust1 "c1"  $ cdr_externalTelephoneNumberWithAppliedPortability cdr) of
               Nothing
                 -> Nothing
               Just (m, _, calcParams)
                 -> Just (MatchStrenght { matchStrenght_telephoneNumber = m }, calcParams)
     in createRatePlanParserFromCSVLineParserWithMatchFun lineParser skipHeader initialRates deriveRateMatchFun

-- | Like `createRatePlanParserFromCSVLineParserWithMatchFun` using a match fun on rating-code telephone prefix recognition.
--   In case rating-codes are specified as `some-code*`, then the max matching lenght is returned,
--   like in case of telephone prefixes.
createRatePlanParserOnRatingCodeFromCSVLineParser
    :: CSVLineParser
    -> Bool
    -> Bool
       -- ^ True for forcing the maximum possible match strenght, instead of the prefix len one.
    -> [(Text.Text, CalcParams)]
    -> RatePlanParser

createRatePlanParserOnRatingCodeFromCSVLineParser lineParser skipHeader useMaxStrength initialRates
  =  let deriveRateMatchFun trie env cdr
           = case trie_match trie (fromTextToByteString $ cdr_ratingCode cdr) of
               Nothing
                 -> Nothing
               Just (m, _, calcParams)
                 -> let strenght = case useMaxStrength of
                                     True -> Text.length $ fromJust1 "c2" $ cdr_externalTelephoneNumberWithAppliedPortability cdr
                                     False -> m

                   in Just (MatchStrenght { matchStrenght_telephoneNumber = strenght }, calcParams)
     in createRatePlanParserFromCSVLineParserWithMatchFun lineParser skipHeader initialRates deriveRateMatchFun

-- | Use a shorter name.
createP :: CSVLineParser -> Bool -> RatePlanParser
createP lp useHeader = createRatePlanParserOnTelephonePrefixFromCSVLineParser lp useHeader []

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

parse_csvStdFormat fieldSeparator decimalSeparator colsToIgnoreStart colsToIgnoreEnd useNormalPrefixes thereIsMinimumBillableSeconds thereIsCostOnCall thereIsWhenRoundNextMinute en = do
  count colsToIgnoreStart $ do
    skipCSVField fieldSeparator
    char fieldSeparator

  prefix1 <- parseCSVString fieldSeparator
  let prefix = if useNormalPrefixes then (Text.snoc prefix1 '*') else prefix1
  char fieldSeparator
  costByMinute <- parser_cost decimalSeparator fieldSeparator
  minimumBillableSeconds
    <- case thereIsMinimumBillableSeconds of
         True
           -> do char fieldSeparator
                 v <- parseInt
                 return $ CP_Value v
         False
           -> do return CP_Nothing
  costOnCall
    <- case thereIsCostOnCall of
         False
           -> do return CP_Nothing
         True
           -> do char fieldSeparator
                 v <- parser_cost decimalSeparator fieldSeparator
                 return $ CP_Value v

  whenRoundNextMinute
    <- case thereIsWhenRoundNextMinute of
         False
           -> return CP_Nothing
         True
           -> do char fieldSeparator
                 v <- parseInt
                 return $ CP_Value v

  count colsToIgnoreEnd $ do
    char fieldSeparator
    skipCSVField fieldSeparator

  let calcParams
        = calcParams_empty {
              calcParams_costForMinute = CP_Value costByMinute
            , calcParams_costOnCall = costOnCall
            , calcParams_atLeastXSeconds = minimumBillableSeconds
            , calcParams_durationDiscreteIncrements = whenRoundNextMinute
          }

  return [(prefix, calcParams)]


-- -------------------------------------------
-- Normalize rates

-- | Parse a CSV file in a StdFormat with cost-by-minute, and normalize it to csv-header-5col-costOnCall format,
--   like requested by NormalizeRatePlan
normalize_csvRate
  :: FieldSeparator
  -> DecimalSeparator
  -> Bool
  -- ^ there is header
  -> Int
  -- ^ how many fields on a line
  -> Maybe Int
  -- ^ description field, starting from 0
  -> Maybe Int
  -- ^ operator field, starting from 0
  -> Int
  -- ^ prefix col, starting from 0
  -> Int
  -- ^ cost by minute field, starting from 0
  -> Maybe Int
  -- ^ cost on call field, starting from 0
  -> RatePlanNormalizer

normalize_csvRate fieldSeparator decimalSeparator skipHeader colsNr maybeDescrCol maybeOperatorCol prefixCol costByMinuteCol maybeCostOnCallCol rateContent = do
  case Text.decodeUtf8' rateContent of
      Left err
        -> Left $ "The rate content is not in UTF8 format: " ++ show err ++ ". Convert the rate in UTF8 format, and update it in Asterisell database."
      Right rateContentT
        -> case runParser csvParser "" rateContentT of
             Right prr ->
               Right $ Trie.fromList $ List.map (\r -> (fromTextToByteString $ nrr_prefix r, r)) prr
             Left err ->
               Left $ describeParser ++ P.errorBundlePretty err

 where

  describeParser :: String =
    "Parser: normalize_csvRate with field separator " ++ show fieldSeparator ++
      ", decimal separator " ++ show decimalSeparator ++
      ", " ++ (if skipHeader then "header on first row" else "no header on first row") ++
      ", " ++ show colsNr ++ " fields in each row" ++
      ", telephone prefix at row (starting from 0) " ++ show prefixCol ++
      ", cost by minute at row " ++ show costByMinuteCol ++
      ", description at row " ++ show maybeDescrCol ++
      ", operator at row " ++ show maybeOperatorCol ++
      ", cost on call at row " ++ show maybeCostOnCallCol ++
      ". "

  csvParser :: Parser [NormalizedRate]
  csvParser = do
    when skipHeader parser_skipLine
    nrrs <- P.many $ parseRow 0 normalizedRate_empty
    P.eof

    return nrrs

  parseRow :: Int -> NormalizedRate -> Parser NormalizedRate
  parseRow rc nrr
    | (Just rc == maybeDescrCol) = do
        s <- parseCSVString fieldSeparator
        maybeSep rc
        parseRow (rc + 1) (nrr { nrr_description = s })

    | (Just rc == maybeOperatorCol) = do
        s <- parseCSVString fieldSeparator
        maybeSep rc
        parseRow (rc + 1) (nrr { nrr_operator = s })

    | (rc == prefixCol) = do
        s <- parseCSVString fieldSeparator
        maybeSep rc
        parseRow (rc + 1) (nrr { nrr_prefix = s })

    | (rc == costByMinuteCol) = do
        c <- parser_cost decimalSeparator fieldSeparator
        maybeSep rc
        parseRow (rc + 1) (nrr { nrr_costByMinute = c })

    | (Just rc == maybeCostOnCallCol) = do
        c <- parser_cost decimalSeparator fieldSeparator
        maybeSep rc
        parseRow (rc + 1) (nrr { nrr_costOnCall = c })

    | rc < colsNr = do
        skipCSVField fieldSeparator
        maybeSep rc
        parseRow (rc + 1) nrr

    | otherwise = do
        return nrr

  maybeSep :: Int -> Parser ()
  maybeSep rc =
    case rc < (colsNr - 1) of
      True -> do
        _ <- P.char fieldSeparator
        return ()

      False -> do
       (P.eof <|> parse_nl)
       return ()

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

         costByMinute <- parser_cost decimalSeparator ','

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = CP_Value costByMinute
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

         costByMinute <- parser_cost decimalSeparator ','
         char ','

         skipCSVField ','
         char ','

         skipCSVField ','

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = CP_Value costByMinute
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

         costByMinute1 <- parser_cost '.' ','
         char ','

         costByMinute2 <- parser_cost '.' ','
         char ','

         costByMinute3 <- parser_cost '.' ','
         char ','

         setupCost1 <- parser_cost '.' ','
         char ','

         setupCost2 <- parser_cost '.' ','
         char ','

         setupCost3 <- parser_cost '.' ','
         char ','

         minimumCharge <- parser_cost '.' ','

         let calcParams costByMinute setupCost
                 = calcParams_empty {
                       calcParams_costForMinute = CP_Value costByMinute
                   ,   calcParams_costOnCall = CP_Value setupCost
                   ,   calcParams_minCostOfCall = CP_Value minimumCharge
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

         cost <- parser_cost '.' ','
         char ','

         -- skip applied field
         skipCSVField ','

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = CP_Nothing
                   ,   calcParams_costOnCall = CP_Value cost
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

         costByMinute <- parser_cost decimalSeparator fieldSeparator

         let calcParams
                 = calcParams_empty {
                       calcParams_costForMinute = CP_Value costByMinute
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

         costByMinute1 <- parser_cost ',' ','
         char ','

         costByMinute2 <- parser_cost ',' ','
         char ','

         initialCost <- parser_cost ',' ','
         char ','

         minimuLen <- parseInt

         let calcParams costByMinute
                 = calcParams_empty {
                       calcParams_costForMinute = CP_Value costByMinute
                   ,   calcParams_atLeastXSeconds = CP_Value minimuLen
                   ,   calcParams_costOnCall = CP_Value initialCost
                   }

         return [(prefixT, calcParams costByMinute1 )
                ,(prefixF, calcParams costByMinute2)
                ]

freeCall = calcParams_empty

makeDigitelNNGPrefix :: Bool -> Text.Text -> Text.Text
makeDigitelNNGPrefix isPeak prefix
    = Text.concat [const_digitelNNGTimeBandPrefix isPeak, "39", prefix, "*"]


-- | Store in a compact way the matching conditions.
data ECNMatch
        = ECNMatch {
            ecn_peakCodes :: StringIndexDictionary
          , ecn_operators :: StringIndexDictionary
          , ecn_match :: Map.Map (Int
                                  -- ^ communication channel
                                 , Int
                                   -- ^ operator
                                 , Maybe Int
                                   -- ^ Nothing for peak code, the off-peak code
                                 )
                                 CalcParams
          }

ecn_empty :: ECNMatch

ecn_empty = ECNMatch {
              ecn_peakCodes = sdict_empty
            , ecn_operators = sdict_empty
            , ecn_match = Map.empty
            }

ecn_insert
    :: ECNMatch
    -> Int -- ^ communication channel
    -> Text.Text -- ^ operator
    -> Maybe Text.Text -- ^ peak code, Nothing for peak calls
    -> CalcParams
    -> ECNMatch

ecn_insert m1 chI op mpk cp
   = let (pkI, m2)
             = case mpk of
                Just n -> let (m2', i) = sdict_insert (ecn_peakCodes m1) n
                          in  (Just i, m1 { ecn_peakCodes = m2'})
                Nothing -> (Nothing, m1)

         (opI, m3)
           = let (m3', i) = sdict_insert (ecn_operators m2) op
             in  (i, m2 { ecn_operators = m3'})

         m4' = Map.insert (chI, opI, pkI) cp (ecn_match m3)

     in  m3 { ecn_match = m4' }

ecn_toMatchFun :: ECNMatch -> MatchFun

ecn_toMatchFun m env cdr
  = let
        m1 = ecn_match m
        channelI = fromJust1 "c55" $ cdr_communicationChannelTypeId cdr
        (_, operatorI) = sdict_insert (ecn_operators m) (cdr_ratingCode cdr)
        peakCodesN = Set.toList $ holidays_match (params_holidays env) (cdr_calldate cdr)
        peakCodesI = List.map (\c -> Just $ snd $ sdict_insert (ecn_peakCodes m) c) peakCodesN
        peakCodesI'
          = case List.null peakCodesI of
              True -> [Nothing]
              False -> peakCodesI
        solutions = List.foldl'
                      (\r pc
                          -> case Map.lookup (channelI, operatorI, pc) m1 of
                               Nothing -> r
                               Just x -> x:r
                      ) [] peakCodesI'

    in case solutions of
         []  -> Nothing
         [calcParams] -> Just (matchStrenght_initial, calcParams)
         _ -> error
                ("These peak-codes "
                        ++ List.concat (List.intersperse ", " (List.map Text.unpack peakCodesN))
                        ++ " defined in Holiday table are not distinct, but they are used in a rate CSV file,"
                        ++ " and so it is not clear which cost apply.") Nothing

-- | Parse a CSV file in a ECN-like format:
--   * description (not used)
--   * communication-channel to match
--   * operator code (rating-code in telephone-prefix table) to match
--   * cost by minute of peak calls (the default calls not matching any peak-code in the holiday table)
--   * cost by minute of calls matching some specified peak-code as specified in the holiday table
--   * continue with more cost by minute on peak-code
--
--  The headers are parsed in this way:
--  * free text
--  * free text
--  * free text
--  * "peak"
--  * other name of offpeak-code in holiday table
--  * other name of offpeak-code in holiday table
--  * ...
--
--  Every peak and offpeak code column will contain the cost for calls
--  having the calltime in this time-frame.
--
parse_ecnNationalCallsFormat
  :: FieldSeparator
  -> DecimalSeparator
  -> RatePlanParser

parse_ecnNationalCallsFormat fieldSeparator decimalSeparator
  = parser_ratePlan $ \envParams -> (do

      _ <- parseCSVString fieldSeparator
      _ <- P.char fieldSeparator

      _ <- parseCSVString fieldSeparator
      _ <- P.char fieldSeparator

      _ <- parseCSVString fieldSeparator
      _ <- P.char fieldSeparator

      _ <- P.try $ P.string "peak"

      offPeakCodes <- P.many $ do _ <- P.char fieldSeparator
                                  parse_peakCode envParams fieldSeparator

      P.eof <|> parse_nl

      ecn <- parseAllLines envParams offPeakCodes ecn_empty

      let ratePlan = RatePlan {
                        rate_userId = ""
                      , rate_systemId = 0
                      , rate_parentSystemId = Nothing
                      , rate_matchBeforeUse = ecn_toMatchFun ecn
                      , rate_children = []
                      , rate_elsePart = []
                      , rate_use = Nothing
                      , rate_bundleParams = Nothing
                      }

      return mainRatePlan_empty {
                 mainRatePlan_bundleRates = []
               , mainRatePlan_normalRates = [ratePlan]
               }

      )

 where

   parseAllLines :: RatingParams -> [Text.Text] -> ECNMatch -> Parser ECNMatch
   parseAllLines env offPeakNames m1
     = (do P.eof
           return m1) <|>
       (do m2 <- parseLine env offPeakNames m1 
           parseAllLines env offPeakNames m2)

   parseLine :: RatingParams -> [Text.Text] -> ECNMatch -> Parser ECNMatch
   parseLine env offPeakCodes m1 = do

      -- skip description
      _ <- parseCSVString fieldSeparator
      _ <- P.char fieldSeparator

      channelI <- parse_communicationChannel env fieldSeparator
      _ <- P.char fieldSeparator

      operatorCode <- parse_ratingCode env fieldSeparator
      _ <- P.char fieldSeparator

      peakCost :: MonetaryValue
        <- parser_cost decimalSeparator fieldSeparator

      offPeakCosts :: [MonetaryValue]
        <- P.count
             (List.length offPeakCodes)
             (do _ <- char fieldSeparator
                 parser_cost decimalSeparator fieldSeparator)

      (P.eof <|> parse_nl)

      let peakNames = [Nothing] ++ List.map Just offPeakCodes

      let m2 = List.foldl'
                 (\m (pN, pC)
                      -> ecn_insert m channelI operatorCode pN (calcParams_empty {calcParams_costForMinute = CP_Value pC }))
                 m1
                 (List.zip peakNames (peakCost:offPeakCosts))

      return m2

-- -------------------------------------------
-- Define Supported Rates

-- | The default rate parsers to use for importing rates.
--   DEV-NOTE: update also `apps/asterisell/lib/jobs/admin/InitRateFormats.php`
configuredRateParsers :: ConfiguredRatePlanParsers
configuredRateParsers
  = Map.fromList $
      [ ("rate-plan-specification",
         (mainRatePlanParser,
          Nothing))
      , ("csv-header-3col",
         (createP (parse_csvStdFormat ',' '.' 1 0 True False False False) True,
          Just $ normalize_csvRate ',' '.' True 3 (Just 0) Nothing 1 2 Nothing))
      , ("csv-header-3col-last-descr",
         (createP (parse_csvStdFormat ',' '.' 0 1 True False False False) True,
          Just $ normalize_csvRate ',' '.' True 3 (Just 2) Nothing 0 1 Nothing))
      , ("csv-header-4col",
         (createP (parse_csvStdFormat ',' '.' 2 0 True False False False ) True ,
          Just $ normalize_csvRate ',' '.' True 4 (Just 0) (Just 1) 2 3 Nothing))
      , ("csv-header-3col-italian",
         (createP (parse_csvStdFormat ',' ',' 1 0 True False False False ) True ,
          Just $ normalize_csvRate ',' ',' True 3 (Just 0) Nothing 1 2 Nothing))
      , ("csv-header-4col-costOnCall",
         (createP (parse_csvStdFormat ',' '.' 1 0 True False True False ) True ,
          Just $ normalize_csvRate ',' '.' True 5 (Just 0) (Just 1) 2 3 (Just 4)))
      , ("csv-header-4col-rating-code-costOnCall"
        , (createRatePlanParserOnRatingCodeFromCSVLineParser (parse_csvStdFormat ',' '.' 1 0 False False True False) True True [],
           Just $ normalize_csvRate ',' '.' True 4 Nothing (Just 0) 2 3 (Just 3)))
      , ("csv-header-5col-costOnCall",
         (createP (parse_csvStdFormat ',' '.' 2 0 True False True False ) True ,
          Just $ normalize_csvRate ',' '.' True 5 (Just 0) (Just 1) 2 3 (Just 4)))
      , ("csv-header-6col-costOnCall",
         (createP (parse_csvStdFormat ',''.' 3 0 True False True False ) True ,
          Just $ normalize_csvRate ',' '.' True 6 (Just 0) (Just 1) 3 4 (Just 5)))
      , ("csv-twt-header-7col",
         (createP (parse_csvStdFormat ',' '.' 1 4 True False False False ) True ,
          Nothing))
      , ("csv-twt-header-7col-italian",
         (createP (parse_csvStdFormat ';' ',' 1 4 True False False False ) True ,
          Nothing))
      , ("csv-twt-no-header-7col",
         (createP (parse_csvStdFormat ',' '.' 1 4 True False False False ) True ,
          Nothing))
      , ("csv-twt-nng-5col",
         (createP (parse_twtNngFormat '.') True,
          Nothing))
      , ("csv-twt-header-5col",
         (createP (parse_twtNngFormat5ColPrefixFirst '.') True,
          Just $ normalize_csvRate ',' '.' True 5 (Just 1) Nothing 0 2 Nothing))
      , ("csv-twt-no-header-5col",
         (createP (parse_twtNngFormat5ColPrefixFirst '.') False,
          Just $ normalize_csvRate ',' '.' False 5 (Just 1) Nothing 0 2 Nothing))
      , ("csv-gamma-header-9col",
         (createP parse_gammaFormat9Col True,
          Nothing))
      , ("csv-gamma-item-rental-6col",
         (createP parse_gammaItemRentalFormat6Col True,
          Nothing))
      , ("csv-header-3col-pref-descr-rate-it",
         (createP (parse_csvWith3ColsPDR ';' ',') True,
          Just $ normalize_csvRate ';' ',' True 3 (Just 1) Nothing 0 2 Nothing))
      , ("csv-header-3col-pref-descr-rate",
         (createP (parse_csvWith3ColsPDR ',' '.') True,
          Just $ normalize_csvRate ',' '.' True 3 (Just 1) Nothing 0 2 Nothing))
      , ("csv-digitel-nng",
         (createRatePlanParserOnTelephonePrefixFromCSVLineParser
                              parse_digitelNNGFormat
                              True
                              [(makeDigitelNNGPrefix True "800", freeCall)
                              ,(makeDigitelNNGPrefix False "800", freeCall)],
          Nothing))
      , ("csv-ecn",
          (parse_ecnNationalCallsFormat ',' '.',
          Nothing))
      ]

-- --------------------------------
-- Unit Tests

tt_specificRates = []
