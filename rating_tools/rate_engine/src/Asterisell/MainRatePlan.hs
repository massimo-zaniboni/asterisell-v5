{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes #-}

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


-- | Support the MainRatePlan format.
--
--   The main rate plan is a configuration language,
--   for combining different specific rates, and it is executed
--   for rating the CDRs.
--
module Asterisell.MainRatePlan(
    mainRatePlanParser
  , tt_rateSpecificationsTests
  , test_interactiveDebug
) where


import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy

import Numeric

import Data.List as List
import Data.Char
import Data.Either
import Data.Ratio
import Data.Ord as Ord
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Text.Encoding as Text
import Data.Text as Text
import qualified Test.HUnit as HUnit
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.Prim((<|>), (<?>))
import Text.Parsec.Text as Parsec
import System.IO
import Debug.Trace
import Data.Maybe
import Data.Set as Set
import Text.Heredoc

------------------------------------
-- PARSE MAIN RATE SPECIFICATIONS --
------------------------------------
-- See the PHP web interface for a description of the rate format.
-- or /apps/asterisell/lib/jobs/admin/InitRateFormats.php

mainRatePlanParser :: RatingParams -> BS.ByteString -> Either String MainRatePlan
mainRatePlanParser env rateContent
  = case Text.decodeUtf8' rateContent of
      Left err
        -> Left $ "The rate content is not in UTF8 format: " ++ show err ++ ". Convert the rate in UTF8 format, and update it in Asterisell database."
      Right rateContentT
        -> case Parsec.parse (mainRatePlanParser_parse env) "" rateContentT of
             Left err
               -> Left $ show err
             Right r
               -> Right r

-- | The entry point for parsing a main-rate.
--   For initializing a MainRatePlan with proper RateSystemId requires also the last processed BundleState.
mainRatePlanParser_parse :: RatingParams -> Parsec.Parser MainRatePlan
mainRatePlanParser_parse env
  = do
       rates :: [Either RootBundleRatePlan RatePlan]
         <- Parsec.many1
                  ((Parsec.try (do r <- p_rateOrClassPlan env
                                   return $ Right r)
                   ) <|> (do r <- p_rootBundleRate env
                             return $ Left r))
       Parsec.eof

       let (bundleRates, normalRates)
             = List.foldl' (\(b, n) r -> case r of
                                           Left x -> (x:b, n)
                                           Right x -> (b, x:n)
                           ) ([], []) rates

       let plan = mainRatePlan_empty {
                    mainRatePlan_bundleRates = bundleRates
                  , mainRatePlan_normalRates = normalRates
                  }

       case mainRatePlan_assignUniqueSystemId plan of
         Left err
           -> fail err
         Right r
           -> return r

-- | Used for specifying different types of rates.
p_rateOrClassPlan :: RatingParams -> Parsec.Parser RatePlan
p_rateOrClassPlan env
  = do p_freeSpaces
       Parsec.many p_comment
       p_freeSpaces

       isExternalRate
         <-     (do p_symbol "rate"
                    return False)
            <|> (do p_symbol "external-rate"
                    return True)

       p_beginDef

       id <- p_id

       children <- case isExternalRate of
                     True
                       -> do p_symbol "use:"
                             v <- p_referenceNameValue
                             return $ Left v
                     False
                       -> return $ Right []

       filters <- p_filters env True
       calcParams <- p_commonCalcParams

       children <- case isExternalRate of
                     True -> return children
                     False -> do r <- Parsec.many (p_rateOrClassPlan env)
                                 return $ Right r

       p_endDef
       p_freeSpaces

       elsePart :: [RatePlan]
         <- Parsec.option [] $ do
              p_symbol "else"
              p_beginDef
              elseRates <- Parsec.many1 $ p_rateOrClassPlan env
              p_endDef
              return elseRates

       Parsec.many p_comment
       p_freeSpaces

       let params
             = RateParams {
                 rate_userId = id
               , rate_match = matchFun_create filters calcParams
               }

       let plan
             = RatePlan {
                 rate_systemId = 0
               , rate_params = params
               , rate_children = children
               , rate_elsePart = elsePart
               }

       return plan

-- | Used for specifying different types of rates.
p_rootBundleRate :: RatingParams -> Parsec.Parser RootBundleRatePlan
p_rootBundleRate env
  = do p_freeSpaces
       Parsec.many p_comment
       p_freeSpaces

       p_symbol "bundle-rate"
       p_beginDef
       id <- p_id

       p_symbol "service-cdr-type:"
       serviceCdrType <- p_description

       p_symbol "service-cdr-description:"
       serviceCdrDescription <- p_description

       p_symbol "schedule:"
       schedule <- p_schedule
       p_symbol "schedule-from:"
       schedule <- p_scheduleFrom schedule

       p_symbol "apply-for-each:"
       priceCategories <- p_referenceList "price category" (fst $ params_rateCategories env)

       leftCalls <- p_maybeInt "none" "limit-on-first-calls:"
       leftDuration <- p_maybeInt "none" "limit-on-first-seconds:"

       p_symbol "limits-are-proportionals-to-activation-date:"
       proportionalLimits <- p_bool

       p_symbol "calls-can-be-split:"
       callsCanBeSplit <- p_bool

       p_symbol "only-for-calls-with-a-cost:"
       onlyForCallsWithACost <- p_bool

       bundleInitialCost <- p_moneyParamOrDefaultValue "set-bundle-initial-cost:" 0
       bundleMinCost <- p_moneyParamOrDefaultValue "set-bundle-min-cost:" 0

       filters <- p_filters env False
       calcParams <- p_commonCalcParams

       children
         <- do r <- Parsec.many (p_bundleRatePlan env)
               return $ Right r

       p_endDef
       p_freeSpaces
       Parsec.many p_comment
       p_freeSpaces

       let params
             = RateParams {
                 rate_userId = id
               , rate_match = matchFun_create filters calcParams
               }

       let bundleParams
             = BundleParams {
                 bundle_initialCost = bundleInitialCost
               , bundle_minCost = bundleMinCost
               , bundle_leftCalls = leftCalls
               , bundle_leftDuration = leftDuration
               , bundle_appliedCost = 0
               }

       let plan
             = BundleRatePlan {
                 bundle_systemId = 0
               , bundle_rateParams = params
               , bundle_bundleParams = bundleParams
               , bundle_children = children
                      }

       let rootPlan
             = RootBundleRatePlan {
                 bundle_serviceCDRType = serviceCdrType
               , bundle_serviceCDRDescription = serviceCdrDescription
               , bundle_timeFrame = schedule
               , bundle_priceCategoryIds = priceCategories
               , bundle_limitsAreProportionalsToActivationDate = proportionalLimits
               , bundle_canSplit = callsCanBeSplit
               , bundle_onlyForCallsWithACost = onlyForCallsWithACost
               , bundle_plan = plan
               }

       return rootPlan

 where

  p_schedule
    = (do p_symbol "monthly"
          p_nl
          return $ MonthlyTimeFrame 0
      )
      <|> (do p_symbol "weekly"
              p_nl
              return $ WeeklyTimeFrame 0
          )

  p_scheduleFrom :: BundleRateTimeFrame -> Parsec.Parser BundleRateTimeFrame
  p_scheduleFrom f
    = case f of
        WeeklyTimeFrame _
          -> do d <- p_dayOfWeek 1 ["Monday"
                                   ,"Tuesday"
                                   ,"Wednesday"
                                   ,"Thursday"
                                   ,"Friday"
                                   ,"Saturday"
                                   ,"Sunday"]
                return $ WeeklyTimeFrame d
        MonthlyTimeFrame _
         -> do i <- p_integer
               case i >= 1 && i <= 28 of
                 True -> return $ MonthlyTimeFrame i
                 False -> fail $ show i ++ " day of month can not be accepted: it must be between 1 and 28."

  p_dayOfWeek n (v:r)
    = (do p_symbol v
          p_nl
          return n) <|> p_dayOfWeek (n + 1) r

  p_dayOfWeek _ []
    = fail "Unrecognized day of week"

-- REFACTORING: join the code in common beetween p_rootBundleRate and p_bundleRatePlan

p_bundleRatePlan :: RatingParams -> Parsec.Parser BundleRatePlan
p_bundleRatePlan env
  = do p_freeSpaces
       Parsec.many p_comment
       p_freeSpaces

       isExternalRate
         <- (do p_symbol "rate"
                return (False))
            <|> (do p_symbol "external-rate"
                    return (True))
       p_beginDef

       id <- p_id
       filters <- p_filters env False
       leftCalls <- p_maybeIntOrNothing "none" "limit-on-first-calls:"
       leftDuration <- p_maybeIntOrNothing "none" "limit-on-first-seconds:"
       bundleInitialCost <- p_moneyParamOrDefaultValue "set-bundle-initial-cost:" 0.0
       bundleMinCost <- p_moneyParamOrDefaultValue "set-bundle-min-cost:" 0.0
       calcParams <- p_commonCalcParams
       children <- case isExternalRate of
                     True
                       -> do p_symbol "use:"
                             v <- p_referenceNameValue
                             return $ Left v
                     False
                       -> do r <- Parsec.many (p_bundleRatePlan env)
                             return $ Right r

       p_endDef
       p_freeSpaces
       Parsec.many p_comment
       p_freeSpaces

       let params
             = RateParams {
                 rate_userId = id
               , rate_match = matchFun_create filters calcParams
               }

       let bundleParams
             = BundleParams {
                 bundle_initialCost = bundleInitialCost
               , bundle_minCost = bundleMinCost
               , bundle_leftCalls = leftCalls
               , bundle_leftDuration = leftDuration
               , bundle_appliedCost = 0
               }

       let plan
             = BundleRatePlan {
                 bundle_systemId = 0
               , bundle_rateParams = params
               , bundle_bundleParams = bundleParams
               , bundle_children = children
                      }

       return plan


p_intParamOrDefaultValue :: Text -> Int -> Parsec.Parser Int
p_intParamOrDefaultValue name d
  = Parsec.try (do p_symbol name
                   v <- p_integer
                   return v)
    <|> (return d)


p_moneyParamOrDefaultValue :: Text -> MonetaryValue -> Parsec.Parser MonetaryValue
p_moneyParamOrDefaultValue name d
  = Parsec.try (do p_symbol name
                   v <- p_float
                   return v)
    <|> (return d)


p_maybeIntOrNothing :: Text -> Text -> Parsec.Parser (Maybe Int)
p_maybeIntOrNothing none name
  = (Parsec.try (p_maybeInt none name)) <|> (return Nothing)

p_maybeInt :: Text -> Text -> Parsec.Parser (Maybe Int)
p_maybeInt none name
  = do p_symbol name
       v <- (do p_symbol none
                p_nl
                return Nothing
            ) <|> (do i <- p_integer
                      return $ Just i
                  )
       return v

p_beginDef
  = (do p_spaces
        Parsec.char '{'
        p_nl
        return ())

p_endDef
  = (do p_spaces
        Parsec.char '}'
        p_nl
        return ())

p_description :: Parsec.Parser Text
p_description
  = do d <- Parsec.manyTill Parsec.anyChar (Parsec.many1 (Parsec.newline <|> Parsec.char '\r'))
       p_freeSpaces
       let dd1 = List.dropWhile isSpace d
       let dd2 = List.reverse $ List.dropWhile isSpace (List.reverse dd1)
       return $ Text.pack dd2

p_id :: Parsec.Parser Text
p_id
  = do p_symbol "id:"
       p_referenceNameValue

p_filters :: RatingParams -> Bool -> Parsec.Parser FilterFun
p_filters env acceptAlsoPriceCategory
  = do filters <- Parsec.many (p_filter env acceptAlsoPriceCategory)
       return $ filterFun_and filters

p_filter :: RatingParams -> Bool -> Parsec.Parser FilterFun
p_filter env acceptAlsoPriceCategory
  = do p_symbol "match-communication-channel:"
       ids <- p_referenceList "communication channel type" (params_channelTypes env)
       return $ filterFun_matchOneOfIds ids (\cdr -> fromJust1 "m1" $ cdr_communicationChannelTypeId cdr)

    <|> do p_symbol "match-vendor:"
           ids <- p_referenceList "vendor" (params_vendors env)
           return $ filterFun_matchOneOfIds ids (\cdr -> fromJust1 "m2" $ cdr_vendorId cdr)

    <|> case acceptAlsoPriceCategory of
          True
            -> do p_symbol "match-price-category:"
                  ids <- p_referenceList "price category"  (fst $ params_rateCategories env)
                  return $ filterFun_matchOneOfIds ids (\cdr -> fromJust1 "m3" $ cdr_priceCategoryId cdr)
          False
            -> fail "match-price-category is not accepted"

    <|> do p_symbol "match-call-direction:"
           direction <- p_direction
           return $ (\env cdr -> if cdr_direction cdr == direction
                                 then Just matchStrenght_initial
                                 else Nothing
                    )

    <|> do p_symbol "match-telephone-number:"
           telephoneNumbers <- p_telephoneNumbers
           let trie = List.foldl' (\t c -> trie_addExtensionCode t c ()) trie_empty (List.map Text.unpack telephoneNumbers)
           return $ (\env cdr
                       -> let cdrTelephoneNumber = fromJust1 "m4" $ cdr_externalTelephoneNumberWithAppliedPortability cdr
                              maybeResult = trie_getMatch trie_getMatch_initial trie (Text.unpack cdrTelephoneNumber)
                          in  case maybeResult of
                                Nothing
                                  -> Nothing
                                Just ((m, _), _)
                                  -> Just (MatchStrenght { matchStrenght_telephoneNumber = m })
                    )

p_referenceList :: Text -> Map.Map Text Int -> Parsec.Parser [Int]
p_referenceList errorSubject dictionay
    = do v <- Parsec.sepBy1 (p_reference errorSubject dictionay) (Parsec.char ',')
         p_nl
         Parsec.many p_comment
         p_freeSpaces
         return v

p_reference :: Text -> Map.Map Text Int -> Parsec.Parser Int
p_reference errorSubject dictionary
    = do n <- p_referenceName
         case Map.lookup n dictionary of
           Just i -> return i
           Nothing -> fail $ "unknown " ++ Text.unpack errorSubject ++ " \"" ++ Text.unpack n ++ "\". Expected values are: " ++ Text.unpack expectedValues
 where

  expectedValues
    = Text.concat $ List.intersperse "," (Map.keys dictionary)

p_commonCalcParams :: Parsec.Parser CalcParams
p_commonCalcParams
  = do params <- return calcParams_empty

       params <- (do r <- integerValue "set-free-seconds:"
                     return $ params { calcParams_freeSecondsAfterCostOnCall = r }
                 ) <|> return params

       params <- (do r <- maybeIntegerValue "set-duration-discrete-increments:"
                     return $ params { calcParams_durationDiscreteIncrements = r }
                 ) <|> return params

       params <- (do r <- integerValue "set-at-least-seconds:"
                     return $ params { calcParams_atLeastXSeconds = r }
                 ) <|> return params

       params <- (do r <- monetaryValueOrImported "set-cost-on-call:"
                     return $ params { calcParams_costOnCall = r }
                 ) <|> return params

       params <- (do r <- monetaryValue "set-cost-for-minute:"
                     return $ params { calcParams_costForMinute = r }
                 ) <|> return params

       params <- (do r <- maybeMonetaryValue "set-max-cost-of-call:"
                     return $ params { calcParams_maxCostOfCall = r }
                 ) <|> return params

       params <- (do r <- maybeMonetaryValue "set-min-cost-of-call:"
                     return $ params { calcParams_minCostOfCall = r }
                 ) <|> return params

       params <- (do r <- maybeIntegerValue "set-round-to-decimal-digits:"
                     return $ params { calcParams_roundToDecimalDigits = r }
                 ) <|> return params

       params <- (do r <- maybeIntegerValue "set-ceil-to-decimal-digits:"
                     return $ params { calcParams_ceilToDecimalDigits = r }
                 ) <|> return params

       params <- (do r <- maybeIntegerValue "set-floor-to-decimal-digits:"
                     return $ params { calcParams_floorToDecimalDigits = r }
                 ) <|> return params

       return params

 where

  integerValue name
    = do p_symbol name
         p_thisOrParentOrValue p_integer

  floatValue name = monetaryValue name

  monetaryValueOrImported name
    = do p_symbol name
         p_thisOrParentOrValue ((do p_symbol "imported"
                                    return $ RateCost_imported)
                                <|> (do p_symbol "expected"
                                        return $ RateCost_expected)
                                <|> (do v <- p_float
                                        return $ RateCost_cost v))

  monetaryValue name
    = do p_symbol name
         p_thisOrParentOrValue p_float

  referenceValue name
    = do p_symbol name
         p_thisOrParentOrValue p_referenceNameValue

  maybeMonetaryValue name
    = do r <- monetaryValue name
         return $ toMaybeValue r

  maybeIntegerValue name
    = do r <- integerValue name
         return $ toMaybeValue r

  toMaybeValue mv
    = case mv of
        Nothing -> Nothing
        Just v -> Just $ Just v

p_direction
  = do p_symbol "incoming"
       p_nl
       return CDR_incoming
    <|> do p_symbol "outgoing"
           p_nl
           return CDR_outgoing
    <|> do p_symbol "internal"
           p_nl
           return CDR_internal
    <|> do p_symbol "system"
           p_nl
           return CDR_system

p_descriptionValue :: Parsec.Parser Text
p_descriptionValue
  = (do p_spaces
        v <- Parsec.manyTill Parsec.anyChar (Parsec.newline <|> Parsec.char '\r')
        Parsec.many p_comment
        p_freeSpaces
        return $ Text.pack v)

p_referenceName :: Parsec.Parser Text
p_referenceName
  = (do p_spaces
        v <- Parsec.many1 validChar
        p_spaces
        return $ Text.pack v)
 where

  validChar = Parsec.alphaNum <|> Parsec.char '_' <|> Parsec.char '-' <|> Parsec.char '/'

p_referenceNameValue
  = (do v <- p_referenceName
        p_nl
        Parsec.many p_comment
        p_freeSpaces
        return v)

p_telephoneNumbers :: Parsec.Parser [Text]
p_telephoneNumbers
  = do line <- p_descriptionValue
       return $ List.map Text.pack $ extensionCodes_extractFromUserSpecification line

p_thisOrParentOrValue valueParser
  = do r <- Parsec.try (do  r <- p_symbol "parent" <|> p_symbol "this"
                            p_nl
                            return Nothing
                       ) <|> (do r <- valueParser
                                 return $ Just r)

       Parsec.many p_comment
       p_freeSpaces
       return r

p_bool
  = (do p_symbol "true"
        p_nl
        Parsec.many p_comment
        p_freeSpaces
        return True
     <|> do p_symbol "false"
            p_nl
            Parsec.many p_comment
            p_freeSpaces
            return False)

p_integer
  = (do -- NOTE: manage in this way for not introducing binary decimal conversion errors
        p_spaces
        integerPart <- Parsec.many1 Parsec.digit
        p_nl
        let r1 :: [(Int, String)] = readDec integerPart
        r <- case r1 of
               [(r2, s2)] -> case List.null s2 of
                               True -> return r2
                               False -> fail ("unrecognized integer number \"" ++ integerPart ++ "\"")
               _    -> fail ("unrecognized number \"" ++ integerPart ++ "\"")
        Parsec.many p_comment
        p_freeSpaces
        return r) <?> "integer"

p_float
  = (do -- NOTE: manage in this way for not introducing binary decimal conversion errors
        p_spaces
        sign <- (Parsec.char '-' <|> Parsec.char '+' <|> return ' ')
        integerPart <- Parsec.many1 Parsec.digit
        decimalPart <- Parsec.option
                         "" (do Parsec.char '.'
                                ds <- Parsec.many1 Parsec.digit
                                return $ "." ++ ds
                            )
        p_nl
        let sn = (if sign == '-' then "-" else "") ++ integerPart ++ decimalPart
        let r1 :: [(MonetaryValue, String)] = readSigned readFloat sn
        r <- case r1 of
               [(r2, [])] -> return r2
               _  -> fail ("unrecognized float number \"" ++ sn ++ "\"")
        Parsec.many p_comment
        p_freeSpaces
        return r) <?> "float number"

p_comment
  = Parsec.try (do p_freeSpaces
                   Parsec.char '#'
                   Parsec.manyTill Parsec.anyChar (Parsec.char '\r' <|> Parsec.char '\n')
                   p_freeSpaces
                   return ()
               )

p_nl
  = (do p_spaces
        Parsec.many (Parsec.newline <|> Parsec.char '\r')
        Parsec.many p_comment
        p_freeSpaces
        return ()) <?> "new line"

p_freeSpaces :: Parsec.Parser ()
p_freeSpaces
  = (do Parsec.skipMany (p_space <|> Parsec.newline <|> Parsec.char '\r')
        return ()) <?> "optional spaces and newlines"

p_space :: Parsec.Parser Char
p_space
  = do (Parsec.char ' ' <|> Parsec.char '\t')
       return ' '

p_spaces :: Parsec.Parser ()
p_spaces
  = do Parsec.skipMany p_space
       return ()

p_symbol :: Text -> Parsec.Parser ()
p_symbol name
  = Parsec.try (do p_spaces
                   Parsec.string (Text.unpack name)
                   (do p_spaces
                       return ()
                    <|> do Parsec.lookAhead p_nl
                           return ())
                   p_spaces
                   return ()
               )

----------------
-- UNIT TESTS --
----------------

-- | Some tests activable using --debug option for seeing the result, but without testing explicitely.
test_interactiveDebug :: IO ()
  = do showParsingResult case1 bundleState_empty
       showParsingResult case2 bundleState_empty
       showParsingResult case3 bundleState_empty
       showParsingResult case3 bundleState1
       showParsingResult case3 bundleState2

 where

  -- TODO test if CDRS with empty channels or similar ar rated, or a correct error is signaled

  env :: RatingParams
  env = (ratingParamsForTest 4) {
    params_rateCategories = rateCategories_create $ Map.fromList [("price-A", 1), ("price-B", 2), ("price-C", 3)]
  , params_channelTypes = Map.fromList [("local", 1), ("sip", 2), ("ch1", 3), ("ch2", 4)]
  , params_vendors = Map.fromList [("vendor-A", 1), ("vendor-B", 2), ("vendor1", 3)]
  }


  bundleState1
    = IMap.fromList [(100, (fromJust1 "m20" $ fromMySQLDateTimeToLocalTime "2014-01-01", "/bundle-demo-1", IMap.empty))]


  bundleState2
    = IMap.fromList [(100, (fromJust1 "m21" $ fromMySQLDateTimeToLocalTime "2014-01-01", "/bundle-demo-1", IMap.empty)),
                     (101, (fromJust1 "m22" $ fromMySQLDateTimeToLocalTime "2014-01-01", "/bundle-demo-2", IMap.empty))
                    ]

  showParsingResult rateContent bundleState
    = do putStrLn ""
         putStrLn rateContent
         let mainRate :: MainRatePlan
               = case Parsec.runP (mainRatePlanParser_parse env) () "" (Text.pack rateContent) of
                   Left err
                     -> error $ show err
                   Right r
                     -> r

         putStrLn $ show mainRate
         putStrLn ""
         putStrLn "after mainRatePlan_assignSharedUniqueSystemId"
         putStrLn $ show $ mainRatePlan_assignSharedUniqueSystemId mainRate bundleState
         putStrLn ""

  name1 = "Parsing of main rate plans"

  case1 = [here|
# Test comments

rate {
  id: start

  rate {
    id: rate-1

    # another comment

    match-communication-channel: local, sip
    match-vendor: vendor-A, vendor-B
    match-price-category: price-A, price-B
    match-call-direction: outgoing

    set-free-seconds: 50
    set-duration-discrete-increments: 1
    set-at-least-seconds: 10

    set-cost-on-call: 1.5
    set-cost-for-minute: 2.5
    set-max-cost-of-call: 5.50
    set-min-cost-of-call: 2.0
    set-round-to-decimal-digits: 4

    rate {
      id: child-rate

      match-price-category: price-C

      set-cost-on-call: -5.0
      # test negative numbers

      set-max-cost-of-call: 6
    }
  }
}
|]

  case3 = [here|
bundle-rate {
  id: bundle-demo-1

  service-cdr-description: 10 free weekly calls

  schedule: weekly
  schedule-from: Monday
  apply-for-each: price-A

  limit-on-first-calls: 10
  limit-on-first-seconds: none
  limits-are-proportionals-to-activation-date: true
  calls-can-be-split: false
  only-for-calls-with-a-cost: true

  set-bundle-initial-cost: 10
  set-bundle-min-cost: 0

  match-call-direction: outgoing

  rate {
    id: free
    set-cost-on-call: 0
  }
}

rate {
  id: normal

  match-call-direction: outgoing
  set-cost-on-call: 0.1

  external-rate {
    id: csv-1
      use: csv-1

      set-cost-on-call: parent
      set-cost-for-minute: this
  }
}

rate {
  id: internal
  match-call-direction: internal
  set-cost-on-call: 0
}

rate {
  id: incoming
  match-call-direction: incoming
  set-cost-on-call: 0
}

|]

  case2 = [here|
rate {
  id: main

  rate {
    id: default-outgoing

    match-call-direction: outgoing

    set-cost-on-call: 0.1

    external-rate {
      id: csv-1
      use: csv-1

      set-cost-on-call: parent
      set-cost-for-minute: this
    }
  }

  rate {
    id: free-incoming

    match-call-direction: incoming
  }

  rate {
    id: free-internal
    match-call-direction: internal
  }
}
|]

test_rateSpecificationSyntax :: String -> Parsec.Parser a -> String -> HUnit.Assertion
test_rateSpecificationSyntax testName p source
  = HUnit.assertEqual testName parseOk  (normalize $ Parsec.runP p () testName (Text.pack source))
 where

  parseOk = "test parsing ok"

  normalize (Right _) = parseOk
  normalize (Left err) = show err

test_parser :: (Show a, Eq a) => String -> Parsec.Parser a -> String -> a -> HUnit.Assertion
test_parser testName p source expectedResult
  = HUnit.assertEqual testName (Just expectedResult) (normalize $ Parsec.runP p () testName (Text.pack source))
 where

  normalize (Right r) = Just r
  normalize (Left err) = Nothing

tt_rateSpecificationsTests
  = [HUnit.TestCase $ test_rateSpecificationSyntax name1 (mainRatePlanParser_parse env) case1
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test 2" (p_filters env True) "match-communication-channel: ch1, ch2\nmatch-vendor: vendor1\n"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test 2" (p_commonCalcParams) "cost-on-call: 1.5\nset-max-cost-of-call:-5.53\n"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test on 10" p_integer "10"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test on 1.5" p_float "-1.5"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test p_telephoneNumbers syntax 1" p_telephoneNumbers  "059\n"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test p_telephoneNumbers syntax 2" p_telephoneNumbers  "059,051\n"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test p_telephoneNumbers syntax 3" p_telephoneNumbers  "059*\n"
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 1" p_telephoneNumbers  "059\n" ["059"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 2" p_telephoneNumbers  "059*\n" ["059*"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 3" p_telephoneNumbers  "059,051\n" ["059", "051"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 4" p_telephoneNumbers  " 059\n" ["059"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 5" p_telephoneNumbers  " 059, 051\n" ["059", "051"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 6" p_telephoneNumbers  "\\ 059\n" [" 059"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 7" p_telephoneNumbers  "\\ 059,\\ 051\\ \n" [" 059"," 051 "]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 8" p_telephoneNumbers  "059\\,051,052\n" ["059,051","052"]
    ,HUnit.TestCase $ test_parser "test p_telephoneNumbers 8" p_telephoneNumbers  "\\ 059, \\ 4 5 6\n" [" 059"," 4 5 6"]
     -- TODO support in the correct way quoted chars, maybe merging with the code of othe parts
    ]

 where

  env :: RatingParams
  env = (ratingParamsForTest 4) {
    params_rateCategories = rateCategories_create $ Map.fromList [("price-A", 1), ("price-B", 2), ("price-C", 3)]
  , params_channelTypes = Map.fromList [("local", 1), ("sip", 2), ("ch1", 3), ("ch2", 4)]
  , params_vendors = Map.fromList [("vendor-A", 1), ("vendor-B", 2), ("vendor1", 3)]
  }

  name1 = "Parsing of main rate plans"

  case1 = [here|
# Test comments

rate {
  id: start

  rate {
    id: rate-1

    # another comment

    match-communication-channel: local, sip
    match-vendor: vendor-A, vendor-B
    match-price-category: price-A, price-B
    match-call-direction: outgoing

    set-free-seconds: 50
    set-duration-discrete-increments: 1
    set-at-least-seconds: 10

    set-cost-on-call: 1.5
    set-cost-for-minute: 2.5
    set-max-cost-of-call: 5.50
    set-min-cost-of-call: 2.0
    set-round-to-decimal-digits: 4

    rate {
      id: child-rate

      match-price-category: price-C

      set-cost-on-call: -5.0
      # test negative numbers

      set-max-cost-of-call: 6
    }
  }
}

bundle-rate {
  id: cooptel-1

  service-cdr-type: Canone Fisso
  service-cdr-description: Canone mensile tutto incluso sui primi 120 minuti di chiamate nazionali e i primi 60 minuti di chiamate verso operatori mobile.

  schedule: monthly
  schedule-from: 1
  apply-for-each: price-A

  limit-on-first-calls: none
  limit-on-first-seconds: none

  limits-are-proportionals-to-activation-date: true
  calls-can-be-split: false
  only-for-calls-with-a-cost: true

  set-bundle-initial-cost: 25

  rate {
    id: fixed-line1
    match-communication-channel: local
    match-telephone-number: 39*
    limit-on-first-seconds: 7200
    # 120 minuti
  }

  rate {
    id: fixed-line2
    match-communication-channel: sip
    match-telephone-number: 39*
    limit-on-first-seconds: 3600
    # 60 minuti
  }
}

|]
