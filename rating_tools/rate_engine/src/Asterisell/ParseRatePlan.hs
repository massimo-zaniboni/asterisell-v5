{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Parse the Asterisell rate-plan DSL.
--
module Asterisell.ParseRatePlan(
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
import Asterisell.Holiday

import Numeric

import Data.List as List
import Data.Char
import Data.Either
import Data.Ratio
import Data.Ord as Ord
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap
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
import Control.Monad
import Data.Time.LocalTime

--------------------------------------------
-- Parse a DSL for rating CDRS.
-- The language is specified in the manual.

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
         <- Parsec.many1 $ do
             p_spacesAndComments
             (Right <$> p_ratePlan False env) <|> (Left <$> p_rootBundleRate False env)

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
p_ratePlan :: Bool -> RatingParams -> Parsec.Parser RatePlan
p_ratePlan withComments env
  = do
       case withComments of
         True -> p_spacesAndComments
         False -> return ()

       p_symbol "rate"

       p_beginDef

       id <- p_id

       filters <- p_filters env True

       bundleParams
         <- Parsec.option Nothing $ do
              v <- p_bundleParams env
              return $ Just v

       externalRateRef
         <- Parsec.option Nothing $ do
              p_symbol "use:"
              Just <$> p_referenceNameValue
 
       calcParams <- p_commonCalcParams

       children <- Parsec.many (p_ratePlan True env)

       p_endDef
       p_freeSpaces

       elsePart :: [RatePlan]
         <- Parsec.option [] $ do
              p_symbol "else"
              p_beginDef
              elseRates <- Parsec.many1 $ p_ratePlan True env
              p_endDef
              return elseRates

       Parsec.many p_comment
       p_freeSpaces

       let plan
             = RatePlan {
                 rate_userId = id
               , rate_systemId = 0
               , rate_parentSystemId = Nothing
               , rate_matchBeforeUse = matchFun_create filters calcParams
               , rate_use = externalRateRef
               , rate_bundleParams = bundleParams
               , rate_elsePart = elsePart
               , rate_children = children
               }

       return plan

-- | Used for specifying different types of rates.
p_rootBundleRate :: Bool -> RatingParams -> Parsec.Parser RootBundleRatePlan
p_rootBundleRate withSpacesAndComments env
  = do case withSpacesAndComments of
         True -> p_spacesAndComments
         False -> return ()

       p_symbol "bundle"
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

       schedule <- (do 
                       p_symbol "schedule-at:"
                       p_scheduleAt schedule
                   ) <|> (return schedule)

       p_symbol "apply-for-each:"
       priceCategories <- p_referenceList "price category" (fst $ params_rateCategories env)

       p_symbol "limits-are-proportionals-to-activation-date:"
       proportionalLimits <- p_bool

       p_symbol "only-for-calls-with-a-cost:"
       onlyForCallsWithACost <- p_bool

       bundleInitialCost <- p_moneyParamOrDefaultValue "bundle-cost:" 0

       filters <- p_filters env False
       plans <- Parsec.many1 (p_ratePlan True env)  

       p_endDef
       p_freeSpaces
       Parsec.many p_comment
       p_freeSpaces

       let rootPlan
             = RootBundleRatePlan {
                 bundle_userId = id
               , bundle_serviceCDRType = serviceCdrType
               , bundle_serviceCDRDescription = serviceCdrDescription
               , bundle_timeFrame = schedule
               , bundle_priceCategoryIds = priceCategories
               , bundle_limitsAreProportionalsToActivationDate = proportionalLimits
               , bundle_onlyForCallsWithACost = onlyForCallsWithACost
               , bundle_children = plans
               , bundle_initialCost = bundleInitialCost
               }

       return rootPlan

 where

  p_schedule
    = (do p_symbol "monthly"
          p_nl
          return $ MonthlyTimeFrame 0 midnight
      )
      <|> (do p_symbol "weekly"
              p_nl
              return $ WeeklyTimeFrame 0 midnight
          )

  p_scheduleFrom :: BundleRateTimeFrame -> Parsec.Parser BundleRateTimeFrame
  p_scheduleFrom f
    = case f of
        WeeklyTimeFrame _ _
          -> do d <- p_dayOfWeek 1 ["Monday"
                                   ,"Tuesday"
                                   ,"Wednesday"
                                   ,"Thursday"
                                   ,"Friday"
                                   ,"Saturday"
                                   ,"Sunday"]
                return $ WeeklyTimeFrame d midnight
        MonthlyTimeFrame _ _
         -> do i <- p_integer
               case i >= 1 && i <= 28 of
                 True -> return $ MonthlyTimeFrame i midnight
                 False -> fail $ show i ++ " day of month can not be accepted: it must be between 1 and 28."

  p_scheduleAt :: BundleRateTimeFrame -> Parsec.Parser BundleRateTimeFrame
  p_scheduleAt f = do
    hh <- p_integer
    p_symbol ":"
    mm <- p_integer
    p_symbol ":"
    ss <- p_integer
    let td = TimeOfDay hh mm (fromInteger $ toInteger ss)
    let f' = case f of
               WeeklyTimeFrame a1 _ -> WeeklyTimeFrame a1 td
               MonthlyTimeFrame a1 _ -> MonthlyTimeFrame a1 td
               EveryDaysTimeFrame a1 a2 _ -> EveryDaysTimeFrame a1 a2 td
    return f'

  p_dayOfWeek n (v:r)
    = (do p_symbol v
          p_nl
          return n) <|> p_dayOfWeek (n + 1) r

  p_dayOfWeek _ []
    = fail "Unrecognized day of week"

p_bundleParams :: RatingParams -> Parsec.Parser BundleParams
p_bundleParams env = do
  leftCalls <- p_maybeIntOrNothing "none" "limit-on-first-calls:"
  leftDuration <- p_maybeIntOrNothing "none" "limit-on-first-seconds:"
  return $ bundleParams_empty {
               bundle_leftCalls = leftCalls
             , bundle_leftDuration = leftDuration
             }

p_commonCalcParams :: Parsec.Parser CalcParams
p_commonCalcParams
  = do params <- return calcParams_empty

       params <- (do r <- param "set-free-seconds:" p_integer
                     return $ params { calcParams_freeSecondsAfterCostOnCall = r }
                 ) <|> return params

       params <- (do r <- param "set-duration-discrete-increments:" p_integer
                     return $ params { calcParams_durationDiscreteIncrements = r }
                 ) <|> return params

       params <- (do r <- param "set-at-least-seconds:" p_integer
                     return $ params { calcParams_atLeastXSeconds = r }
                 ) <|> return params

       params <- (do r <- param "set-cost-on-call:" p_float
                     return $ params { calcParams_costOnCall = r }
                 ) <|> return params

       params <- (do r <- param "set-cost-for-minute:" p_float
                     return $ params { calcParams_costForMinute = r }
                 ) <|> return params

       params <- (do r <- param "set-max-cost-of-call:" p_float
                     return $ params { calcParams_maxCostOfCall = r }
                 ) <|> return params

       params <- (do r <- param "set-min-cost-of-call:" p_float
                     return $ params { calcParams_minCostOfCall = r }
                 ) <|> return params

       params <- (do r <- param "set-round-to-decimal-digits:" p_integer
                     return $ params { calcParams_roundToDecimalDigits = r }
                 ) <|> return params

       params <- (do r <- param "set-ceil-to-decimal-digits:" p_integer
                     return $ params { calcParams_ceilToDecimalDigits = r }
                 ) <|> return params

       params <- (do r <- param "set-floor-to-decimal-digits:" p_integer
                     return $ params { calcParams_floorToDecimalDigits = r }
                 ) <|> return params

       return params

 where

  param name parser = do 
    p_symbol name 
    p_paramValue parser

----------------------------------
-- Low level parsing 

p_spacesAndComments :: Parsec.Parser ()
p_spacesAndComments = do
  p_freeSpaces
  _ <- Parsec.many p_comment
  p_freeSpaces
  return ()

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

    <|> do p_symbol "match-price-category:"
           when (not acceptAlsoPriceCategory) (fail "match-price-category is not accepted")
           ids <- p_referenceList "price category"  (fst $ params_rateCategories env)
           return $ filterFun_matchOneOfIds ids (\cdr -> fromJust1 "m3" $ cdr_priceCategoryId cdr)

    <|> do p_symbol "match-call-direction:"
           direction <- p_direction
           return $ (\_ cdr -> if cdr_direction cdr == direction
                               then Just matchStrenght_initial
                               else Nothing
                    )

    <|> do p_symbol "match-rating-code:"
           ids <- p_referenceTextL "rating code" (params_ratingCodes env) <?> "rating codes"
           return $ filterFun_matchOneOfIds ids cdr_ratingCode

    <|> do p_symbol "match-peak-code:"
           allowedPeakCodes <- Set.fromList <$> p_referenceTextL "peak code" (params_peakCodes env) <?> "peak codes"
           return $ (\env2 cdr
                       -> let cdrPeakCodes = holidays_match (params_holidays env) (cdr_calldate cdr)
                              commonCodes = Set.intersection allowedPeakCodes cdrPeakCodes
                          in  case Set.null commonCodes of
                                True ->  Nothing
                                False -> Just matchStrenght_initial)
 
    <|> do p_symbol "match-telephone-number:"
           telephoneNumbers <- p_telephoneNumbers
           let trie = List.foldl' (\t c -> trie_insertExtension t c ()) trie_empty (List.map fromTextToByteString telephoneNumbers)
           return $ (\env2 cdr
                       -> let cdrTelephoneNumber = fromJust1 "m4" $ cdr_externalTelephoneNumberWithAppliedPortability cdr
                              maybeResult = trie_match trie (fromTextToByteString cdrTelephoneNumber)
                          in  case maybeResult of
                                Nothing
                                  -> Nothing
                                Just (m, _, _)
                                  -> Just (MatchStrenght { matchStrenght_telephoneNumber = m })
                    )

p_referenceBSS :: Text -> Set.Set BS.ByteString -> Parsec.Parser [BS.ByteString]
p_referenceBSS errorSubject dictionary 
    = do v <- Parsec.sepBy1 (p_referenceBS errorSubject dictionary) (Parsec.char ',')
         p_nl
         _ <- Parsec.many p_comment
         p_freeSpaces
         return v

p_referenceBS :: Text -> Set.Set BS.ByteString -> Parsec.Parser BS.ByteString
p_referenceBS errorSubject dictionary
    = do n <- fromTextToByteString <$> p_referenceName
         case Set.member n dictionary of
           True -> return n
           False -> fail $ "unknown " ++ Text.unpack errorSubject ++ " \"" ++ fromByteStringToString n ++ "\". Expected values are: " ++ expectedValues
 where

  expectedValues
    = List.concat $ List.intersperse "," $ List.map fromByteStringToString (Set.elems dictionary)

p_referenceTextL :: Text -> Set.Set Text.Text -> Parsec.Parser [Text.Text]
p_referenceTextL errorSubject dictionary 
    = do v <- Parsec.sepBy1 (p_referenceText errorSubject dictionary) (Parsec.char ',')
         p_nl
         _ <- Parsec.many p_comment
         p_freeSpaces
         return v

p_referenceText :: Text -> Set.Set Text.Text -> Parsec.Parser Text.Text
p_referenceText errorSubject dictionary
    = do n <- p_referenceName
         case Set.member n dictionary of
           True -> return n
           False -> fail $ "unknown " ++ Text.unpack errorSubject ++ " \"" ++ Text.unpack n ++ "\". Expected values are: " ++ expectedValues
 where

  expectedValues
    = List.concat $ List.intersperse "," $ List.map Text.unpack (Set.elems dictionary)

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
p_descriptionValue = Parsec.try $ do
  p_spaces
  v <- Parsec.manyTill Parsec.anyChar (Parsec.newline <|> Parsec.char '\r')
  Parsec.many p_comment
  p_freeSpaces
  return $ Text.pack v

p_referenceName :: Parsec.Parser Text
p_referenceName = Parsec.try $ do
  p_spaces
  v <- Parsec.many1 validChar
  p_spaces
  return $ Text.pack v
 where

  validChar = Parsec.alphaNum <|> Parsec.char '_' <|> Parsec.char '-' <|> Parsec.char '/'

p_referenceNameValue = do
  v <- p_referenceName
  p_nl
  Parsec.many p_comment
  p_freeSpaces
  return v

p_telephoneNumbers :: Parsec.Parser [Text]
p_telephoneNumbers = do
  line <- p_descriptionValue
  return $ List.map fromByteStringToText $ extensionCodes_extractFromUserSpecification line

p_paramValue valueParser
  = do r <- (do  r <- p_symbol "parent" 
                 p_nl
                 return CP_Parent
            ) <|> (do r <- p_symbol "external" 
                      p_nl
                      return CP_External
            ) <|> (do r <- p_symbol "imported" 
                      p_nl
                      return CP_Imported
            ) <|> (do r <- p_symbol "expected" 
                      p_nl
                      return CP_Expected
            ) <|> (CP_Value <$> valueParser)

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

p_integer = Parsec.try (do 
  -- NOTE: manage in this way for not introducing binary decimal conversion errors
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

p_float = Parsec.try (do
  -- NOTE: manage in this way for not introducing binary decimal conversion errors
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

------------------------------------------
-- Unit tests

-- | Some tests activable using --debug option for seeing the result, but without testing explicitely.
test_interactiveDebug :: IO ()
  = do showParsingResult case1 bundleState_empty
       showParsingResult case2 bundleState_empty
       showParsingResult case3 bundleState_empty
       showParsingResult case3 bundleState1
       showParsingResult case3 bundleState2

 where

  bundleState_empty = (Nothing, IMap.empty)

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
                     -> pError (show err) 
                   Right r
                     -> r

         putStrLn $ show mainRate
         putStrLn ""
         putStrLn "after mainRatePlan_assignUniqueSystemId"
         putStrLn $ show $ mainRatePlan_assignUniqueSystemId mainRate 
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
    match-rating-code: fixed-line

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
bundle {
  id: bundle-demo-1

  service-cdr-description: 10 free weekly calls

  schedule: weekly
  schedule-from: Monday
  apply-for-each: price-A

  limit-on-first-calls: 10
  limit-on-first-seconds: none
  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true

  bundle-cost: 10

  rate {
    id: free
    match-call-direction: outgoing
    set-cost-on-call: 0
  }
}

rate {
  id: normal
  match-call-direction: outgoing

  use: csv-1
  set-cost-on-call: 0.1
  set-cost-for-minute: external
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

    use: csv-1
    set-cost-on-call: 0.1
    set-cost-for-minute: external
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
    ,HUnit.TestCase $ test_rateSpecificationSyntax name1 (mainRatePlanParser_parse env) case2
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test 2.1" (p_filters env True) "match-communication-channel: ch1, ch2\nmatch-vendor: vendor1\n"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test 2.2" (p_commonCalcParams) "cost-on-call: 1.5\nset-max-cost-of-call:-5.53\n"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test 2.3" (p_filters env True) "match-rating-code: fixed-line"
    ,HUnit.TestCase $ test_rateSpecificationSyntax "test 2.4" (p_filters env True) "match-rating-code: fixed-line"
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
    ]

 where

  env :: RatingParams
  env = (ratingParamsForTest 4) {
    params_rateCategories = rateCategories_create $ Map.fromList [("price-A", 1), ("price-B", 2), ("price-C", 3)]
  , params_channelTypes = Map.fromList [("local", 1), ("sip", 2), ("ch1", 3), ("ch2", 4)]
  , params_vendors = Map.fromList [("vendor-A", 1), ("vendor-B", 2), ("vendor1", 3)]
  , params_ratingCodes = Set.fromList ["fixed-line", "mobile-line", "external"]
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
    match-rating-code: fixed-line

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

bundle {
  id: cooptel-1

  service-cdr-type: Canone Fisso
  service-cdr-description: Canone mensile tutto incluso sui primi 120 minuti di chiamate nazionali e i primi 60 minuti di chiamate verso operatori mobile.

  schedule: monthly
  schedule-from: 1
  apply-for-each: price-A

  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true

  bundle-cost: 25

  rate {
      id: fixed-line1
      match-communication-channel: local
      match-telephone-number: 39*
      limit-on-first-seconds: 7200
      # 120 minuti
  }

  rate {
    id: fixed-line
    match-communication-channel: sip
    match-telephone-number: 39*
    limit-on-first-seconds: 3600
    # 60 minuti
  }
}

|]

  case2 = [here|
# Test bundle rate before rate and different orders

bundle {
  id: cooptel-1

  service-cdr-type: Canone Fisso
  service-cdr-description: Canone mensile tutto incluso sui primi 120 minuti di chiamate nazionali e i primi 60 minuti di chiamate verso operatori mobile.

  schedule: monthly
  schedule-from: 1
  apply-for-each: price-A

  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true

  bundle-cost: 25

  rate {
    id: fixed-line1
    match-communication-channel: local
    match-telephone-number: 39*
    limit-on-first-seconds: 7200
    # 120 minuti
  }

  rate {
    id: fixed-line
    match-communication-channel: sip
    match-telephone-number: 39*
    limit-on-first-seconds: 3600
    # 60 minuti
  }
}


rate {
  id: start

  rate {
    id: rate-1

    # another comment

    match-communication-channel: local, sip
    match-vendor: vendor-A, vendor-B
    match-price-category: price-A, price-B
    match-call-direction: outgoing
    match-rating-code: fixed-line

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

bundle {
  id: cooptel-2

  service-cdr-type: Canone Fisso
  service-cdr-description: Canone mensile tutto incluso sui primi 120 minuti di chiamate nazionali e i primi 60 minuti di chiamate verso operatori mobile.

  schedule: monthly
  schedule-from: 1
  apply-for-each: price-A

  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true

  bundle-cost: 25

  rate {
    id: fixed-line1
    match-communication-channel: local
    match-telephone-number: 39*
    limit-on-first-seconds: 7200
    # 120 minuti
  }

  rate {
    id: fixed-line
    match-communication-channel: sip
    match-telephone-number: 39*
    limit-on-first-seconds: 3600
    # 60 minuti
  }
}

rate {
  id: start2

  rate {
    id: rate-1

    # another comment

    match-communication-channel: local, sip
    match-vendor: vendor-A, vendor-B
    match-price-category: price-A, price-B
    match-call-direction: outgoing
    match-rating-code: fixed-line

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

