{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Manage all rates loaded from DB:
--   * data/CSV files
--   * rate-plans
--
--  They can be of different physical and logical formats
--  and they can change with time.
--
--  DEV-NOTE: for some circular references,
--  I can not split this module in distinct modules.
--
module Asterisell.RatePlan (
    RateRole(..)
  , CalcParam(..)
  , CountOfCDRS
  , ConfiguredRatePlanParsers
  , RatePlanParser
  , RatePlanNormalizer
  , FieldSeparator
  , DecimalSeparator
  , RatingParams(..)
  , InitialRatingParams(..)
  , ExtensionsToExport
  , UseHeader
  , MatchStrenght(..)
  , DBRateReferenceName
  , ActivationDate
  , BundleUnitId
  , CallDuration
  , ExternalRateReference
  , CompleteUserRateRefName
  , AssignedService(..)
  , NextScheduledLocalTime
  , RateSystemId
  , rateSystemId_initial
  , rateSystemId_null
  , Service(..)
  , ServicePrice(..)
  , ratingParams_empty
  , serviceParams_load
  , ratePlan_loadRates
  , calc_saveBundleStateImmediatelyAfter
  , mainRatePlan_externalRateReferences
  , rate_showDebug
  , mainRatePlan_assignUniqueSystemId
  , matchStrenght_initial
  , match_ord
  , match_combineWithChild
  , MatchFun
  , matchFun_create
  , matchFun_initial
  , FilterFun
  , filterFun_and
  , filterFun_matchOneOfIds
  , mainRatePlan_getBundleRateStartCallDate
  , mainRatePlan_getBundleRateEndCallDate
  , MainRatePlan(..)
  , CheckedMainRatePlan
  , fromCheckedMainRatePlan
  , mainRatePlan_empty
  , mainRatePlanRef
  , bundleParams_empty
  , bundleParams_add
  , BundleRecord
  , BundleState
  , bundleState_fake
  , bundleRecord_nextScheduledLocalTime
  , RootBundleRatePlan(..)
  , BundleParams(..)
  , RatePlan(..)
  , BundleRateTimeFrame(..)
  , CalcParams(..)
  , calcParams_empty
  , calcParams_defaultValues
  , calcParams_override
  , env_getRate
  , iparams_dbConf
  , params_dbConf
  , debug_showRateSystemIds
  , BundleRateSystemId
  , CallDate
  , RatePlanIdMap
  , UnitIdMap
  , TimeFrame
  , timeFrame_getYearsAndMonth
  , MaybeTimeFrame
  , maybeTimeFrame_larger
  , timeFrame_duration
  , mainRatePlan_exportServiceCDRSTelephonePrefixes
  , timeFrame_fromCallDate
  , timeFrame_fromCallDate1
  , ratingParams_respectCodeContracts
  , timeFrames_allInsideRatingPeriod
  , CalcService(..)
  , ServiceId
  , ServiceIdMap
  , params_peakCodes
  , params_isDebugMode
  , params_isVoipReseller
  , params_digitsToMask
  , params_defaultTelephonePrefixToNotDisplay
  , params_currencyPrecision
  , params_debugFileName
  , params_fromDate
  , params_testToDate
  , params_isRateUnbilledCallsEvent
  , params_dbName
  , params_dbUser
  , params_dbPasswd
  , params_configuredRatePlanParsers
  , params_priceCategoryCode
  , ratingParamsForTest
  , serviceParams_insertAssignment
  , ConflictingUnitIds
  , UnitIdSet
  , NormalizedRate(..)
  , normalizedRate_empty
  , configuredRatePlanNormalizer_get
  , normalizedRate_sameCost
  , NormalizedRateTrie
  , normalizedRateTrie_insert
  , normalizedRates_fromSpecificCosts
  , specificRateFormat
  , normalizedRates_header
  , normalizedRates_toSpecificRateCSV
  , normalizedRates_info
  , NormalizedDiffRate(..)
  , normalizedDiffRate_sameCost
  , normalizedDiffRate_calc
  , normalizedRate_diffHeader
  , normalizedRates_toDiffRate
  , normalizedDiffRates_toCSV
  ) where

import Asterisell.DB
import Asterisell.Cdr
import Asterisell.Error
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.Params
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.Holiday

import Data.List as List
import Data.Maybe
import Control.Monad.State.Strict as State
import Data.Ord as Ord
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntSet as ISet
import Control.Monad as M
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Calendar.WeekDate
import Control.Monad.Except
import qualified Data.Text as Text
import Data.Set as Set
import Data.HashSet as HSet
import Data.IntMap.Strict as IMap
import qualified Data.Serialize as DBS
import qualified System.IO.Streams as S
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
import Numeric (showFFloat)
import Data.Functor (fmap)

-- ------------------
-- Match strenght

-- | The strenght of a Match.
--   NOTE: now I'm comparing only the telephone number, so the code could be simpler,
--   but I mantain this complex code, in case in future I need to compare other parameters.
data MatchStrenght
  = MatchStrenght {

      matchStrenght_telephoneNumber :: Int
      -- ^ 0 for the weaker match
   }
   deriving (Show)

-- | The minimum match strenght.
matchStrenght_initial :: MatchStrenght
matchStrenght_initial
  = MatchStrenght {
      matchStrenght_telephoneNumber = 0
    }

-- | Compare two different strenghts.
--   Return Nothing if they are not comparable.
match_ord :: MatchStrenght -> MatchStrenght -> Maybe Ordering
match_ord x y
  = let x1 = matchStrenght_telephoneNumber x
        y1 = matchStrenght_telephoneNumber y
     in Just $ Ord.compare x1 y1

-- | If there is a match that is a child of a parent match,
--   then the real matching condition is the strongest match possible,
--   because all the matches must hold.
match_combineWithChild :: MatchStrenght -> MatchStrenght -> MatchStrenght
match_combineWithChild x y
  = let x1 = matchStrenght_telephoneNumber x
        y1 = matchStrenght_telephoneNumber y
    in  MatchStrenght {
          matchStrenght_telephoneNumber = max x1 y1
        }

-- | User readable show.
matchStrenght_show :: MatchStrenght -> String
matchStrenght_show m
  = if matchStrenght_telephoneNumber m == 0
    then "simple match"
    else "match " ++ show (matchStrenght_telephoneNumber m) ++ " telephone digits"

-- ---------------------------------------
-- Rating funs

-- | Decide if a rate can be applied to a CDR.
type FilterFun = RatingParams -> CDR -> Maybe MatchStrenght

-- | Combine in "and" the matching functions.
filterFun_and :: [FilterFun] -> FilterFun
filterFun_and funs env cdr
  = process (Just matchStrenght_initial) funs

 where

  process maybeCurrentMatch []
    = maybeCurrentMatch

  process maybeCurrentMatch (f:fs)
    = case f env cdr of
        Nothing
          -> Nothing
             -- if one of the filter does not match, then invalidate also the rest
        Just nextMatch
          -> let bestMatch = case maybeCurrentMatch of
                               Nothing -> Just nextMatch
                               Just currentMatch -> Just $ match_combineWithChild currentMatch nextMatch
             in process bestMatch fs

-- | Generic function for matching at least one of the specific ids, that will be used from other functions.
filterFun_matchOneOfIds :: (Eq a) => [a] -> (CDR -> a) -> FilterFun
filterFun_matchOneOfIds ids funId env cdr
  = if List.elem (funId cdr) ids
    then Just matchStrenght_initial
    else Nothing

-- | Decide if a rate can be applied to a CDR,
--   returning also the corresponding CalcParams.
type MatchFun = RatingParams -> CDR -> Maybe (MatchStrenght, CalcParams)

instance Show MatchFun where
  show f = "Env -> CDR -> Maybe (MatchStrenght, CalcParams)"

matchFun_create :: FilterFun -> CalcParams -> MatchFun
matchFun_create filterFun calcParams env cdr
  = case filterFun env cdr of
      Nothing
        -> Nothing
      Just match
        -> Just (match, calcParams)

matchFun_initial :: MatchFun
matchFun_initial env cdr
  = Just (matchStrenght_initial, calcParams_defaultValues)

-- | The value of a CalcParams.
data CalcParam a = 
        CP_Nothing
      | CP_Parent
        -- ^ a value inherited from parent rate
      | CP_External
        -- ^ a value specified in the external CSV rate   
      | CP_Imported
       -- ^ the value imported in a field of the CDR directly from the source format, so it is a value specified by the vendor
      | CP_Expected
       -- ^ the value imported in a field of the CDR directly from the source format, so it is a value specified by the vendor
      | CP_Value a

type IsExternal = Bool

-- | Override the CalcParam of a parent rate with the params of a child rate.
--   Specify when the child rate is a referenced external rate, because the semantic is different.
calcParam_override :: CalcParam a -> IsExternal -> CalcParam a -> CalcParam a
calcParam_override CP_Nothing _ CP_Nothing = CP_Nothing
calcParam_override CP_Nothing _ y = y
calcParam_override x _ (CP_Nothing) = x
calcParam_override x _ (CP_Parent) = x
calcParam_override CP_External _ y = y
calcParam_override (CP_Value x) True (CP_Value y) = (CP_Value x) 
calcParam_override CP_Imported True (CP_Value y) = (CP_Imported)
calcParam_override CP_Expected True (CP_Value y) = (CP_Expected) 
calcParam_override x True y = x
calcParam_override x False y = y

-- | The params used for the default calc of CDRs.
--   The first Maybe level says if a param must override the parent param,
--   the next Maybe level (if present) represent the value of the param.
data CalcParams
  = CalcParams {

      calcParams_costForMinute :: CalcParam MonetaryValue
      -- ^ the cost for minute.
      -- NOTE: the call can be rated also by second,
      -- but cost are showed by minute because otherwise
      -- prices are too high

    , calcParams_costOnCall :: CalcParam MonetaryValue
      -- ^ the initial cost of the call

    , calcParams_atLeastXSeconds :: CalcParam Int
       -- ^ The minimum billable duration (in seconds)
       -- of a call. Call shorter than this duration will
       -- be billed at least for this minimum duration.

    , calcParams_durationDiscreteIncrements :: CalcParam Int
      -- ^ rate every X seconds. A 0 duration call is rated
      --   the specified discrete increment,
      --   and so on.

    , calcParams_freeSecondsAfterCostOnCall :: CalcParam Int
      -- ^ after the applying of the cost on call,
      --   do not consider in the cost the next specified seconds

    , calcParams_ceilToDecimalDigits :: CalcParam Int
      -- ^ ceil the cost of the call to the specified digits.
      --   If left unspecified, use the maximum possible precision.

    , calcParams_floorToDecimalDigits :: CalcParam Int
      -- ^ floor the cost of the call to the specified digits.
      --   If left unspecified, use the maximum possible precision.

    , calcParams_roundToDecimalDigits :: CalcParam Int
      -- ^ round the cost of the call to the specified digits.
      --   If left unspecified, use the maximum possible precision.

    , calcParams_maxCostOfCall :: CalcParam MonetaryValue
      -- ^ after calculating the cost of the call,
      --   limit it to this maximum cost

    , calcParams_minCostOfCall :: CalcParam MonetaryValue
      -- ^ after calculating the cost of the call
      --   set it to this minimum value

    , calcParams_customCalc :: CalcParam (CDR -> MonetaryValue -> MonetaryValue)
      -- ^ calculate the cost using some custom function. Apply after the calculation of other params, and the calculated value is passed to the function.
    }

-- | The params not overriding the parent params.
--
calcParams_empty :: CalcParams
calcParams_empty
  = CalcParams {
      calcParams_costForMinute = CP_Nothing
    , calcParams_costOnCall = CP_Nothing
    , calcParams_atLeastXSeconds = CP_Nothing
    , calcParams_durationDiscreteIncrements = CP_Nothing
    , calcParams_freeSecondsAfterCostOnCall = CP_Nothing
    , calcParams_ceilToDecimalDigits = CP_Nothing
    , calcParams_floorToDecimalDigits = CP_Nothing
    , calcParams_roundToDecimalDigits = CP_Nothing
    , calcParams_maxCostOfCall = CP_Nothing
    , calcParams_minCostOfCall = CP_Nothing
    , calcParams_customCalc = CP_Nothing
    }

-- | The default initial values for a calcParams.
--
calcParams_defaultValues :: CalcParams
calcParams_defaultValues = calcParams_empty 

-- | Add new specified params to the parent params, overriding them.
--   Override the first passed params, adding the second passed params to them,
--   considering the first param as the parent rate, and the second param as the child rate.
--   
calcParams_override :: CalcParams -> IsExternal -> CalcParams -> CalcParams
calcParams_override x isExternal y
  = CalcParams {
      calcParams_costForMinute = ex calcParams_costForMinute x y
    , calcParams_costOnCall = ex calcParams_costOnCall x y
    , calcParams_atLeastXSeconds = ex calcParams_atLeastXSeconds x y
    , calcParams_durationDiscreteIncrements = ex calcParams_durationDiscreteIncrements x y
    , calcParams_freeSecondsAfterCostOnCall = ex calcParams_freeSecondsAfterCostOnCall x y
    , calcParams_ceilToDecimalDigits = ex calcParams_ceilToDecimalDigits x y
    , calcParams_floorToDecimalDigits = ex calcParams_floorToDecimalDigits x y
    , calcParams_roundToDecimalDigits = ex calcParams_roundToDecimalDigits x y
    , calcParams_maxCostOfCall = ex calcParams_maxCostOfCall x y
    , calcParams_minCostOfCall = ex calcParams_minCostOfCall x y
    , calcParams_customCalc = ex calcParams_customCalc x y
    }

 where

  ex :: (CalcParams -> CalcParam a) -> CalcParams -> CalcParams -> CalcParam a
  ex f x y = calcParam_override (f x) isExternal (f y)
  {-# INLINE ex #-}

-- | User readable description of calc params.
calcParams_show :: CalcParams -> String
calcParams_show p
  = if msg1 == "" then "free call" else msg1 
 where

  scp :: (a -> String) -> String -> CalcParam a -> String -> String
  scp _ _ CP_Nothing _ = ""
  scp _ s1 CP_Parent s2 = s1 ++ "as set in the parent rate " ++ s2 
  scp _ s1 CP_External s2 = s1 ++ "as set in the referenced external rate " ++ s2 
  scp _ s1 CP_Imported s2 = s1 ++ "as imported from the CDR (a value of the vendor) " ++ s2 
  scp _ s1 CP_Expected s2 = s1 ++ "as expected from the CDR (a value of the vendor) " ++ s2
  scp s s1 (CP_Value a) s2 = s1 ++ s a ++ " " ++ s2
  
  scpp s s1 f s2 = scp s s1 (f p) s2 

  msg1 = 
    concat [
        scpp show "do not consider the first " calcParams_freeSecondsAfterCostOnCall " seconds, "
      , scpp show "consider call duration increments of " calcParams_durationDiscreteIncrements " seconds (e.g. with a value of 5, calls from 0s-4s are billed as 5s, calls from 5s to 9s are billed as 10s, and so on)," 
      , scpp show "bill at least " calcParams_atLeastXSeconds " seconds, "
      , scpp monetaryValue_show "initial cost of the call is " calcParams_costOnCall ", "
      , scpp monetaryValue_show "cost by minute is " calcParams_costForMinute ", "
      , scpp monetaryValue_show "the maximum cost will be in any case " calcParams_maxCostOfCall ", "
      , scpp monetaryValue_show "the minimum cost will be in any case " calcParams_minCostOfCall ", "
      , scpp show "the cost will be rounded to " calcParams_roundToDecimalDigits " decimal digits, "
      , scpp show "ceil on " calcParams_ceilToDecimalDigits " decimal digits, "
      , scpp show "floor on " calcParams_floorToDecimalDigits " decimal digits, "
      ]

instance Show CalcParams where
  show = calcParams_show

-- -----------------------------------------------
-- A rate loaded from DB.
-- It can be a CSV file, or a main-rate-plan.

data RateRole
  = IncomeRate
  | CostRate
 deriving(Ord, Eq)

instance Show RateRole where
  show IncomeRate = "income"
  show CostRate = "cost"

mainRatePlanRef :: RateRole -> DBRateReferenceName
mainRatePlanRef IncomeRate = "main-income-rate"
mainRatePlanRef CostRate = "main-cost-rate"
{-# INLINE mainRatePlanRef #-}

-- | A rate loaded from the DB, and valid until a certain time-frame.
--   This is a common interfaces for managing both RatePlan, and CSV data.
--   DEV NOTE: "MatchFun" is the trick used for allowing extensions to every type of rate plan/method.
--   For example a CSV based rate, store rows in an ad-hoc data structure, read from the MatchFun method.
data MainRatePlan
  = MainRatePlan {
      mainRatePlan_bundleRates :: [RootBundleRatePlan]
    , mainRatePlan_normalRates :: [RatePlan]
    , mainRatePlan_systemIdToRatePlan :: RatePlanIdMap RatePlan
    , mainRatePlan_systemIdToUserIdPath :: RatePlanIdMap Text.Text
    , mainRatePlan_maxSystemId :: RateSystemId
    } deriving (Show, Generic, NFData)

-- | Calculate the calldate from which you can start with a empty BundleState, because it is the init of bundle timeframe.
--   So this calc is based on the logical specification of the bundle, and not on the definitions date inside the DB.
--   There can be different BundleState with different time frame.
mainRatePlan_getBundleRateStartCallDate
  :: MainRatePlan
  -> CallDate
     -- ^ at this date you want start the rating, but you are asking if you must include other previous CDRS, before starting with an empty bundle-state
  -> CallDate
      -- ^ the initial call date where you can start with an empty BundleState.
mainRatePlan_getBundleRateStartCallDate plan rateFromDate
  = let allDates = List.map (\rb -> fst $ timeFrame_fromCallDate rb rateFromDate) (mainRatePlan_bundleRates plan)
    in case List.null allDates of
         True -> rateFromDate
         False -> List.minimum allDates
                  -- NOTE: due to `timeFrame_fromCallDate` behaviour, the time frame returned here contains always `rateFromDate`

-- | Return the calldate from which you can save a BundleState, because it is the end of bundle timeframe.
--   There can be different BundleState with different time frame.
mainRatePlan_getBundleRateEndCallDate
  :: MainRatePlan
  -> CallDate
     -- ^ at this date you want close the rating, but you are asking if you must include next CDRS, before saving a complete bundle-state
  -> CallDate
      -- ^ the initial call date where you can safely close the bundle state

mainRatePlan_getBundleRateEndCallDate plan rateToDate
  = let allDates = List.map (\rb -> snd $ timeFrame_fromCallDate rb rateToDate) (mainRatePlan_bundleRates plan)
    in case List.null allDates of
         True -> rateToDate
         False -> List.maximum allDates
         -- NOTE: it contains always `rateToDate`, due to `timeFrame_fromCallDate` behaviour

-- | A MainRatePlan that was checked during initialization, and for which there can not be errors during processing.
type CheckedMainRatePlan = MainRatePlan

-- | Remove the error part from a CheckedMainRatePlan, considering it impossible.
fromCheckedMainRatePlan :: Either a b -> b
fromCheckedMainRatePlan (Left a) = error "Unexpected error in the code, calling fromCheckedMainRatePlan"
fromCheckedMainRatePlan (Right b) = b

mainRatePlan_empty :: MainRatePlan
mainRatePlan_empty
  = MainRatePlan {
      mainRatePlan_bundleRates = []
    , mainRatePlan_normalRates = []
    , mainRatePlan_systemIdToUserIdPath = IMap.empty
    , mainRatePlan_systemIdToRatePlan = IMap.empty
    , mainRatePlan_maxSystemId = 0
    }

-- ---------------------------------
-- Bundle and normal rates.
-- They are specified using a DSL (see Asterisell manual for details)

-- | The complete name of a rate, joining all the UserRateRefName of the rate hierarchy.
--   Something like "/root/outgoing/normal-price/mobile-line".
type CompleteUserRateRefName = Text.Text

-- | A reference to an internal rate.
type InternalRateReference = Text.Text

-- | A compact and efficient, system generated reference to a part of a RatePlan.
type RateSystemId = Int

rateSystemId_initial :: RateSystemId
rateSystemId_initial = -1
{-# INLINE rateSystemId_initial #-}

rateSystemId_null :: RateSystemId -> Bool
rateSystemId_null r = (r == rateSystemId_initial)
{-# INLINE rateSystemId_null #-}

type NormalRateSystemId = RateSystemId

type BundleRateSystemId = RateSystemId

-- | CallDate greather or equal to this value,
--   are to be considered in the next bundle-rate scheduling timeframe.
type NextScheduledLocalTime = CallDate

-- | A Map associating a rate_systemId with some value.
--   An IntMap is rather fast, because it is specialized for Int.
type RatePlanIdMap a = IMap.IntMap a

ratePlanIdMap_empty :: RatePlanIdMap a
ratePlanIdMap_empty = IMap.empty
{-# INLINE ratePlanIdMap_empty #-}

-- | A Map associating a Organization unitId with some value.
type UnitIdMap a = IMap.IntMap a

-- | A UnitId with a direct assignment to a BundleRate
type BundleUnitId = UnitId

-- | The root of a bundle-rate, with initial params that are the same for every nested bundle-rate.
data RootBundleRatePlan
  = RootBundleRatePlan {
      bundle_userId :: Text.Text
    , bundle_serviceCDRType :: Text.Text
    , bundle_serviceCDRDescription :: Text.Text
    , bundle_timeFrame:: BundleRateTimeFrame
    , bundle_priceCategoryIds :: [PriceCategoryId]
    , bundle_limitsAreProportionalsToActivationDate :: Bool
    , bundle_initialCost :: ! MonetaryValue
    , bundle_onlyForCallsWithACost :: Bool
    -- ^ true for applying the bundle only to calls with a cost. Other calls will not change the bundle limits.
    , bundle_children :: [RatePlan]
  } deriving(Show, Generic, NFData)

-- | RatePlan specification.
data RatePlan
  = RatePlan {
      rate_userId :: Text.Text
    , rate_systemId :: RateSystemId
    , rate_parentSystemId :: Maybe RateSystemId
    , rate_matchBeforeUse :: MatchFun
    , rate_use :: Maybe ExternalRateReference
    , rate_bundleParams :: Maybe BundleParams
    , rate_elsePart :: [RatePlan]
    , rate_children :: [RatePlan]
    } deriving(Show, Generic, NFData)

data BundleParams
  = BundleParams {
      bundle_leftCalls :: !(Maybe Int)
      -- ^ how many calls can be processed from the bundle.
      --   Nothing if this is not a limit.
    , bundle_leftDuration :: !(Maybe Int)
      -- ^ how many seconds can be processed from the bundle.
      --   Nothing if this is not a limit.
    , bundle_appliedCost :: !MonetaryValue
      -- ^ the cost of calls associated to the BundleRate.
  } deriving(Show, Generic, NFData)

bundleParams_empty :: BundleParams
bundleParams_empty
  = BundleParams {
      bundle_leftCalls = Nothing
    , bundle_leftDuration = Nothing
    , bundle_appliedCost = 0
    }

bundleParams_add :: BundleParams -> BundleParams -> BundleParams
bundleParams_add p1 p2 =
   BundleParams {
     bundle_leftCalls = addI (bundle_leftCalls p1) (bundle_leftCalls p2)
   , bundle_leftDuration = addI (bundle_leftDuration p1) (bundle_leftCalls p2)
   , bundle_appliedCost = (bundle_appliedCost p1) + (bundle_appliedCost p2)
   }

  where

    addI :: Maybe Int -> Maybe Int -> Maybe Int
    addI (Just x) (Just y) = Just (x + y)
    addI _ _ = Nothing
    {-# INLINE addI #-}

    addV :: Maybe MonetaryValue -> Maybe MonetaryValue -> Maybe MonetaryValue
    addV (Just x) (Just y) = Just (x + y)
    addV _ _ = Nothing
    {-# INLINE addV #-}

-- MAYBE join RateParams with RatePlan

data BundleRateTimeFrame
  = WeeklyTimeFrame Int TimeOfDay
    -- 1 for Monday, 7 for Sunday
  | MonthlyTimeFrame Int TimeOfDay
  -- ^ start scheduling from the specified day of the month
  | EveryDaysTimeFrame Int CallDate TimeOfDay
  -- ^ schedule every specified days, starting from the calldate
 deriving(Eq, Show, Generic, NFData)

type CountOfCDRS = Int

-- | A timeframe
type TimeFrame
  = (CallDate
     -- ^ starting time frame (inclusive)
    , CallDate
    -- ^ ending time frame (exclusive).
    )

--- | Extract Days, and YearAndMonth from a TimeFrame
timeFrame_getYearsAndMonth :: TimeFrame -> (Set.Set Day, Set.Set YearAndMonth)
timeFrame_getYearsAndMonth tf
  = f (Set.empty, Set.empty) tf
 where

  f :: (Set.Set Day, Set.Set YearAndMonth) -> (CallDate, CallDate) -> (Set.Set Day, Set.Set YearAndMonth)
  f (setD1, setMY1) (d0, d2)
    = let d = localDay d0
          (yyyy, mm, _) = toGregorian d
          setMY2 = Set.insert (yyyy, mm) setMY1
          setD2 = Set.insert d setD1
          d1 = d0 { localDay = addDays 1 (localDay d0) }
          -- NOTE: it is adding a day step by step. For normal intervals (few years) this is fast enough.
      in  if d0 > d2 then (setD1, setMY1) else f (setD2, setMY2) (d1, d2)

type YearAndMonth = (Integer, Int)

-- | Rate from (inclusive) to (exclusive)
type MaybeTimeFrame = Maybe (LocalTime, LocalTime)

maybeTimeFrame_larger :: MaybeTimeFrame -> MaybeTimeFrame -> MaybeTimeFrame
maybeTimeFrame_larger Nothing Nothing = Nothing
maybeTimeFrame_larger (Just r1) Nothing = (Just r1)
maybeTimeFrame_larger Nothing (Just r2) = (Just r2)
maybeTimeFrame_larger (Just (x1, y1)) (Just (x2, y2)) = Just (min x1 x2, max y1 y2)
{-# INLINABLE maybeTimeFrame_larger #-}

-- | The seconds to bill
type CallDuration = Int

instance DBS.Serialize BundleParams

-- | Add support for binary serialization to LocalTime.
instance DBS.Serialize CallDate where
  put l
    = do DBS.put $ show l

  get
    = do s :: String <- DBS.get
         return $ read s

-- | True for normal cdrs, False for ServiceCDR.
type IsNormalCdr = Bool

debug_showRateSystemIds :: [RatePlan] -> String
debug_showRateSystemIds rs = List.concat $ List.intersperse ", " $ List.map show $ List.map rate_systemId rs
{-# INLINE debug_showRateSystemIds #-}

-- --------------------------------------------
-- Bundle timeframes

-- | Return the number of seconds of time frame duration.
timeFrame_duration :: TimeFrame -> Rational
timeFrame_duration (fromCallDate, toCallDate)
  = let fromUTC = localTimeToUTC utc fromCallDate
        toUTC = localTimeToUTC utc toCallDate
        diff = diffUTCTime toUTC fromUTC
    in toRational $ diff
{-# INLINE timeFrame_duration #-}

-- | Given  Cdr call-date return the timeframe in wich the CDR is contained.
timeFrame_fromCallDate :: RootBundleRatePlan -> CallDate -> TimeFrame
timeFrame_fromCallDate params callDate
  = timeFrame_fromCallDate1 (bundle_timeFrame params) callDate
{-# INLINE timeFrame_fromCallDate #-}

-- | Given  Cdr call-date return the timeframe in wich the CDR is contained.
timeFrame_fromCallDate1 :: BundleRateTimeFrame -> CallDate -> TimeFrame
timeFrame_fromCallDate1 timeFrame callDate
  = case timeFrame of
      EveryDaysTimeFrame deltaDays d0 _
        -> pError "Bundles using days as time-frame are implemented, but not yet tested. Contact the assistance for complete activation and support."
           {-
           -- DEV NOTE: this code/timeframe up to date not tested and not supported.
           let deltaSeconds :: NominalDiffTime = fromInteger $ (toInteger deltaDays) * 24 * 60 * 60
               dd0 = localTimeToUTC utc d0
               dd1 = localTimeToUTC utc callDate
               totalDiff = diffUTCTime dd1 dd0
               totalDays = totalDiff / deltaSeconds

               lapsedCompleteTimeFrame = totalDays / (fromInteger $ toInteger deltaDays)
               startingCallDateOfCurrentTimeFrame = addUTCTime (lapsedCompleteTimeFrame * deltaSeconds) dd0
               endingCallDateOfCurrentTimeFrame = addUTCTime deltaSeconds startingCallDateOfCurrentTimeFrame
           in (utcToLocalTime utc startingCallDateOfCurrentTimeFrame, utcToLocalTime utc endingCallDateOfCurrentTimeFrame)
           -}

      MonthlyTimeFrame dayOfMonth limitTD
        -> let (yyyy, mm, dd) = toGregorian $ localDay callDate

               (newYYYY, newMM) = if mm < 12 then (yyyy, mm + 1) else (yyyy + 1, 1)
               (oldYYYY, oldMM) = if mm > 1 then (yyyy, mm - 1) else (yyyy - 1, 12)

               newDate = toCallDate (fromGregorian newYYYY newMM dayOfMonth) limitTD
               oldDate = toCallDate (fromGregorian oldYYYY oldMM dayOfMonth) limitTD
               currDate = toCallDate (fromGregorian yyyy mm dayOfMonth) limitTD

           in if callDate < currDate
              then (oldDate, currDate)
              else (currDate, newDate)

      WeeklyTimeFrame dayOfWeek limitTD
        -> let (_, _, cdrDayOfWeek) = toWeekDate (localDay callDate)

               diffDays = dayOfWeek - cdrDayOfWeek
               -- from cdr date to the day of week when start the new timeframe (it can be negative or positive)

               startDate = addDays (toInteger diffDays) (localDay callDate)

               prevDate =  toCallDate (addDays (-7) startDate) limitTD
               nextDate =  toCallDate (addDays 7 startDate) limitTD
               currDate =  toCallDate startDate limitTD

           in if callDate < currDate
              then (prevDate, currDate)
              else (currDate, nextDate)

 where

  toCallDate :: Day -> TimeOfDay -> CallDate
  toCallDate d td = LocalTime {
                   localDay = d
                 , localTimeOfDay = td
                 }
  {-# INLINE toCallDate #-}


timeFrames_allInsideRatingPeriod
  :: BundleRateTimeFrame
  -- ^ the schudule to use
  -> CallDate
  -- ^ rate from this date (comprehensive)
  -> CallDate
  -- ^ rate to this date (exclusive)
  -> [TimeFrame]
  -- ^ all the timeframes starting inside the rating period

timeFrames_allInsideRatingPeriod serviceSchedule rateFromDate rateToDate
  = List.takeWhile isValidTimeFrame $ List.dropWhile isBeforeTimeFrame $ List.iterate nextTimeFrame $ timeFrame_fromCallDate1 serviceSchedule rateFromDate

 where

   nextTimeFrame :: TimeFrame -> TimeFrame
   nextTimeFrame (start1, end1)
     = timeFrame_fromCallDate1 serviceSchedule end1

   {-# INLINE isBeforeTimeFrame #-}
   isBeforeTimeFrame (serviceFromCallDate, serviceToCallDate)
     = serviceFromCallDate < rateFromDate

   {-# INLINE isValidTimeFrame #-}
   isValidTimeFrame (serviceFromCallDate, serviceToCallDate)
     = serviceFromCallDate >= rateFromDate && serviceFromCallDate < rateToDate

-- ---------------------------------------
-- Normalize rate-plans
--
-- DEV-NOTE: usually rate-plans are very short (from a computer point of view),
-- so it is feasible scanning them multiple times: code is clear, but speed only a fraction of second slower.

-- | Assign a unique RateSystemId to all rates, and complete also other derived info.
--   NOTE: this code at every execution had to assign the same ID to the same bundle state rate parts,
--   in case the main-rate-plan is not changed. In case of changes of the rate-plan, the bundle-state
--   is regenerated from scratch and so this constraint does not hold.
mainRatePlan_assignUniqueSystemId
  :: MainRatePlan
  -> Either String MainRatePlan

mainRatePlan_assignUniqueSystemId mainPlan1
  = let (mainPlan2, (maxId, mapToRatePlan)) = runState (processMainRatePlan mainPlan1) (1, IMap.empty)
        mainPlan3 = mainPlan2 { mainRatePlan_maxSystemId = maxId, mainRatePlan_systemIdToRatePlan = mapToRatePlan }
    in  mainRatePlan_completeDerivedInfo mainPlan3

 where

  nextId :: State (Int, RatePlanIdMap RatePlan) Int
  nextId = do
    (i, m) <- get
    put (i + 1, m)
    return i

  saveRatePlan :: RatePlan -> State (Int, RatePlanIdMap RatePlan) RatePlan
  saveRatePlan p = do
    (i, m) <- get
    put (i, IMap.insert (rate_systemId p) p m)
    return p

  processMainRatePlan :: MainRatePlan -> State (Int, RatePlanIdMap RatePlan) MainRatePlan
  processMainRatePlan p
    = do br <- mapM processRootBundleRatePlan (mainRatePlan_bundleRates p)
         nr <- mapM (processRatePlan Nothing) (mainRatePlan_normalRates p)
         return $ mainRatePlan_empty {
                    mainRatePlan_bundleRates = br
                  , mainRatePlan_normalRates = nr
                  }

  processRootBundleRatePlan :: RootBundleRatePlan -> State (Int, RatePlanIdMap RatePlan) RootBundleRatePlan
  processRootBundleRatePlan p
    = do chs <- mapM (processRatePlan Nothing) (bundle_children p)
         return $ p { bundle_children = chs }

  processRatePlan :: Maybe RateSystemId -> RatePlan -> State (Int, RatePlanIdMap RatePlan) RatePlan
  processRatePlan parentId p
    = do i <- nextId
         chs <- mapM (processRatePlan (Just i)) (rate_children p)
         elsePart <- mapM (processRatePlan (Just i)) (rate_elsePart p)
         saveRatePlan $ p { rate_systemId = i
                          , rate_parentSystemId = parentId
                          , rate_children = chs
                          , rate_elsePart = elsePart
                          }

mainRatePlan_completeDerivedInfo
  :: MainRatePlan
     -- ^ @require mainRatePlan_assignUniqueSystemId is called, and it have unique system id.
  -> Either String MainRatePlan
mainRatePlan_completeDerivedInfo mainPlan1 = do
     m2 <- mainRatePlan_mapSystemIdToUserIdPath mainPlan1
     return $ mainPlan1 { mainRatePlan_systemIdToUserIdPath = m2 }

-- | Associate a RateSystemId to something like "root/normal/rate-1" name, composed of the concatenation of rate_userId.
mainRatePlan_mapSystemIdToUserIdPath :: MainRatePlan -> Either String (RatePlanIdMap CompleteUserRateRefName)
mainRatePlan_mapSystemIdToUserIdPath mainPlan
  = case execStateT (processMainRatePlan (Text.pack "") mainPlan) (IMap.empty, Set.empty) of
      Left err
        -> Left err
      Right (m, _)
        -> Right m
 where

  processMainRatePlan n p
    = do mapM_ (processRootBundleRatePlan n) (mainRatePlan_bundleRates p)
         mapM_ (processRatePlan n) (mainRatePlan_normalRates p)

  processRootBundleRatePlan n p = do
    let name = Text.concat [n, "/", bundle_userId p]
    mapM_ (processRatePlan name) (bundle_children p)

  processRatePlan n p
    = do let name = Text.concat [n, "/", rate_userId p]
         insertUniqueValue (rate_systemId p) name
         mapM_ (processRatePlan name) $ rate_children p
         mapM_ (processRatePlan n) $ rate_elsePart p
         -- NOTE: use the path of the parent

  insertUniqueValue rateSystemId completeName
    = do (map1, set1) <- get

         let set2 = Set.insert completeName set1
         when (Set.size set1 == Set.size set2)
              (throwError $ "The rate complete name \"" ++ Text.unpack completeName ++ "\" is used in two rates, and it is not unique. Fix the rate specification, using unique complete names for rates.")

         case IMap.insertLookupWithKey (\k o v -> v) rateSystemId completeName map1 of
           (Nothing, map2)
             -> put (map2, set2)
           (Just _, _)
             -> throwError $ "There is an error in the application code, during processing of rate name \"" ++ Text.unpack completeName ++ "\", with id " ++ show rateSystemId ++ ". It should be unique, but it is not. Contact the assistance."

-- ----------------------------------------------------
-- A BundleState that can be efficiently saved on disk

type ActivationDate = CallDate

-- | Keep track of the current limits assigned to a UnitId, insidle a BundleRate timeframe.
type BundleRecord
       = UnitIdMap ( ActivationDate
                     -- ^ when the unitId is assigned to the priceCategory of the bundle
                     , NextScheduledLocalTime
                     -- ^ when this bundle will terminate
                     , Rational
                     -- ^ 1.0 for full application of the bundle cost
                     , RatePlanIdMap BundleParams
                     -- ^ the bundleLimits left
                   )

type BundleState
       = (Maybe NextScheduledLocalTime
          -- ^ the (cached) minimum NextScheduledLocalTime of all bundle rates.
          --   If a CDR has a calldate greather than this, then serviceCDRS had to be generated,
          --   and some bundle state reset again.
         , BundleRecord
         )

bundleState_empty :: NextScheduledLocalTime -> BundleState
bundleState_empty d = (Just d, IMap.empty)
{-# INLINE bundleState_empty #-}

-- | Used for activating the real BundleState, after the specified calldate.
bundleState_fake :: NextScheduledLocalTime -> BundleState
bundleState_fake callDate = (Just callDate, IMap.empty)
{-# INLINE bundleState_fake #-}

bundleRecord_nextScheduledLocalTime :: BundleRecord -> Maybe NextScheduledLocalTime
bundleRecord_nextScheduledLocalTime br
  = case IMap.null br of
      True -> Nothing
      False -> Just $ List.minimum $ IMap.map (\(_, x, _, _) -> x) br

-- -------------------------------
-- Rating Params

-- | Rating params that are "externally" specified, and that can can not be derived
--   reading the DB content.
--   Usually these params are specified from the command line.
data InitialRatingParams
  = InitialRatingParams {
        iparams_isDebugMode :: Bool
      , iparams_isVoipReseller :: Bool
      , iparams_digitsToMask :: Int
        -- ^ mask the last digits of an external telephone number, for privacy reasons
      , iparams_defaultTelephonePrefixToNotDisplay :: Maybe Text.Text
        -- ^ telephone numbers starting with these prefix are shortened, not including it,
        -- because it is an implicit prefix.
      , iparams_organizationToIgnore :: Maybe Text.Text
      , iparams_currencyPrecision :: Int
      , iparams_debugFileName :: Maybe FilePath
      , iparams_fromDate :: CallDate
      , iparams_testToDate :: Maybe CallDate
        -- ^ a date on which limit ar_source_cdr, for unit testing reasons
      , iparams_isRateUnbilledCallsEvent :: Bool
        -- ^ True if the rating operation is associated to a rerate all unbilled calls
      , iparams_generateExportedCDRInfo :: Bool
        -- ^ True for adding additional info to the ar_cdr table
      , iparams_dbName :: String
      , iparams_dbUser :: String
      , iparams_dbPasswd :: String
      , iparams_configuredRatePlanParsers :: ConfiguredRatePlanParsers
    }
 deriving (Show, Generic, NFData)

iparams_dbConf :: InitialRatingParams -> DBConf
iparams_dbConf p
  =  DBConf {
       dbConf_user = fromStringToByteString $ iparams_dbUser p
     , dbConf_password = fromStringToByteString $ iparams_dbPasswd p
     , dbConf_dbName = fromStringToByteString $ iparams_dbName p
     }

params_dbConf :: RatingParams -> DBConf
params_dbConf p = iparams_dbConf $ params_initial p
{-# INLINE params_dbConf #-}

-- | Extensions to export to an external database.
type ExtensionsToExport = Trie ()

-- | A set of UnitIds
type UnitIdSet = ISet.IntSet

type ConflictingUnitIds = UnitIdSet

-- | Initial and derived rating params.
--   These params are loaded from the DB, but after the rating process starts,
--   they are immutable and read-only.
data RatingParams
  = RatingParams {
      params_initial :: InitialRatingParams
    , params_isShorterSafeTimeFrame :: Maybe CallDate
      -- ^ Nothing if all CDRS can be rated.
      --   A calldate where stop rating, before the original requested timeframe,
      --   because in the original time-frame there were changes in main income or cost rates.
    , params_saveBundleStateImmediatelyAfter :: CallDate
      -- ^ save the bundle for the first CDR after this date.
      --   This is a date a little before the last imported CDR,
      --   in order to not invalidate the bundle, in case there are pending calls,
      --   not yet imported.
    , params_lastSavedBundleState :: BundleState
      -- ^ in case it starts with an empty BundleState, the initial serviceCDRS
    , params_unbilledCallsFrom :: CallDate
    , params_rateChanges :: DBRateChanges
    , params_rates :: CachedDBRates
    , params_extensionsToExport :: ExtensionsToExport
    , params_rateCategories :: RateCategories
    , params_ratingCodes :: RatingCodes
    , params_vendors :: Vendors
    , params_channelTypes :: ChannelTypes
    , params_channelDomains :: ChannelDomains
    , params_telephonePrefixes :: TelephonePrefixes
    , params_idToTelephonePrefix :: IdToTelephonePrefix
    , params_holidays :: Holidays
    , params_organizations :: Info
    , params_fastLookupCDRImporters :: FastLookupCDRImporters
    , params_services :: ServiceIdMap Service
    , params_servicePrices :: ServiceIdMap [ServicePrice]
    -- ^ @ensure prices are ordered by date in reverse order
    , params_assignedServices :: ServiceIdMap (UnitIdMap [AssignedService])
    -- ^ @ensure assignments are in reverse order of assignment
    , params_incomeRate :: MainRatePlan
    , params_costRate :: MainRatePlan
    , params_conflictingUnitIds :: ConflictingUnitIds
    , params_initialBundleRateServices :: Chunk ServiceCDR
   } deriving (Show)

calc_saveBundleStateImmediatelyAfter :: CallDate -> CallDate
calc_saveBundleStateImmediatelyAfter d0
    = let d1 = localTimeToUTC utc d0
          seconds = - (60 * 30)
          d2 = addUTCTime (realToFrac seconds) d1
      in  utcToLocalTime utc d2

params_peakCodes :: RatingParams -> PeakCodes
params_peakCodes p = holidays_peakCodes $ params_holidays p
{-# INLINE params_peakCodes #-}

params_isDebugMode :: RatingParams -> Bool
params_isDebugMode p = iparams_isDebugMode $ params_initial p
{-# INLINE params_isDebugMode #-}

params_isVoipReseller :: RatingParams -> Bool
params_isVoipReseller p = iparams_isVoipReseller $ params_initial p
{-# INLINE params_isVoipReseller #-}

params_organizationToIgnore :: RatingParams -> Maybe Text.Text
params_organizationToIgnore p = iparams_organizationToIgnore $ params_initial p
{-# INLINE params_organizationToIgnore #-}

params_digitsToMask :: RatingParams -> Int
params_digitsToMask p = iparams_digitsToMask $ params_initial p
{-# INLINE params_digitsToMask #-}

params_defaultTelephonePrefixToNotDisplay :: RatingParams -> Maybe Text.Text
params_defaultTelephonePrefixToNotDisplay p = iparams_defaultTelephonePrefixToNotDisplay $ params_initial p
{-# INLINE params_defaultTelephonePrefixToNotDisplay #-}

params_currencyPrecision :: RatingParams -> Int
params_currencyPrecision p = iparams_currencyPrecision $ params_initial p
{-# INLINE params_currencyPrecision #-}

params_debugFileName :: RatingParams -> Maybe FilePath
params_debugFileName p = iparams_debugFileName $ params_initial p
{-# INLINE params_debugFileName #-}

params_fromDate :: RatingParams -> CallDate
params_fromDate p = iparams_fromDate $ params_initial p
{-# INLINE params_fromDate #-}

params_testToDate :: RatingParams -> Maybe CallDate
params_testToDate p = iparams_testToDate $ params_initial p
{-# INLINE params_testToDate #-}

params_isRateUnbilledCallsEvent :: RatingParams -> Bool
params_isRateUnbilledCallsEvent p = iparams_isRateUnbilledCallsEvent $ params_initial p
{-# INLINE params_isRateUnbilledCallsEvent #-}

params_dbName :: RatingParams -> String
params_dbName p = iparams_dbName $ params_initial p
{-# INLINE params_dbName #-}

params_dbUser :: RatingParams -> String
params_dbUser p = iparams_dbUser $ params_initial p
{-# INLINE params_dbUser #-}

params_dbPasswd :: RatingParams -> String
params_dbPasswd p = iparams_dbPasswd $ params_initial p
{-# INLINE params_dbPasswd #-}

params_configuredRatePlanParsers :: RatingParams -> ConfiguredRatePlanParsers
params_configuredRatePlanParsers p = iparams_configuredRatePlanParsers $ params_initial p
{-# INLINE params_configuredRatePlanParsers #-}

ratingParams_empty :: InitialRatingParams -> RatingParams
ratingParams_empty p = RatingParams {
       params_initial = p
     , params_isShorterSafeTimeFrame = Nothing
     , params_saveBundleStateImmediatelyAfter = iparams_fromDate p
     , params_lastSavedBundleState = (Nothing, IMap.empty)
     , params_rateChanges = rateChanges_empty
     , params_rates = cachedRates_empty
     , params_extensionsToExport = trie_empty
     , params_rateCategories = rateCategories_empty
     , params_ratingCodes = Set.empty
     , params_vendors = Map.empty
     , params_channelTypes = Map.empty
     , params_channelDomains = trie_empty
     , params_telephonePrefixes = trie_empty
     , params_idToTelephonePrefix = IMap.empty
     , params_holidays = []
     , params_organizations = info_empty
     , params_fastLookupCDRImporters = IMap.empty
     , params_services = IMap.empty
     , params_servicePrices = IMap.empty
     , params_assignedServices = IMap.empty
     , params_incomeRate = mainRatePlan_empty
     , params_costRate = mainRatePlan_empty
     , params_unbilledCallsFrom = iparams_fromDate p
     , params_conflictingUnitIds = ISet.empty
     , params_initialBundleRateServices = V.empty
   }

params_priceCategoryCode :: RatingParams -> PriceCategoryId -> RateCategoryCode
params_priceCategoryCode p pId
  = let (_, m) = params_rateCategories p
    in fromJust1 "err 745" $ Map.lookup pId m

-- ----------------------------------------------
-- Check code contracts (used during debugging)

ratingParams_respectCodeContracts :: RatingParams -> Either String ()
ratingParams_respectCodeContracts env =
  case (rateChanges_respectCodeContracts (params_rateChanges env)) of
    Left err -> Left err
    Right _ ->
      case (info_respectCodeContracts (params_organizations env) (params_fromDate env)) of
        Left err -> Left err
        Right _ -> 
          case (serviceParams_respectCodeContracts env) of
            Left err -> Left err
            Right _ -> Right ()

-- | Return an error in case ServiceParams does not respect code contracts.
serviceParams_respectCodeContracts :: RatingParams -> Either String ()
serviceParams_respectCodeContracts s = do
  case (extractAndCheckReverseOrder "(err 1075) prices are not in reverse order of date" (servicePrice_fromDate) (params_servicePrices s)) of
    Left err -> Left err
    Right _ -> 
       case (extractAndCheckReverseOrder2 "(err 1076) service assignmentes are not in reverse order of date" (assignedService_fromDate) (params_assignedServices s)) of
         Left err -> Left err
         Right _ -> Right ()

 where

  extractAndCheckReverseOrder msgError extract map1
    = let l1 = List.map snd $ IMap.toAscList map1
          l2 = List.map (List.map extract) l1
          l3 = List.map isDescendingOrder l2
      in  case (List.all id l3) of
            True -> Right ()
            False -> Left msgError

  extractAndCheckReverseOrder2 msgError extract map1
    = let l1 = List.map snd $ IMap.toAscList map1
      in  mapM_ (extractAndCheckReverseOrder msgError extract) l1

rateChanges_respectCodeContracts :: DBRateChanges -> Either String ()
rateChanges_respectCodeContracts map1
  = let map2 = Map.map (\l1 -> isDescendingOrder $ List.map fst l1) map1
    in  case List.all id (List.map snd $ Map.toList map2) of
          True -> Right ()
          False -> Left $ dbc_error "rp105"

-- -----------------------
-- Service cdrs export

-- | Export the telephone prefixes to use for service-cdrs.
mainRatePlan_exportServiceCDRSTelephonePrefixes :: DB.MySQLConn -> MainRatePlan -> IO ()
mainRatePlan_exportServiceCDRSTelephonePrefixes conn sp = do
  s1 :: S.InputStream RootBundleRatePlan <- S.fromList $ mainRatePlan_bundleRates sp
  s2 :: S.InputStream TelephonePrefixRecord
     <- S.map
          (\s -> let sName = bundle_serviceCDRDescription s
                     sType = bundle_serviceCDRType s
                     prefix = serviceCdr_defaultExternalTelephoneNumber sType sName
                 in TelephonePrefixRecord {
                       tpr_prefix = prefix
                     , tpr_matchOnlyExactDigits = Just $ Text.length prefix
                     , tpr_name = sName
                     , tpr_geographic_location = sName
                     , tpr_rating_code = ""
                     , tpr_operator_type = sType
                     , tpr_display_priority = 10
                     }
          ) s1
  updateS <- telephonePrefixes_update conn
  S.connect s2 updateS
  return ()

-- ----------------------------------------------
-- Read Rates from DB

-- | The character used as decimal separator.
type DecimalSeparator = Char

type FieldSeparator = Char

type UseHeader = Bool

-- | The ID used in PHP world: "ar_rate.id" field value.
--   Every PHP rate, has a unique RateId.
--   Note that RatePlan can be composed of nested rates, having many RateSystemId
--   that are id in a different namespace, and with the RatePlan scope.
type DBRateId = Int

-- | From DBRateId to MainRatePlan.
--   Because rating passage are using a limited time-frame, it is feasible
--   loading all the rates in advance in RAM.
type CachedDBRates = IMap.IntMap MainRatePlan

cachedRates_empty :: CachedDBRates
cachedRates_empty = IMap.empty

-- | "ar_rate.internal_name" used from user for identifying a rate
--   and its successive versions, in a unique name.
--   There can be different rates with the same ReferenceName,
--   but they are valid on distinct time-frames.
type DBRateReferenceName = Text.Text

-- | Store for each DBRateReferenceName, the date from wich is valid,
--   in order to load it if it is not appropiate for the CDR to rate.
--   @ensure: the list of CallDate and DBRateId are in descending order of date.
type DBRateChanges = Map.Map DBRateReferenceName [(LocalTime, DBRateId)]

rateChanges_empty :: DBRateChanges
rateChanges_empty = Map.empty

-- | Add a value only if it is useful for rating the specified time frame.
--   @require: recent values are inserted before older values (descending order on application date)
--   @ensure: recent values are near the head of the list
rateChanges_insertIfInTimeFrame :: DBRateChanges -> CallDate -> DBRateReferenceName -> DBRateId -> CallDate -> DBRateChanges
rateChanges_insertIfInTimeFrame changes rateCDRSFromCallDate rateRef rateId applyRateFromTime
  = Map.insertWith
     (\newValue oldValue
       -> let (applyRateToTime, _) = List.last oldValue
          in case applyRateToTime <= rateCDRSFromCallDate of
               True
                 -> oldValue
                    -- do nothing because the previous more recent rate was sufficient to rate all the CDRS
                    -- in the rating time-frame, and this is superfluous, because too old
               False
                 -> oldValue ++ newValue
                    -- insert the new rate time frame after the previous rate,
                    -- so most recent rates are in head (NOTE: the input is from recent to old rate)

     ) rateRef [(applyRateFromTime, rateId)] changes
     -- NOTE: new rates are added by default by `insertWith` function.

-- | Return the rate version, active at the CDR calldate.
rateChanges_getRateId
  :: DBRateChanges
  -> DBRateReferenceName
  -> LocalTime
  -- ^ the CDR calldate
  -> Maybe (DBRateId
            -- ^ the rate to use
           , Maybe LocalTime
             -- ^ from this date (inclusive) the rate is not any more applicable,
             --   Nothing if it is always applicable
           )

rateChanges_getRateId rateChanges refName cdrTime
  = case Map.lookup refName rateChanges of
      Nothing
        -> Nothing
      Just changes
        -> f Nothing changes

 where

  f _ [] = Nothing

  f l ((t, i):r)
    = if cdrTime >= t
      then Just (i, l)
      else f (Just t) r


-- | Complete RatingParams with the info about the specified rates.
ratePlan_loadRates
  :: DB.MySQLConn
  -> RatingParams
     -- @require the time frame is safe: there are no changes in main or income cost rates
  -> HSet.HashSet DBRateReferenceName
     -- ^ load only the rate with the specified internal name
  -> IO RatingParams

ratePlan_loadRates conn p1 ratesToImport = do

  -- Get the rates used in the rating time frame,
  -- without loading also the big content.

  let q1 = [str| SELECT
               |   id
               | , internal_name
               | , from_time
               | FROM ar_rate
               | WHERE from_time <= ?
               | ORDER BY from_time DESC
               |]

  (_, inS1) <- DB.query conn (DB.Query q1) [toDBLocalTime $ params_fromDate p1]

  changes :: DBRateChanges
    <- S.fold (\s [rId', rInternalName', rToTime']
                 -> let rId = fromDBInt rId'
                        rInternalName = fromDBText rInternalName'
                        rToTime = fromDBLocalTime rToTime'
                    in case HSet.member rInternalName ratesToImport of
                         False
                           -> s
                         True
                           -> rateChanges_insertIfInTimeFrame s (params_fromDate p1) rInternalName rId rToTime
              ) rateChanges_empty inS1

  -- Load the big content only for really used rates.

  let q2 = [str| SELECT
               |   ar_rate_format.internal_name
               | , ar_rate.source_data_file
               | FROM ar_rate
               | INNER JOIN ar_rate_format
               | ON ar_rate.ar_rate_format_id = ar_rate_format.id
               | WHERE ar_rate.id = ?
               |]

  getRateStmt <- DB.prepareStmt conn (DB.Query q2)

  rates :: CachedDBRates <- M.foldM (loadRate getRateStmt) cachedRates_empty (List.concat $ Map.elems changes)

  return p1 {
             params_rateChanges = changes
           , params_rates = rates
           }

 where

  loadRate :: DB.StmtID -> CachedDBRates -> (LocalTime, DBRateId) -> IO CachedDBRates
  loadRate getRateStmt s (_, rateId) = do
    (_, inS2) <- DB.queryStmt conn getRateStmt [toDBInt64 rateId]
    maybeR  <- S.read inS2
    case maybeR of
      Nothing
        -> throwIO $ AsterisellException
                     ("The rate with id \"" ++ (show rateId) ++ "\" is not present in the DB. The CDRs can not be rated. This is an error of the application. If the problem persist, contact the assistance.")

      Just [formatName', rateContent']
        -> do S.skipToEof inS2
              let formatName = fromDBText formatName'
              rateContent <-
                case fromMaybeDBValue fromDBByteString rateContent' of
                  Nothing
                    -> throwIO $ AsterisellException ("The rate with id  \"" ++ show rateId ++ "\" has no content. Add the content of the rate.")
                  Just c
                    -> return c

              case configuredRatePlanParsers_get (params_configuredRatePlanParsers p1) formatName of
                Nothing
                  -> throwIO $ AsterisellException
                               ("The rate format \"" ++ (Text.unpack formatName) ++ "\" used for the definition of rate with id " ++ show rateId ++ "\" is unknown, and the rate specification can not be parsed. The call report will not report the CDRs with problems, because this is a critical error preventing the import and rating of all the CDRs in the timeframe. The CDRs can not be rated. Use a correct rate format name, or contact the assistance, for adding the support for this new rate format in the application code.")

                Just rateParser
                  -> do case rateParser p1 rateContent of
                          Left err
                            -> throwIO $ AsterisellException
                                         ("Error during parsing of rate id " ++ show rateId ++ ". CDRs will be not rated. Fix the rate specification. Parsing error: " ++ err)
                          Right ratePlan1
                            -> return $ IMap.insert rateId ratePlan1 s

-- ------------------------------------------------------
-- Normalized CSV rates

-- | A CSV rate converted to a standard format.
--   This conversion can not be applied to all rates, but to many of them.
data NormalizedRate =
  NormalizedRate {
      nrr_description :: Text.Text
    , nrr_operator :: Text.Text
    , nrr_prefix :: Text.Text
    , nrr_costByMinute :: MonetaryValue
    , nrr_costOnCall :: MonetaryValue
                 } deriving(Show, Eq, Ord, Generic)

instance CSV.ToField MonetaryValue where
   toField v =
     let v' :: Double = fromRational v
     in  CSV.toField v'

maybeMonetaryValue_toField :: Maybe MonetaryValue -> CSV.Field
maybeMonetaryValue_toField Nothing = "\\N"
maybeMonetaryValue_toField (Just v) = CSV.toField v

normalizedRate_empty :: NormalizedRate
normalizedRate_empty =
  NormalizedRate {
      nrr_description = ""
    , nrr_operator = ""
    , nrr_prefix = ""
    , nrr_costByMinute = 0
    , nrr_costOnCall = 0
                 }

instance CSV.ToRecord NormalizedRate where
    toRecord r =
      CSV.record [
           CSV.toField $ nrr_description r
         , CSV.toField $ nrr_operator r
         , CSV.toField $ nrr_prefix r
         , CSV.toField $ monetaryValue_show $ nrr_costByMinute r
         , CSV.toField $ monetaryValue_show $ nrr_costOnCall r
         ]


normalizedRate_sameCost :: NormalizedRate -> NormalizedRate -> Bool
normalizedRate_sameCost r1 r2 =
  (nrr_costByMinute r1 == nrr_costByMinute r2) &&
    (nrr_costOnCall r1 == nrr_costOnCall r2)
{-# INLINE normalizedRate_sameCost #-}

-- | Parse a rate specification (usually a CSV file) and produce a file in csv-header-5col-costOnCall format,
--  i.e. description, operator, prefix, cost-by-minute, cost-on-call, with an initial header line.
type RatePlanNormalizer = BS.ByteString -> (Either String NormalizedRateTrie)

instance Show RatePlanNormalizer where
    show s = "<normalize rate plan>"

-- | A trie of NormalizedRate, using the rate prefix as key.
type NormalizedRateTrie = Trie.Trie NormalizedRate

normalizedRateTrie_insert :: NormalizedRateTrie -> BS.ByteString -> NormalizedRate -> NormalizedRateTrie
normalizedRateTrie_insert trie1 prefix value =
  Trie.insert prefix (value { nrr_prefix = fromByteStringToText prefix }) trie1
{-# INLINE normalizedRateTrie_insert #-}

-- | Merge a base rate with two specific rates, calculating the minimum specific rate.
--   In the rate plan the specific rate is used for rating a call, and the base rate
--   is used only if there are no valid entries in the specific rate. So a shorter prefix
--   in the specific rate is stronger of a shorter prefix on the base rate, because
--   the base rate is called only in case of missing valid prefixes.
--
--   Specific rate can be specified using two different approach:
--   * specifying a match-all-longest prefix way, that overrides all prefixes of the base-rate
--   * specifying an exception-like prefix, that overrides only the single exact prefix, and use the base-rate otherwise
--
--   These two approaches are complementary, and they can be used both:
--   * firt the match-all-longest prefix rate is applied
--   * then the resulting rate is patched with the exact-prefix rate
--
--   REQUIRE: the rates to merge have prefixes with implicit final "*",
--   so they match all longest prefixes.
--
normalizedRates_fromSpecificCosts
  :: NormalizedRateTrie
  -- ^ the base rate
  -> NormalizedRateTrie
  -- ^ the specific rate with prefixes matching all stronger prefixes
  -> NormalizedRateTrie
  -- ^ the specific rate with prefixes matching exactly the prefix, but not stronger prefixes
  -> NormalizedRateTrie
  -- ^ the specific rate with only the prefixes with a cost different from base rate

normalizedRates_fromSpecificCosts baseRate matchAllPrefixesRate matchExactPrefixesRate  =
  let
      -- All prefixes to match
      allPrefixes :: [BS.ByteString] =
        Trie.keys $ Trie.unionL baseRate $ Trie.unionL matchAllPrefixesRate matchExactPrefixesRate

      -- Only specific prefixes
      specificPrefixes :: NormalizedRateTrie =
        Trie.unionL matchAllPrefixesRate matchExactPrefixesRate

      -- Create a combined rate favouring the exact prefix first, and the specific rate,
      -- and use baseRate as fallback.
      combinedRate :: NormalizedRateTrie =
        List.foldl'
          (\trie1 prefix ->
             let rate :: NormalizedRate =
                   case Trie.lookup prefix matchExactPrefixesRate of
                     Just r -> r
                     Nothing ->
                       case Trie.match matchAllPrefixesRate prefix of
                         Just (_, r, _) -> r
                         Nothing ->
                           case Trie.match baseRate prefix of
                             Just (_, r, _) -> r
                             Nothing -> error "ERR 142753 unexpected condition in code"

             in normalizedRateTrie_insert trie1 prefix rate
          ) Trie.empty allPrefixes

      -- Compress longest prefixes having the same cost of shorter prefixes.
      -- Do not insert prefixes if the base-rate suffices.
      compressedRate :: NormalizedRateTrie =
        List.foldl'
          (\trie1 (currPrefix, currRate) ->
             let insertInTrie =
                   case Trie.match specificPrefixes currPrefix of
                     Nothing ->
                       False
                       -- use base rate as fallback, because the prefix is never matched by specific rate
                     Just _ ->
                       case Trie.match trie1 currPrefix of
                         Nothing ->
                           case Trie.match baseRate currPrefix of
                             Nothing -> True
                             -- this prefix is only in specific rates, and not in base rate,
                             -- so inserti it
                             Just (_, baseRate, _) -> not $ normalizedRate_sameCost currRate baseRate
                             -- If the rate to insert is equal to base-rate,
                             -- do not insert the prefix and fall back to base-rate.
                             -- Prefixes are received in alphabetic order, so no new matching prefix
                             -- will be added later.
                         Just (_, specificRate, _) -> not $ normalizedRate_sameCost currRate specificRate
                             -- If the he rate to insert is equal to an already inserted prefix,
                             -- do not insert the prefix and use the more compact prefix instead.
                             -- Prefixes are received in alphabetic order, so no new matching prefix
                             -- will be added later.

                 trie2 = if insertInTrie
                         then normalizedRateTrie_insert trie1 currPrefix currRate
                         else trie1

             in  trie2
          )
          Trie.empty
          (Trie.toList combinedRate)

 in normalizedRates_completeDescriptions compressedRate [matchExactPrefixesRate, matchAllPrefixesRate, baseRate]

betterDescr :: Text.Text -> Text.Text -> Text.Text
betterDescr specificDescr baseDescr =
        if Text.null specificDescr then baseDescr else specificDescr
{-# INLINE betterDescr #-}

-- | Complete the missing descriptions for prefixes using the other rates.
--   First rates have more priority than last rates.
normalizedRates_completeDescriptions :: NormalizedRateTrie -> [NormalizedRateTrie] -> NormalizedRateTrie
normalizedRates_completeDescriptions specificRate descrRates =
  case descrRates of
    [] -> specificRate
    (r:rs) -> normalizedRates_completeDescriptions (completeDescr specificRate r) rs

  where

      -- | An empty description is replaced with a more complete description if present in the second trie.
      completeDescr :: NormalizedRateTrie -> NormalizedRateTrie -> NormalizedRateTrie
      completeDescr specificTrie baseTrie =
        fmap
          (\specificDescr ->
            case Trie.lookup (fromTextToByteString $ nrr_prefix specificDescr) baseTrie of
              Nothing -> specificDescr
              Just baseDescr ->
                let description2 = betterDescr (nrr_description specificDescr) (nrr_description baseDescr)
                    operator2 = betterDescr (nrr_operator specificDescr) (nrr_operator baseDescr)
                in specificDescr {
                     nrr_description = description2
                   , nrr_operator = operator2
                                 }
          )
          specificTrie

-- | The format to use for parsing specific rates.
specificRateFormat :: Text.Text
specificRateFormat = "csv-header-5col-costOnCall"

normalizedRates_header :: LBS.ByteString
normalizedRates_header = "description,telephone operator,telephone prefix,cost by minute,cost on call\r\n"

-- | The specific rate in CSV format.
normalizedRates_toSpecificRateCSV :: NormalizedRateTrie -> LBS.ByteString
normalizedRates_toSpecificRateCSV costTrie =
      LBS.append
        normalizedRates_header
        (CSV.encode $ Trie.elems costTrie)

-- | A human readable summary of statistical differences between the base and specific rate.
normalizedRates_info :: NormalizedRateTrie -> NormalizedRateTrie -> Text.Text
normalizedRates_info baseRate specificRate =
  let
      diffs = normalizedRates_toDiffRate baseRate specificRate

      diffsCount = Trie.size diffs

      sameCostCount = List.length $ List.filter normalizedDiffRate_sameCost $ Trie.elems diffs

      diffCostCount = diffsCount - sameCostCount

      specificRateCount = Trie.size specificRate

      showPerc :: Int -> Int -> String
      showPerc _ 0 = "0%"
      showPerc x y =
        (show $ round $ 100.0 * (fromIntegral x) / (fromIntegral y)) ++ "%"

      reductionPerc = showPerc specificRateCount diffsCount

      savingPerc = showPerc (diffsCount - specificRateCount) diffsCount

      sameCostPerc = showPerc sameCostCount diffsCount

      diffCostPerc = showPerc diffCostCount diffsCount

  in Text.pack $
       "There are " ++ show diffsCount ++ " total distinct telephone prefixes in base and specific rate.\n" ++
       "There are " ++ show specificRateCount ++ " prefixes (" ++ reductionPerc ++ " of total prefixes) in the specific rate file, with a saving in size of " ++ savingPerc ++ "\n" ++
       "There are " ++ show diffCostCount ++ " (" ++ diffCostPerc ++ " of base rate) prefixes with different cost respect base rate. The two rates are similar in " ++ sameCostPerc ++ " of prefixes."

-- | A CSV rate converted to a standard format.
--   This conversion can not be applied to all rates, but to many of them.
data NormalizedDiffRate =
  NormalizedDiffRate {
      ndr_description :: Text.Text
    , ndr_operator :: Text.Text
    , ndr_prefix :: Text.Text
    , ndr_oldCostByMinute :: Maybe MonetaryValue
    , ndr_oldCostOnCall :: Maybe MonetaryValue
    , ndr_newCostByMinute :: Maybe MonetaryValue
    , ndr_newCostOnCall :: Maybe MonetaryValue
                 } deriving(Show, Eq, Ord, Generic)

monetaryValue_maybeShow :: Maybe MonetaryValue -> Maybe String
monetaryValue_maybeShow Nothing = Nothing
monetaryValue_maybeShow (Just v) = Just $ monetaryValue_show v

float_maybeShow :: Maybe Float -> Maybe String
float_maybeShow Nothing = Nothing
float_maybeShow (Just v) = Just $ (showFFloat Nothing v) ""

instance CSV.ToRecord NormalizedDiffRate where
    toRecord r =
      CSV.record [
           CSV.toField $ ndr_description r
         , CSV.toField $ ndr_operator r
         , CSV.toField $ ndr_prefix r
         , CSV.toField $ monetaryValue_maybeShow $ ndr_oldCostByMinute r
         , CSV.toField $ monetaryValue_maybeShow $ ndr_oldCostOnCall r
         , CSV.toField $ monetaryValue_maybeShow $ ndr_newCostByMinute r
         , CSV.toField $ monetaryValue_maybeShow $ ndr_newCostOnCall r
         , CSV.toField $ normalizedDiffRate_sameCostField r
         ]

normalizedDiffRate_sameCost :: NormalizedDiffRate -> Bool
normalizedDiffRate_sameCost r =
  (ndr_oldCostByMinute r == ndr_newCostByMinute r) &&
  (ndr_oldCostOnCall r == ndr_newCostOnCall r)
{-# INLINE normalizedDiffRate_sameCost #-}

normalizedDiffRate_sameCostField :: NormalizedDiffRate -> Text.Text
normalizedDiffRate_sameCostField r =
  if normalizedDiffRate_sameCost r then "true" else "false"
{-# INLINE normalizedDiffRate_sameCostField #-}

normalizedDiffRate_calc :: NormalizedRateTrie -> NormalizedRateTrie -> BS.ByteString -> NormalizedDiffRate
normalizedDiffRate_calc baseRate specificRate prefix =
  let (baseCost, baseCostOnCall) =
        case Trie.match baseRate prefix of
          Nothing -> (Nothing, Nothing)
          Just (_, r, _) -> (Just $ nrr_costByMinute r, Just $ nrr_costOnCall r)

      (newCost, newCostOnCall) =
        case Trie.match specificRate prefix of
          Nothing -> (baseCost, baseCostOnCall)
          Just (_, r, _) -> (Just $ nrr_costByMinute r, Just $ nrr_costOnCall r)

      (specificDescr, specificOperator) =
        case Trie.lookup prefix specificRate of
          Nothing ->("", "")
          Just r -> (nrr_description r, nrr_operator r)

      (baseDescr, baseOperator) =
            case Trie.lookup prefix baseRate of
              Nothing -> ("", "")
              Just r -> (nrr_description r, nrr_operator r)

      calcDiscount :: Maybe MonetaryValue -> Maybe MonetaryValue -> Maybe Float
      calcDiscount mBaseCost mSpecificCost =
        case (mBaseCost, mSpecificCost) of
          (Just 0, Just 0) -> Just 0
          (Just _, Just 0) -> Nothing
          (Just baseCost, Just specificCost) ->
                Just $
                      fromRational $
                        ((baseCost - specificCost) / specificCost) * (fromIntegral 100)
          _ -> Nothing

  in NormalizedDiffRate {
        ndr_description = betterDescr specificDescr baseDescr
      , ndr_operator = betterDescr specificOperator baseOperator
      , ndr_prefix = fromByteStringToText prefix
      , ndr_oldCostByMinute = baseCost
      , ndr_oldCostOnCall = baseCostOnCall
      , ndr_newCostByMinute =  newCost
      , ndr_newCostOnCall = newCostOnCall
                       }

normalizedRate_diffHeader :: LBS.ByteString
normalizedRate_diffHeader = "description,telephone operator,telephone prefix,old cost by minute,old cost on call,new cost by minute,new cost on call,same cost\r\n"

normalizedRates_toDiffRate :: NormalizedRateTrie -> NormalizedRateTrie -> Trie.Trie NormalizedDiffRate
normalizedRates_toDiffRate baseRate specificRate =
  let

      -- take all prefixes
      allPrefixes = Trie.unionL baseRate specificRate

      diffs =
        Trie.fromList $
          List.map (\prefix ->
                      (prefix, normalizedDiffRate_calc baseRate specificRate prefix))
          (Trie.keys allPrefixes)

  in diffs

normalizedDiffRates_toCSV :: Trie.Trie NormalizedDiffRate -> LBS.ByteString
normalizedDiffRates_toCSV diffs =
  LBS.append
    normalizedRate_diffHeader
    (CSV.encode $ Trie.elems diffs)

-- ------------------------------------------------------
-- External rates: rates stored as CSV files and similar

-- | Traverse a MainRatePlan and complete info about ExternalRateReference
mainRatePlan_externalRateReferences :: HSet.HashSet ExternalRateReference -> MainRatePlan -> HSet.HashSet ExternalRateReference
mainRatePlan_externalRateReferences s p = f s p
 where

    f :: HSet.HashSet ExternalRateReference -> MainRatePlan -> HSet.HashSet ExternalRateReference
    f s1 p1
      = let s2 = List.foldl' ratePlan_externalRateReferences s1 (mainRatePlan_normalRates p1)
            s3 = List.foldl' ratePlan_externalRateReferences s2 (List.concatMap bundle_children $ mainRatePlan_bundleRates p1)
         in s3

-- | Traverse a RatePlan and complete info about ExternalRateReference
ratePlan_externalRateReferences :: HSet.HashSet ExternalRateReference -> RatePlan -> HSet.HashSet ExternalRateReference
ratePlan_externalRateReferences s1 p
  = let s2 = case rate_use p of
               Nothing -> s1
               Just r -> HSet.insert r s1
        s3 = List.foldl' ratePlan_externalRateReferences s2 (rate_children p)
        s4 = List.foldl' ratePlan_externalRateReferences s3 (rate_elsePart p)
    in  s4

-- | A name for a rate format, as imported from the external database.
type RateFormatName = Text.Text

-- | Parse a rate specification, deriving the matching function.
type RatePlanParser = RatingParams -> BS.ByteString -> (Either String MainRatePlan)

instance Show RatePlanParser where
    show s = "<rate plan parser>"

type ConfiguredRatePlanParsers = Map.Map RateFormatName (RatePlanParser, Maybe RatePlanNormalizer)

-- | The logical type associated to main rate plans.
ratePlanSpecificationType :: Text.Text
ratePlanSpecificationType = Text.pack "rate-plan-specification"

-- | The reference name of an external rate.
type ExternalRateReference = Text.Text

configuredRatePlanParsers_get :: ConfiguredRatePlanParsers -> RateFormatName -> Maybe RatePlanParser
configuredRatePlanParsers_get conf1 l =
  case Map.lookup l conf1 of
    Just (r, _) -> Just r
    Nothing -> Nothing

configuredRatePlanNormalizer_get :: ConfiguredRatePlanParsers -> RateFormatName -> Maybe RatePlanNormalizer
configuredRatePlanNormalizer_get conf1 l =
  case Map.lookup l conf1 of
    Just (_, maybeR) -> maybeR
    Nothing -> Nothing

-- | Retrieve a RatePlan, using PHP reference name.
env_getRate
  :: RatingParams
  -> DBRateReferenceName
  -> LocalTime
  -- ^ the CDR calldate
  -> Maybe (  MainRatePlan
              -- ^ the rate plan to apply
            , Maybe LocalTime
              -- ^ at this date in the future respect CDR calldate, the rate plan is not any more applicable.
              --   Nothing if it is always applicable.
           )

env_getRate env refName cdrTime
  = let rateChanges = params_rateChanges env
    in case rateChanges_getRateId rateChanges refName cdrTime of
         Nothing
           -> Nothing
         Just (i, t)
           -> Just (fromJust1 "rp30" $ IMap.lookup i (params_rates env), t)

-- ---------------------------
-- Generic parsing of rates

-- | Show a CDR content, with also additional initializated content.
--   This make sense only if the CDR has passed the first initialization pass, and there are errors in the next rating part.
rate_showDebug :: RatingParams -> CDR -> String
rate_showDebug env cdr
  =    (cdr_showDebug cdr)
    ++ (addLine "organization-ids" organizationIds)
    ++ (addLine "price-category" priceCategoryName)
    ++ (addLine "communication-channel-type" channelTypeName)
    ++ (addLine "vendor" vendorName)

 where

    addLine l v
      = "\n   " ++ l ++ ": " ++ v

    info = params_organizations env

    unassigned = "<not yet assigned>"

    (_, rateCategories) = params_rateCategories env

    invertMap m
       =  Map.fromList $  List.map (\(x,y) -> (y,x)) $ Map.toList m

    channelTypes
       = invertMap $ params_channelTypes env

    vendors
       = invertMap $ params_vendors env

    priceCategoryName
       = case cdr_priceCategoryId cdr of
           Nothing
             -> unassigned
           Just id
             -> Text.unpack $ fromJust1 "rpa1" $ Map.lookup id rateCategories

    organizationIds
       = case cdr_cachedParentIdHierarchy cdr of
           Nothing
             -> unassigned
           Just ids
             -> List.concatMap (\i -> "/" ++ show i) ids

    channelTypeName
      = case cdr_communicationChannelTypeId cdr of
          Nothing
            -> unassigned
          Just id
            -> Text.unpack $ fromJust1 "rpa2" $ Map.lookup id channelTypes

    vendorName
      = case cdr_vendorId cdr of
          Nothing
            -> unassigned
          Just id
            -> case Map.lookup id vendors of
                 Nothing
                   -> "unnamed-vendor-" ++ show id
                 -- DEV-NOTE: vendors without an explicit internal-name remain unnamed
                 Just n
                   -> Text.unpack n

-- --------------------------------------------------
-- Manage Rent Services associated to customers.
-- They are different from Bundle Service CDRs, because they are explicitely associated to a customer though Service related tables,
-- and they are not associated to BundleRate and Price Category.

data Service
  = Service {
      service_id :: !Int
    , service_name :: !Text.Text
    , service_description :: !Text.Text
    , service_priceIsProportionalToActivationDate :: !Bool
    , service_priceChangeWithPriceList :: !Bool
    , service_isAppliedOnlyOneTime :: !Bool
    , service_schedule :: BundleRateTimeFrame
  } deriving(Show, Generic, NFData)


data ServicePrice
  = ServicePrice {
      servicePrice_id :: Int
    , servicePrice_serviceId :: Int
    , servicePrice_fromDate :: CallDate
    , servicePrice_price :: MonetaryValue
    } deriving (Show, Generic, NFData)

data AssignedService
  = AssignedService {
      assignedService_id :: Int
    , assignedService_serviceId :: Int
    , assignedService_unitId :: Int
    , assignedService_nrOfItems :: Int
    , assignedService_fromDate :: CallDate
    , assignedService_discount :: Rational
    } deriving (Show, Generic, NFData)

-- | Load services from the DB.
serviceParams_load :: DB.MySQLConn -> RatingParams -> IO RatingParams
serviceParams_load  conn envParams = do

     let q1 = [str| SELECT
                  |   id
                  | , customer_name
                  | , customer_description
                  | , customer_price_depend_from_activation_date
                  | , customer_price_change_with_price_list
                  | , is_applied_only_one_time
                  | , schedule_timeframe
                  | , schedule_from
                  | , schedule_at
                  | FROM ar_service
                  |]

     (_, inS) <- DB.query_ conn q1
     services <- S.foldM importService IMap.empty inS

     let q2 = [str| SELECT
                  |   id
                  | , ar_service_id
                  | , from_date
                  | , price
                  | FROM ar_service_price
                  | ORDER BY from_date DESC
                  |]

     (_, inS) <- DB.query_ conn q2
     let precisionDigits = params_currencyPrecision envParams
     servicePrices <- S.foldM (importServicePrice precisionDigits) IMap.empty inS

     let q3 = [str| SELECT
                  |   id
                  | , ar_service_id
                  | , ar_organization_unit_id
                  | , nr_of_items
                  | , from_date
                  | , discount
                  | FROM ar_assigned_service
                  | ORDER BY from_date DESC, nr_of_items DESC
                  |]


     (_, inS) <- DB.query_ conn q3
     assignedServices <- S.foldM importAssignedService IMap.empty inS

     let r = envParams {
                  params_services = services
                , params_servicePrices = servicePrices
                , params_assignedServices = assignedServices
             }

     case (params_isDebugMode envParams) of
        False
            -> return r
        True
            -> case serviceParams_respectCodeContracts r of
                 Right ()
                     -> return r
                 Left err
                     -> throwIO $ AsterisellException $ "Error in application code. Failed code contracts. " ++ err

 where

   importService
     map1
     [  id'
      , customer_name'
      , customer_description'
      , customer_price_depend_from_activation_date'
      , customer_price_change_with_price_list'
      , is_applied_only_one_time'
      , schedule_timeframe'
      , schedule_from'
      , schedule_at' ] = do

       let id = fromDBInt id'
       customer_name <- nn "service" id fromDBText customer_name'
       let customer_description
             = case fromMaybeDBValue fromDBText customer_description' of
                 Nothing -> ""
                 Just d -> d

       customer_price_depend_from_activation_date <- nn "service" id fromDBBool customer_price_depend_from_activation_date'
       customer_price_change_with_price_list <- nn "service" id fromDBBool customer_price_change_with_price_list'
       is_applied_only_one_time <- nn "service" id fromDBBool is_applied_only_one_time'
       schedule_timeframe <- nn "service" id fromDBText schedule_timeframe'
       schedule_from <- nn "service" id fromDBText schedule_from'
       schedule_at <- nn "service" id fromDBTime schedule_at'

       s <- case schedule_timeframe of
              "monthly"
                -> case fromTextToInt $ schedule_from of
                     Nothing
                       -> throwIO $ AsterisellException $ "In service with id " ++ show id ++ "expected a number in field schedule_from, instead of " ++ (show $ schedule_from)
                     Just i
                       -> return $ MonthlyTimeFrame i schedule_at

              "weekly"
                -> case schedule_from of
                     "Monday" -> return $ WeeklyTimeFrame 1 schedule_at
                     "Tuesday" -> return $ WeeklyTimeFrame 2 schedule_at
                     "Wednesday" -> return $ WeeklyTimeFrame 3 schedule_at
                     "Thursday" -> return $ WeeklyTimeFrame 4 schedule_at
                     "Friday" -> return $ WeeklyTimeFrame 5 schedule_at
                     "Saturday" -> return $ WeeklyTimeFrame 6 schedule_at
                     "Sunday" -> return $ WeeklyTimeFrame 7 schedule_at
                     _ -> throwIO $ AsterisellException $ "In service with id " ++ show id ++ " expected a day of week like Monday, Tuesday, and so on, in schedule_from, instead of " ++ show schedule_from

       let r = Service {
                service_id = id
              , service_name = customer_name
              , service_description = customer_description
              , service_priceIsProportionalToActivationDate = customer_price_depend_from_activation_date
              , service_priceChangeWithPriceList = customer_price_change_with_price_list
              , service_isAppliedOnlyOneTime = is_applied_only_one_time
              , service_schedule = s
              }

       return $ IMap.insert id r map1

   importService _ _ = throwIO $ AsterisellException "err 1755 in code: unexpected DB format for ar_service"

   addToHead [newValue] oldList = newValue:oldList

   importServicePrice
     precision
     map1
     [  id'
      , ar_service_id'
      , from_date'
      , price'] = do

        let id = fromDBInt id'
        ar_service_id <- nn "service_price" id fromDBInt ar_service_id'
        from_date <- nn "service_price" id fromDBLocalTime from_date'
        price <- fromIntegerWithFixedPrecisionToMonetaryValue precision <$> nn "service_price" id fromDBInt price'

        let r = ServicePrice {
                   servicePrice_id = id
                 , servicePrice_serviceId  = ar_service_id
                 , servicePrice_fromDate = from_date
                 , servicePrice_price = price
                }
        return $ IMap.insertWith (addToHead) id [r] map1

   importServicePrice _ _ _ = throwIO $ AsterisellException "err 1756 in code: unexpected DB format for ar_service_price"

   importAssignedService
     map1
     [ id'
     , ar_service_id'
     , ar_organization_unit_id'
     , nr_of_items'
     , from_date'
     , discount'
     ] = do
            let id = fromDBInt id'
            ar_service_id <- nn "assigned_service" id fromDBInt ar_service_id'
            ar_organization_unit_id <- nn "assigned_service" id fromDBInt ar_organization_unit_id'
            let nr_of_items
                  = case fromMaybeDBValue fromDBInt nr_of_items' of
                      Nothing -> 0
                      Just i -> i
            from_date <- nn "assigned_service" id fromDBLocalTime from_date'
            discount'' <- nn "assigned_service" id fromDBInt discount'

            let discount :: Rational = (toRational $ discount'') / (toRational 100)

            let r = AssignedService {
                      assignedService_id = id
                    , assignedService_serviceId = ar_service_id
                    , assignedService_unitId = ar_organization_unit_id
                    , assignedService_nrOfItems = nr_of_items
                    , assignedService_fromDate= from_date
                    , assignedService_discount = discount
                    }
            return $ serviceParams_insertAssignment map1 r

   importAssignedService  _ _ = throwIO $ AsterisellException "err 1757 in code: unexpected DB format for ar_assigned_service"

type ServiceId = Int

type ServiceIdMap a = RatePlanIdMap a

serviceParams_insertAssignment :: ServiceIdMap (UnitIdMap [AssignedService]) -> AssignedService -> ServiceIdMap (UnitIdMap [AssignedService])
serviceParams_insertAssignment map1 assignment
  = let serviceId = assignedService_serviceId assignment
        unitId = assignedService_unitId assignment

        addToHead [newValue] oldList = newValue:oldList

    in case IMap.lookup serviceId map1 of
         Nothing
           -> IMap.insert serviceId (IMap.singleton unitId [assignment]) map1
         Just map2
           -> IMap.insert serviceId (IMap.insertWith (addToHead) unitId [assignment] map2) map1

data CalcService
  = CalcService {
      calcService_price :: MonetaryValue
    , calcService_nrOfItems :: Int
    , calcService_timeFrame :: TimeFrame
    , calcService_discount :: Rational
    } deriving(Show)

-- ----------------------------------------
-- Debug

ratingParamsForTest :: Int -> RatingParams
ratingParamsForTest precisionDigits
  = ratingParams_empty $ InitialRatingParams {
        iparams_isDebugMode = True
      , iparams_isVoipReseller = True
      , iparams_digitsToMask = 0
      , iparams_defaultTelephonePrefixToNotDisplay = Nothing
      , iparams_currencyPrecision = precisionDigits
      , iparams_organizationToIgnore = Nothing
      , iparams_debugFileName = Nothing
      , iparams_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2000-01-01 00:00:00"
      , iparams_testToDate = Nothing
      , iparams_isRateUnbilledCallsEvent = False
      , iparams_dbName = ""
      , iparams_dbUser = ""
      , iparams_dbPasswd = ""
      , iparams_configuredRatePlanParsers = Map.empty
      , iparams_generateExportedCDRInfo = False
    }
