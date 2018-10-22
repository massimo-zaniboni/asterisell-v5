{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Manage rate plans, services, bundle rates, and rate CDRs.
--
module Asterisell.RatePlan (
    RateRole(..)
  , ConfiguredRatePlanParsers
  , RatePlanParser
  , FieldSeparator
  , DecimalSeparator
  , RatingParams(..)
  , InitialRatingParams(..)
  , ExtensionsToExport
  , UseHeader
  , MatchStrenght(..)
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
  , MapToRootBundleRate
  , mainRatePlan_deriveMapFromChildrenRateToMainRate
  , mainRatePlan_getBundleRateStartCallDate
  , mainRatePlan_getBundleRateEndCallDate
  , MainRatePlan(..)
  , CheckedMainRatePlan
  , fromCheckedMainRatePlan
  , mainRatePlan_empty
  , ratingParamsForTest
  , mainRatePlanRef
  , BundlePartition
  , bundlePartition_all
  , bundlePartition_contains
  , bundlePartition_containsUnitId
  , bundleState_partition
  , bundleState_join
  , bundleState_areThereServiceCdrs
  , RootBundleRatePlan(..)
  , BundleRatePlan(..)
  , BundleParams(..)
  , RatePlan(..)
  , RateParams(..)
  , SelectedRate
  , RateCost(..)
  , mainRatePlan_calcCost
  , BundleRateTimeFrame(..)
  , BundleState
  , bundleState_empty
  , bundleState_fake
  , CalcParams(..)
  , calcParams_empty
  , calcParams_defaultValues
  , calcParams_override
  , calcParams_overrideList
  , calcParams_calc
  , ratingParams_empty
  , env_getRate
  , bundleState_updateAccordingRatingParams
  , bundleState_updateAndGetServiceCdrs
  , bundleState_updateToSomeCallDate
  , RatingDebugInfo(..)
  , iparams_dbConf
  , params_dbConf
  , BundleRateSystemId
  , BundleRateUnitId
  , CallDate
  , RatePlanIdMap
  , UnitIdMap
  , TimeFrame
  , timeFrame_duration
  , mainRatePlan_exportServiceCDRSTelephonePrefixes
  , tt_ratePlanTests
  , timeFrame_fromCallDate
  , timeFrame_fromCallDate1
  , ratingParams_respectCodeContracts
  , timeFrames_allInsideRatingPeriod
  , service_exportServiceCDRSTelephonePrefixes
  , service_defaultExternalTelephoneNumber
  , service_generate
  , serviceParams_load
  , ratePlan_loadRatingParams
  , CalcService(..)
  , ServiceId
  , ServiceIdMap
  , tt_servicesTests
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
  ) where

import Asterisell.DB
import Asterisell.Cdr
import Asterisell.CustomerSpecificImporters
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
import Control.Monad.State.Strict as State
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Ord as Ord
import Control.Exception.Assert.Sugar

import qualified Data.Map.Strict as SMap
import qualified Data.Map as Map
import Data.Vector as V (length)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding

import Control.Monad as M
import Control.Monad.IO.Class       (liftIO)
import Data.Word
import System.FilePath.Posix
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Calendar.WeekDate
import qualified Control.Exception.Base as Exception
import Control.Monad.Except
import Data.IORef

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char (ord)

import qualified Test.HUnit as HUnit

import qualified Data.Csv as CSV

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.Prim((<|>), (<?>))
import Text.Parsec.ByteString.Lazy as Parsec
import qualified Text.Parsec.Char as Parsec

import qualified Data.Text.Lazy.IO as LazyIO
import qualified Data.Text.Lazy as LazyText

import qualified Data.ByteString.Base64 as B64
import qualified Codec.Compression.QuickLZ as LZ

import System.IO as IO
import Debug.Trace

import Data.Maybe
import Data.Set as Set
import Data.HashSet as HSet
import Data.IntMap as IMap
import Data.Hashable
import qualified Data.Serialize as DBS
import qualified Data.Serialize.Text as DBS
import Data.Either

import qualified System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S
import qualified Data.Vector as V

import Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc

import GHC.Generics
import Control.DeepSeq
import Control.Exception.Safe

-- -------------------
-- MATCH STRENGHT

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

---------------
-- RATE PLAN --
---------------

data RateRole
  = IncomeRate
  | CostRate
 deriving(Ord, Eq)

mainRatePlanRef :: RateRole -> DBRateReferenceName
mainRatePlanRef IncomeRate = "main-income-rate"
mainRatePlanRef CostRate = "main-cost-rate"
{-# INLINE mainRatePlanRef #-}

instance Show RateRole where
  show IncomeRate = "income"
  show CostRate = "cost"

-- | The complete name of a rate, joining all the UserRateRefName of the rate hierarchy.
--   Something like "/root/outgoing/normal-price/mobile-line".
type CompleteUserRateRefName = Text.Text

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

type ReverseRatePath = [RateSystemId]

-- | A list of RateSystemId, from child to parent, following
--   the reversed order used usually during rating, with
--   the applied CalcParams.
type ReverseRatePathAndCalcParams = [(RateSystemId, Text.Text, CalcParams)]

-- | The most specific RateSystemId
reverseRatePath_specificId :: ReverseRatePath -> RateSystemId
reverseRatePath_specificId = List.head

-- | The most specific RateSystemId
reverseRatePathAndCalcParams_specificId :: ReverseRatePathAndCalcParams -> RateSystemId
reverseRatePathAndCalcParams_specificId x
  = let (r, _, _) = List.head x
    in r

-- | A reference to an internal rate.
type InternalRateReference = Text.Text

-- | A compact and efficient, system generated reference to a parto of a RatePlan.
type RateSystemId = Int

type NormalRateSystemId = RateSystemId

type BundleRateSystemId = RateSystemId

-- | A complete rate plan.
--   DEV NOTE: "MatchFun" is the trick used for allowing extensions to every type of rate plan/method. For example a CSV based rate, store rows in an ad-hoc data structure, read from the MatchFun method.
data MainRatePlan
  = MainRatePlan {
      mainRatePlan_bundleRates :: [RootBundleRatePlan]
    , mainRatePlan_normalRates :: [RatePlan]
    , mainRatePlan_systemIdToRootBundleRate :: MapToRootBundleRate
    , mainRatePlan_systemIdToUserIdPath :: RatePlanIdMap Text.Text
    , mainRatePlan_maxSystemId :: RateSystemId
    } deriving (Show, Generic, NFData)

-- | Return the calldate from which you can start with a empty BundleState, because it is the init of bundle timeframe.
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
    , mainRatePlan_systemIdToRootBundleRate = IMap.empty
    , mainRatePlan_maxSystemId = 0
    }

-- | The root/main BundleRatePlan.
--   It is distinct from BundleRatePlan, because it can have some initial params,
--   that can not be changed from children rates.
data RootBundleRatePlan
  = RootBundleRatePlan {
      bundle_serviceCDRType :: Text.Text
    , bundle_serviceCDRDescription :: Text.Text
    , bundle_timeFrame:: BundleRateTimeFrame
    , bundle_priceCategoryIds :: [PriceCategoryId]
    , bundle_limitsAreProportionalsToActivationDate :: Bool
    , bundle_canSplit :: Bool
    -- ^ True if a call with duration greather than bundle left duration,
    --   can be split in a call with a part rated according the bundle, and
    --   another part rated outside the bundle.
    , bundle_onlyForCallsWithACost :: Bool
    -- ^ true for applying the bundle only to calls with a cost. Other calls will not change the bundle limits.
    , bundle_plan :: BundleRatePlan
  } deriving(Show, Generic, NFData)

-- | A bundle rate plan, with children.
data BundleRatePlan
  = BundleRatePlan {
      bundle_systemId :: RateSystemId
    , bundle_rateParams :: RateParams
    , bundle_bundleParams :: BundleParams
    , bundle_children :: Either ExternalRateReference [BundleRatePlan]
    } deriving(Show, Generic, NFData)

-- | BundleParams that can be changed from any root and children bundle-rate.
data BundleParams
  = BundleParams {
      bundle_initialCost :: ! MonetaryValue
    , bundle_leftCalls :: !(Maybe Int)
      -- ^ how many calls can be processed from the bundle.
      --   Nothing if this is not a limit.
    , bundle_leftDuration :: !(Maybe Int)
      -- ^ how many seconds can be processed from the bundle.
      --   Nothing if this is not a limit.
    , bundle_leftCost :: !(Maybe MonetaryValue)
      -- ^ the bundle can accomodate calls with this cost.
      --   Nothing if this is not a limit.
      --   DEV-NOTE: not yet supported
     , bundle_appliedCost :: !MonetaryValue
      -- ^ the cost of calls associated to the BundleRate.
  } deriving(Show, Generic, NFData)

-- | RatePlan specification.
data RatePlan
  = RatePlan {
      rate_systemId :: RateSystemId
    , rate_params :: RateParams
    , rate_elsePart :: [RatePlan]
    , rate_children :: Either ExternalRateReference [RatePlan]
    } deriving(Show, Generic, NFData)

-- | The params of a normal rate-plan.
data RateParams
  = RateParams {
      rate_userId :: Text.Text
    -- ^ user assigned human readable short name/id
    , rate_match :: MatchFun
  } deriving(Show, Generic, NFData)

-- | The params are the initial limits of the BundleRatePlan, at the beginning of the time-frame.
--   A BundleRate can process all the calls in the TimeFrame.
type BundleRecord = (BundleParams, TimeFrame)

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

-- | The part of the call that can be rated using the bundle rate.
type BundleCallDuration = Int

-- | The part of the call that can not be rated using the bundle rate.
type ResidualCallDuration = Int

-- | A CallDuration comprising the bundle and residual part.
type CallDuration = (BundleCallDuration, ResidualCallDuration)

-- | A Map associating a Organization unitId with some value.
type UnitIdMap a = IMap.IntMap a

-- | The UnitId for which is applicable the BundleRate, and for which must be decreased the BundleRate limits.
--   It can be different from the UnitId associated to the CDR, but it is in any case a parent organization of it.
type BundleRateUnitId = UnitId

-- | A rate that was not applied to a CDR, with the reason.
type DiscardedRate = ( Text.Text -- ^ the name of the rate
                     , Text.Text -- ^ the reason of discard
                     )

-- | Used internally in the code for selecting the current best normal rate.
type SelectedRate = (MatchStrenght, ReverseRatePathAndCalcParams)

-- | Used internally in the code for selecting the best BundleRate.
type SelectedBundleRate = (MatchStrenght, ReverseRatePathAndCalcParams, CallDuration)

-- | Used internally in the code for selecting the current best rate.
type SelectedRateOrError = Either AsterisellError (Maybe SelectedRate, [DiscardedRate])

type SelectedBundleRateOrError = Either AsterisellError (Maybe (SelectedBundleRate, BundleRateUnitId), [DiscardedRate])

-- | True for normal cdrs, False for ServiceCDR.
type IsNormalCdr = Bool

instance DBS.Serialize BundleParams

rootBundleRatePlan_userId :: RootBundleRatePlan -> Text.Text
rootBundleRatePlan_userId p = rate_userId $ bundle_rateParams $ bundle_plan p

-- | Add support for binary serialization to LocalTime.
instance DBS.Serialize CallDate where
  put l
    = do DBS.put $ show l

  get
    = do s :: String <- DBS.get
         return $ read s

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

-- | Assign a unique RateSystemId to all rates, and complete also other derived info.
--   NOTE: this code at every execution had to assign the same ID to the same bundle state rate parts,
--   in case the main-rate-plan is not changed. In case of changes of the rate-plan, the bundle-state
--   is regenerated from scratch and so this constraint does not hold.
mainRatePlan_assignUniqueSystemId
  :: MainRatePlan
  -> Either String MainRatePlan

mainRatePlan_assignUniqueSystemId mainPlan1
  = let (mainPlan2, maxId) = runState (processMainRatePlan mainPlan1) 1
        mainPlan3 = mainPlan2 { mainRatePlan_maxSystemId = maxId }
    in  mainRatePlan_completeDerivedInfo mainPlan3

 where

  processMainRatePlan :: MainRatePlan -> State Int MainRatePlan
  processMainRatePlan p
    = do br <- mapM processRootBundleRatePlan (mainRatePlan_bundleRates p)
         nr <- mapM processNormalRate (mainRatePlan_normalRates p)
         return $ mainRatePlan_empty {
                    mainRatePlan_bundleRates = br
                  , mainRatePlan_normalRates = nr
                  }

  processRootBundleRatePlan :: RootBundleRatePlan -> State Int RootBundleRatePlan
  processRootBundleRatePlan p
    = do br <- processBundleRatePlan (bundle_plan p)
         return $ p { bundle_plan = br }

  processBundleRatePlan :: BundleRatePlan -> State Int BundleRatePlan
  processBundleRatePlan p
    = do i <- get
         put (i + 1)
         chs <- case bundle_children p of
                  Left e
                    -> return $ Left e
                  Right ch
                    -> do chs1 <- mapM processBundleRatePlan ch
                          return $ Right chs1

         return $ p {   bundle_systemId = i
                      , bundle_children = chs
                    }

  processNormalRate :: RatePlan -> State Int RatePlan
  processNormalRate p
    = do i <- get
         put (i + 1)
         chs <- case rate_children p of
                  Left e
                    -> return $ Left e
                  Right ch
                    -> do chs1 <- mapM processNormalRate ch
                          return $ Right chs1

         elsePart <- mapM processNormalRate (rate_elsePart p)
         return $ p { rate_systemId = i
                    , rate_children = chs
                    , rate_elsePart = elsePart
                    }

mainRatePlan_completeDerivedInfo
  :: MainRatePlan
     -- ^ @require mainRatePlan_assignUniqueSystemId is called, and it have unique system id.
  -> Either String MainRatePlan
mainRatePlan_completeDerivedInfo mainPlan1
  =  let mainPlan2 = mainPlan1 { mainRatePlan_systemIdToRootBundleRate = mainRatePlan_deriveMapFromChildrenRateToMainRate mainPlan1 }
     in  case mainRatePlan_mapSystemIdToUserIdPath mainPlan2 of
           Left err
             -> Left err
           Right systemIdToNameOrError
             -> Right $ mainPlan2 { mainRatePlan_systemIdToUserIdPath = systemIdToNameOrError }

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
         mapM_ (processNormalRate n) (mainRatePlan_normalRates p)

  processRootBundleRatePlan n p
    = do processBundleRatePlan n (bundle_plan p)

  processBundleRatePlan n p
    = do let name = Text.concat [n, "/", rate_userId $ bundle_rateParams p]
         insertUniqueValue (bundle_systemId p) name
         case bundle_children p of
           Left _
             -> return ()
           Right children
             -> mapM_ (processBundleRatePlan name) children

  processNormalRate n p
    = do let name = Text.concat [n, "/", rate_userId $ rate_params p]
         insertUniqueValue (rate_systemId p) name
         case rate_children p of
           Left _
             -> return ()
           Right children
             -> mapM_ (processNormalRate name) children
         mapM_ (processNormalRate n) $ rate_elsePart p
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

data RatingDebugInfo
  = RatingDebugInfo {
      info_appliedRate :: RateSystemId
    -- ^ debug info: applied rate
    , info_residualAppliedRate :: Maybe RateSystemId
    -- ^ residual applied rate
    , info_residualCallDuration :: Maybe ResidualCallDuration
    -- ^ residual call duration
    , info_bundleRateSystemId :: Maybe RateSystemId
    , info_ratingDetails :: Maybe Text.Text
    -- ^ detailed info on the rating method used, generated only in case of debug-jobs processing, because it uses many resources
    } deriving(Show)

ratingDebugInfo_empty :: RateSystemId -> Maybe RateSystemId -> RatingDebugInfo
ratingDebugInfo_empty appliedRate bundleRateId
  = RatingDebugInfo {
      info_appliedRate = appliedRate
    , info_residualAppliedRate = Nothing
    , info_residualCallDuration = Nothing
    , info_bundleRateSystemId = bundleRateId
    , info_ratingDetails = Nothing
    }

-- | Describe the discarded rates
discardedRates_toDetails :: [DiscardedRate] -> Text.Text
discardedRates_toDetails drs
  = Text.concat $ List.concatMap (\(n, reason) -> ["\nThe rate ", n, " was not applied because ", reason]) drs

-- | Calculate the cost of the call, updating the BundleState.
--
--   The algo is composed of two phases:
--   * select the rate to apply
--   * apply the rate
--
--   The bundle-state is applied only for normal-cdrs: service CDRS are not added to BundleState.
--
--   @require: bundleState1 is correctly initializated according the time-frame
mainRatePlan_calcCost
  :: Bool
     -- ^ True if it must generate detailed debug info
  -> RateRole
  -> RatingParams
  -> BundleState
  -> CheckedMainRatePlan
     -- ^ the main income or cost rate.
     --   NOTE: cost rates have no associated bundle, so this code will not process it because it is missing
  -> CDR
  -- ^ the call to rate
  -> Either AsterisellError
            ( MonetaryValue
              -- ^ the cost/income of the CDR
            , Maybe BundleRateUnitId
              -- ^ in case of BundleRate application, the used UnitId
            , BundleState
              -- ^ the new BundleState after processing the CDR
            , Maybe RatingDebugInfo
            )

mainRatePlan_calcCost isDebug rateRole env bundleState1 mainRatePlan cdr
  = case cdr_isServiceCDR cdr of
      True
        -> normalResult []
           -- a service CDR can be rated, because they can be imported from providers, but it is never associated to a bundle-state.
           -- So use normal rates.
      False
        -> case normalCost of
            Left err
              -> Left err
                 -- in any case it must have a normal cost in case te bundle is not applicable, so signal the error
            Right _
              -> case selectBundleRate of
                   Left err
                     -> Left err
                   Right (Just (selectedBundleRate@(_, ratePath, (bundleDuration, residualDuration)), bundleRateUnitId), unselectedRates)
                     -> let (bundleCost, finalBundleState) = applyBundleRate selectedBundleRate bundleRateUnitId
                            appliedRate = rateSystemId ratePath
                            info1 = createDebugInfo appliedRate (Just appliedRate) unselectedRates
                        in case residualDuration of
                             0 -> Right (bundleCost, Just bundleRateUnitId, finalBundleState, info1)
                             _ -> case selectNormalRate of
                                    Left err
                                      -> Left err
                                    Right (Just ((s, residualRatePath, _), _), unselectedRates2)
                                      -> let 
                                             info3 = case info1 of
                                                       Nothing -> Nothing
                                                       Just info1' -> let info2 = info1' { info_residualAppliedRate = Just $ rateSystemId residualRatePath }
                                                                      in addToDebugInfo info2 unselectedRates2
                                         in Right (bundleCost + (applyNormalRate (s, residualRatePath) (Just residualDuration)), Just bundleRateUnitId, finalBundleState, info3)
                   Right (Nothing, unselectedRates)
                     -> normalResult unselectedRates
                        -- DEV NOTE: reuse the already calculated value, for bundle_onlyForCallsWithACost

 where

  callDate = cdr_calldate cdr

  callBillsec = fromJust1 "rp1" $ cdr_billsec cdr

  organizationsInfo = params_organizations env

  cdrOrganizationUnitId = fromJust1 "rp1" $ cdr_organizationUnitId cdr

  normalResult unselectedRates1
    = case normalCost of
        Left err
          -> Left err
        Right (cost, bundleState, selectedRatePath, unselectedRates2)
          -> Right ( cost
                   , Nothing
                   , bundleState
                   , createDebugInfo (rateSystemId selectedRatePath) Nothing (appendIfDebug isDebug unselectedRates1 unselectedRates2)
                   )

  normalCost
    = case selectNormalRate of
        Left err
          -> Left err
        Right (Just ((s, selectedRatePath, _), _), unselectedRates2)
          -> Right (applyNormalRate (s, selectedRatePath) Nothing
                   , bundleState1
                   , selectedRatePath
                   , unselectedRates2
                   )
  {-# INLINE normalCost #-}

  hasNormalCost
    = case normalCost of
        Left _ -> False
        Right (m, _, _, _) -> m > 0
  {-# INLINE hasNormalCost #-}

  createRateDetails :: [DiscardedRate] -> Maybe Text.Text
  createRateDetails dsr
    = if isDebug then (Just $ discardedRates_toDetails dsr) else Nothing
  {-# INLINE createRateDetails #-}

  createDebugInfo :: RateSystemId -> Maybe RateSystemId -> [DiscardedRate] -> Maybe RatingDebugInfo
  createDebugInfo sId msId dsr
    = if isDebug then (Just $ (ratingDebugInfo_empty sId msId) { info_ratingDetails = createRateDetails dsr }) else Nothing
  {-# INLINE createDebugInfo #-}

  addToDebugInfo :: RatingDebugInfo -> [DiscardedRate] -> Maybe RatingDebugInfo
  addToDebugInfo info dsr
    = if isDebug
      then let details1 = info_ratingDetails info
               details2 = createRateDetails dsr
               details3 = case (details1, details2) of
                            (Nothing, Nothing) -> Nothing
                            (Just s1, Nothing) -> Just s1
                            (Just s1, Just s2) -> Just (Text.append s1 s2)
                            (Nothing, Just s2) -> Just s2
                                                  
           in Just $ info { info_ratingDetails = details3 }
      else Nothing
  {-# INLINE addToDebugInfo #-}

  -- | Return the first RateSystemId that is not 0.
  --   There can be RateSystemId at 0 because they are the referenced external rates.
  --   @require not $ List.null n
  rateSystemId :: ReverseRatePathAndCalcParams -> RateSystemId
  rateSystemId n@((i, _, _):rest) = if (i == 0) then rateSystemId rest else i
  {-# INLINE rateSystemId #-}

  -- | Given a Cdr return some of its key values, for reducing the number of errors.
  --   Requirements:
  --   * not all CDRS generate a unique key
  --   * there are enough error signalation for each type of error
  cdr_missingRateKey :: CDR -> String
  cdr_missingRateKey cdr
    = let
          add4 :: (CDR -> Maybe Text.Text) -> Text.Text
          add4 f = Text.take 4 $ fromJust1 "rp2" $ f cdr

          add :: (CDR -> Maybe Text.Text) -> Text.Text
          add f = fromJust1 "rp3" $ f cdr

          addI :: (CDR -> Maybe Int) -> Text.Text
          addI f = Text.pack $ show $ fromJust1 "rp4" $ f cdr

          mAppend :: [Text.Text] -> Text.Text
          mAppend l = List.foldl' (\r t -> Text.append r t) Text.empty l

      in  Text.unpack $ mAppend ((addI cdr_communicationChannelTypeId):(add4 cdr_externalTelephoneNumberWithAppliedPortability):(Text.pack $ show $ cdr_direction cdr):(addI cdr_priceCategoryId):[])
  {-# INLINE cdr_missingRateKey #-}

  -- | Select a BundleRate for the CDR organization, or one of its parent organizations.
  --   This because BundleRate have greather priority respect normal rates.
  --   The algo high level is this:
  --   * select the root bundle associated to the price category of the extension
  --   * try to match
  --   * if no match, then try with the parent organization owning the extension, that can have a different bundle category
  --   Try to match with the deepest bundle rate in the hierarchy.
  selectBundleRate :: SelectedBundleRateOrError
  selectBundleRate
    = f cdrOrganizationUnitId
   where

     f unitId
       = case selectBundleRateForUnitId unitId of
           Left err
             -> Left err
           Right (Just solution, unselectedRates1)
             -> Right (Just solution, unselectedRates1)
           Right (Nothing, unselectedRates1)
             -> let dataInfo = fromJust1 "rp5" $ info_getDataInfoForUnitId organizationsInfo unitId callDate True
                in case info_getDataInfoParent organizationsInfo dataInfo callDate True of
                  Nothing
                    -> Right (Nothing, unselectedRates1)
                  Just parentInfo
                    -> case f (unit_id parentInfo) of
                         Left err
                           -> Left err
                         Right (maybeResult, unselectedRates2)
                           -> Right (maybeResult, appendIfDebug isDebug unselectedRates1 unselectedRates2)

  -- | Select the BundleRate associated to a BundleRateUnitId, that can be a parent of the CDR UnitId.
  selectBundleRateForUnitId :: BundleRateUnitId -> SelectedBundleRateOrError
  selectBundleRateForUnitId bundleRateUnitId
    = case info_getDirectPriceCategory organizationsInfo bundleRateUnitId callDate of
        Nothing
          -> Right (Nothing, [("all bundle rates", Text.concat ["the organization ", Text.pack $ show bundleRateUnitId, " has no direct price category assignation."])])
        Just priceCategoryId
          -> case findBundle priceCategoryId of
              Right Nothing
                -> Right (Nothing, [("all bundle rates", Text.append "there is no bundle rate with price category " (Text.pack $ show priceCategoryId))])
              Left err
                -> Left err
              Right (Just bundle)
                -> selectInsideMainBundleRateForUnitId bundle bundleRateUnitId priceCategoryId

   where

     findBundle :: PriceCategoryId -> Either AsterisellError (Maybe RootBundleRatePlan)
     findBundle priceCategoryId
       = case List.filter (\b -> List.elem priceCategoryId (bundle_priceCategoryIds b)) (mainRatePlan_bundleRates mainRatePlan) of
           [] -> Right Nothing
           [r] -> Right (Just r)
           _ -> let priceCategoryName
                      = rateCategories_code (params_rateCategories env) priceCategoryId
                in Left $ createError
                            Type_Error
                            Domain_RATES
                            ("conflicting bundle rates on price category - " ++ show rateRole ++ "-" ++ show priceCategoryId)
                            ("There is more than one " ++ show rateRole ++ " bundle rate on price-category \"" ++ Text.unpack priceCategoryName ++ "\", during rating of CDR " ++ rate_showDebug env cdr)
                            ("All similar CDRs can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                            ("Correct the rate specification, or contact the assistance.")

  selectInsideMainBundleRateForUnitId :: RootBundleRatePlan -> BundleRateUnitId -> PriceCategoryId -> SelectedBundleRateOrError
  selectInsideMainBundleRateForUnitId mainPlan bundleRateUnitId priceCategoryId
    = case (bundle_onlyForCallsWithACost mainPlan && (not hasNormalCost)) of
        True
          -> Right (Nothing, [(rootBundleRateUserName, "the call has cost 0")])

        False
          -> case selectCurrentRateOrBestChild env bundleState1 rateRole isDebug cdr (matchStrenght_initial, [], (callBillsec, 0)) (Just $ Left (mainPlan, bundleRateUnitId, bundle_plan mainPlan)) children of
               Left err
                 -> Left err
               Right (Nothing, unselectedRates)
                 -> Right (Nothing, unselectedRates)
               Right (Just r, unselectedRates)
                 -> Right $ (Just r, unselectedRates)

   where

     rootBundleRateUserName = Text.append "/" (rootBundleRatePlan_userId mainPlan)
     children = case bundle_children $ bundle_plan mainPlan of
                  Left r -> Left r
                  Right r -> Right $ List.map Left r

  selectNormalRate :: SelectedBundleRateOrError
  selectNormalRate
    = case selectCurrentRateOrBestChild env bundleState1 rateRole isDebug cdr (matchStrenght_initial, [], (callBillsec, 0)) Nothing (Right $ List.map Right $ mainRatePlan_normalRates mainRatePlan) of
         Right (Nothing, unselectedRates)
          -> Left $ createError
                      Type_Error
                      Domain_RATES
                      ("missing rate for - " ++ show rateRole ++ "-" ++ (cdr_missingRateKey cdr))
                      ("There is no " ++ show rateRole ++ " (no-bundle) rate for rating CDRs like " ++ rate_showDebug env cdr ++ "\n\n" ++ (Text.unpack $ discardedRates_toDetails unselectedRates))
                      ("All similar CDRs can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                      ("Correct the rate specification, or contact the assistance.")
         r -> r

  -- | Apply the BundleRate on the current cdr.
  applyBundleRate
    :: SelectedBundleRate
    -> BundleRateUnitId
    -> ( MonetaryValue
        -- ^ the cost/income of the CDR
        , BundleState
        -- ^ the new BundleState after processing the CDR
       )

  applyBundleRate (m, reverseIdsAndCalcParams, (bundleDuration, _)) unitId
    = (cost, state3)

   where

    cost = applyNormalRate (m, reverseIdsAndCalcParams) (Just bundleDuration)

    rateId = let (x, _, _) = List.head reverseIdsAndCalcParams
             in x

    state3 = bundleState_update bundleState1 rateId unitId (cdr_countOfCalls cdr) bundleDuration cost

  -- | Apply the rate on the current cdr.
  applyNormalRate
    :: SelectedRate
    -> Maybe ResidualCallDuration
    -- ^ in case of residual call, use this instead of the CDR billsec
    -> MonetaryValue

  applyNormalRate (_, paramsAndIds) maybeResidualCallDuration
    = cost
   where

     applicableDuration
       = case maybeResidualCallDuration of
           Just v -> v
           Nothing -> fromJust1 "rp6" $ cdr_billsec cdr

     params = List.map (\(_, _, x) -> x) (List.reverse paramsAndIds)

     calcParams = calcParams_overrideList params

     cost = calcParams_calc rateRole calcParams (cdr { cdr_billsec = Just applicableDuration })

-- | Select the specified rate if applicable, or the best of its children, in a recursive way.
--   This function has "strange" parameters, because in this way I can reuse in many points of the code,
--   both for bundle rates and normal rats, and in recursive calls.
selectCurrentRateOrBestChild
   :: RatingParams
   -> BundleState
   -> RateRole
   -> Bool
   -> CDR
   -> (MatchStrenght, ReverseRatePathAndCalcParams, CallDuration)
      -- ^ the parent path. So this does not comprise current rate
   -> Maybe (Either (RootBundleRatePlan, BundleRateUnitId, BundleRatePlan) RatePlan)
      -- ^ the current rate to evaluate. Nothing for root rate.
   -> Either ExternalRateReference [Either BundleRatePlan RatePlan]
      -- ^ the children of the current rate to evaluate
      -- @require children are the children of maybeCurrentRate
      -- @require if maybeCurrentRate is Nothing, then this is the list of all root rates that are non-bundles
   -> SelectedBundleRateOrError
      -- ^ the best inner-most selected rate

selectCurrentRateOrBestChild env bundleState1 rateRole isDebug cdr (parentStrenght, parentPath, callDuration) maybeCurrentRate currentChildren
   = case maybeCurrentRate of
       Nothing
         -> selectCurrentRateOrBestChild1 True (parentStrenght, parentPath, callDuration) currentChildren
            -- call this because it is for sure applicable (no rate), and so starts immediately choosing the best child.
       Just currentBundleOrNormalPlan
         -> -- test if the current rate is applicable (CDR respect params) and in case search for best children.
            let (currentRateName, currentRateUserId, currentRateSystemId)
                  = case currentBundleOrNormalPlan of
                      Right currentPlan
                        -> let userId = rate_userId $ rate_params currentPlan
                               systemId = rate_systemId currentPlan
                           in  (Text.concat [ratePathName parentPath, "/", userId], userId, systemId)
                      Left (mainRatePlan, unitId, currentPlan)
                        -> let userId = rate_userId $ bundle_rateParams currentPlan
                               systemId = bundle_systemId currentPlan
                           in  (Text.concat [ratePathName parentPath, "/", userId], userId, systemId)

                currentParams
                  = case currentBundleOrNormalPlan of
                      Right currentPlan
                        -> rate_params currentPlan
                      Left (mainRatePlan, unitId, currentPlan)
                        -> bundle_rateParams currentPlan

                -- | Nothing if the BundleRate is not applicable because it does not respect the limits.
                maybeBundleCallDuration :: Maybe CallDuration
                maybeBundleCallDuration
                  = case currentBundleOrNormalPlan of
                      Right _
                        -> Just callDuration
                      Left (mainPlan, bundleRateUnitId, currentPlan)
                        -> case bundle_canBeApplied mainPlan currentPlan bundleState1 (cdr, bundleRateUnitId, fst callDuration) of
                             Nothing -> Nothing
                             Just d -> Just (d, callBillsec - d)

                elseParts
                  = case currentBundleOrNormalPlan of
                      Right r -> List.map Right $ rate_elsePart r
                      Left _ -> []

            in case (maybeBundleCallDuration, rate_match currentParams env cdr) of
                 (Nothing, _)
                   -> Right (Nothing, [(currentRateName, "there are no sufficient left resources in the bundle state, associated to the CDR extension")])
                 (Just _, Nothing)
                   -> let unselectedRates1 = [(currentRateName, "params are not respected")]
                          -- if the parent rate is not respected, then children are not checked, and only the else part is checked
                      in case List.null elseParts of
                           True
                             -> Right (Nothing, unselectedRates1)

                           False
                             -> case selectCurrentRateOrBestChild env bundleState1 rateRole isDebug cdr(parentStrenght, parentPath, callDuration) Nothing (Right elseParts) of
                                  Left err
                                    -> Left err
                                  Right (Nothing, unselectedRates2)
                                    -> Right (Nothing, appendIfDebug isDebug unselectedRates1 unselectedRates2)
                                  Right (Just finalResult, unselectedRates2)
                                    -> Right (Just finalResult, appendIfDebug isDebug unselectedRates1 unselectedRates2)

                 (Just bundleCallDuration, Just (currentStrenght1, currentCalcParams1))
                   -> let currentStrenght = match_combineWithChild currentStrenght1 parentStrenght
                          currentPath = (currentRateSystemId, currentRateUserId, currentCalcParams1):parentPath
                      in  case selectCurrentRateOrBestChild1 True (currentStrenght, currentPath, bundleCallDuration) currentChildren of
                            Right (Nothing, unselectedRates1)
                              -> -- in this case a nested external reference rate can be failed, so we must manage the elsePart
                                 case List.null elseParts of
                                   True
                                     -> Right (Nothing, unselectedRates1)

                                   False
                                     -> case selectCurrentRateOrBestChild env bundleState1 rateRole isDebug cdr (parentStrenght, parentPath, callDuration) Nothing (Right elseParts) of
                                          Left err
                                            -> Left err
                                          Right (Nothing, unselectedRates2)
                                            -> Right (Nothing, appendIfDebug isDebug unselectedRates1 unselectedRates2)
                                          Right (Just finalResult, unselectedRates2)
                                            -> Right (Just finalResult, appendIfDebug isDebug unselectedRates1 unselectedRates2)

                            errorOrResult -> errorOrResult
  where

    callBillsec = fromJust1 "rp1" $ cdr_billsec cdr

    callDate = cdr_calldate cdr

    rootUnitId
      = case maybeCurrentRate of
          Nothing
            -> fromJust1 "err 724454" $ cdr_organizationUnitId cdr
          Just (Left (_, r, _))
            -> r
          Just (Right _)
            -> fromJust1 "err 724455" $ cdr_organizationUnitId cdr

    rootBundleRate :: Maybe RootBundleRatePlan
    rootBundleRate = case maybeCurrentRate of
                       Just (Left (a, _, _)) -> Just a
                       _ -> Nothing

    -- | Given an ExternalRateReference, retrieve the external children.
    getExternalChildren :: ExternalRateReference -> Either AsterisellError [RatePlan]
    getExternalChildren externalRateReference
      = case env_getRate env externalRateReference callDate of
          Nothing
            -> Left $ createError
                        Type_Error
                        Domain_RATES
                        ("unknown external rate name - " ++ show rateRole ++ "-" ++ Text.unpack externalRateReference)
                        ("The " ++ show rateRole ++ " external rate reference \"" ++ Text.unpack externalRateReference ++ "\", does not exists at date " ++ showLocalTime callDate ++ ". The CDR content is " ++ rate_showDebug env cdr)
                        ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                        ("Correct the rate specification, or contact the assistance.")

          Just (mainRatePlan, _)
            -> case List.null (mainRatePlan_bundleRates mainRatePlan) of
                 False
                   -> Left $ createError
                               Type_Error
                               Domain_RATES
                               ("external rate name with bundle params - " ++ show rateRole ++ "-" ++ Text.unpack externalRateReference)
                               ("The " ++ show rateRole ++ " external rate reference \"" ++ Text.unpack externalRateReference ++ "\", exists at date " ++ showLocalTime callDate ++ ", but it has bundle children, and it can not be embeded correctly in a rate plan.")
                               ("CDRS can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                               ("Correct the rate specification, or contact the assistance.")
                 True
                   -> Right $ mainRatePlan_normalRates mainRatePlan

    -- | Search the best child, assuming that the current rate is applicable.
    selectCurrentRateOrBestChild1
      :: Bool
         -- ^ True if we are evaluating a normal rate, False if we are evaluating an external referenced rate
      -> SelectedBundleRate
          -- ^ the current rate strenght, and path, before processing the child
      -> Either ExternalRateReference [Either BundleRatePlan RatePlan]
         -- ^ the children of the current rate
      -> SelectedBundleRateOrError
         -- ^ the best child, with all the info completed, also respect parent path

    selectCurrentRateOrBestChild1 isNormalRate (currentStrenght, currentPath, callDuration) currentChildren
      = case currentChildren of
          Left externalRateRef
            -> case getExternalChildren externalRateRef of
                 Left err
                   -> Left err
                 Right children
                   -> selectCurrentRateOrBestChild1 False (currentStrenght, currentPath, callDuration) (Right $ List.map Right children)
          Right []
            -> if isNormalRate
               then (Right $ (Just ((currentStrenght, currentPath, callDuration), rootUnitId), []))
                    -- if there are no children, then the current rate is already applicable and the virtual best child
               else (Right (Nothing, []))
                    -- in case of external reference rate, there should be at least a child for testing applicability
          Right children
            -> let candidateChildren :: [SelectedBundleRateOrError]
                     = List.map (\child
                                    -> let cs :: Either ExternalRateReference [Either BundleRatePlan RatePlan]
                                           cs
                                             = case child of
                                                  Left a
                                                    -> case bundle_children a of
                                                         Left b -> Left b
                                                         Right b -> Right $ List.map Left b
                                                  Right a
                                                    -> case rate_children a of
                                                         Left b -> Left b
                                                         Right b -> Right $ List.map Right b
                                           c :: (Either (RootBundleRatePlan, BundleRateUnitId, BundleRatePlan) RatePlan)
                                           c = case child of
                                                 Left a
                                                   -> Left (fromJust1 "err 6667755" $ rootBundleRate, rootUnitId, a)
                                                 Right a
                                                   -> Right a
                                       in  selectCurrentRateOrBestChild env bundleState1 rateRole isDebug cdr (matchStrenght_initial, currentPath, callDuration) (Just c) cs
                                           -- DEV-NOTE: I'm using a 0 strenght, because I want select the best matching child, before combining with the strenght of current rate
                                ) children


                   selectChild :: SelectedBundleRateOrError -> SelectedBundleRateOrError -> SelectedBundleRateOrError
                   selectChild (Left err1) (Left err2) = (Left err1)
                   selectChild (Left err1) (Right _) = (Left err1)
                   selectChild (Right _) (Left err2) = (Left err2)
                   selectChild (Right (Nothing, unselectedRates1)) (Right (result2, unselectedRates2))
                     = Right (result2, appendIfDebug isDebug unselectedRates1 unselectedRates2)
                   selectChild (Right (result1, unselectedRates1)) (Right (Nothing, unselectedRates2))
                     = Right (result1, appendIfDebug isDebug unselectedRates1 unselectedRates2)
                   selectChild (Right (Just ((oldStrenght, oldPath, oldDuration), unitId), unselectedRates1)) (Right (Just ((newStrenght, newPath, newDuration), _), unselectedRates2))
                     = let unselectedRates3 = appendIfDebug isDebug unselectedRates1 unselectedRates2
                       in case match_ord oldStrenght newStrenght of
                            Just EQ
                              -> let name1 = ratePathName oldPath
                                     name2 = ratePathName newPath
                                 in Left $ createError
                                             Type_Error
                                             Domain_RATES
                                             ("virtual rate conflict - " ++ show rateRole ++ "-" ++ Text.unpack name1 ++ "-" ++ Text.unpack name2)
                                             ("The " ++ show rateRole ++ " rate \"" ++ Text.unpack name1 ++ "\", conflicts with rate \"" ++ Text.unpack name2 ++ "\", because it matches the CDR with the same strenght, and the system have no hints about which is the best rate to apply. The CDR has calldate is " ++ showLocalTime (cdr_calldate cdr) ++ ". The CDR content, after import, and initial processing is " ++ rate_showDebug env cdr)
                                             ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")                         ("Correct the rate specification, or contact the assistance.")

                            Just LT
                              -> Right (Just ((newStrenght, newPath, newDuration), unitId), unselectedRates3)
                            Just GT
                              -> Right (Just ((oldStrenght, oldPath, oldDuration), unitId), unselectedRates3)

               in case List.foldl' selectChild (Right (Nothing, [])) candidateChildren of
                    Left err
                      -> Left err
                    Right (Nothing, unselectedRates)
                      -> let rateName = ratePathName currentPath
                             (errorMessage1, errorMessage2)
                               = if (List.null currentPath)
                                 then (("There is no applicable " ++ show rateRole ++ " rate "), "")
                                 else (("The " ++ show rateRole ++ " rate " ++ Text.unpack rateName ++ " is applicable ")
                                      ,("\n\nbut none of its more specific children rates can be applied.\nFor avoiding ambiguites in rating specifications, one and only one children rate must be selected, but in this case none can be selected.\n\n"))

                         in if isNormalRate
                            then (Left $ createError
                                           Type_Error
                                           Domain_RATES
                                           ("not applicale children rate - " ++ show rateRole ++ "-" ++ Text.unpack rateName)
                                           (errorMessage1 ++ " to a CDR like this: \n" ++ rate_showDebug env cdr ++ errorMessage2)
                                           ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                                           ("Improve the rate specification."))
                            else (Right (Nothing, unselectedRates))

                    Right (Just ((bestChildrenStrenght, bestChildrenPath, bestDuration), unitId), unselectedRates)
                      -> Right (Just ((match_combineWithChild currentStrenght bestChildrenStrenght, bestChildrenPath, bestDuration), unitId), unselectedRates)

-- | The rate name to use for debug info.
ratePathName :: ReverseRatePathAndCalcParams -> Text.Text
ratePathName rs
  = Text.concat $ List.concatMap (\(_,n, _) -> ["/", n]) (List.reverse rs)
{-# INLINE ratePathName #-}

-- | Reduce the load on the system, collecting info only in debug mode. Haskell lazy feature, can reduce the needing for this,
--   but I'm using it as safety measure.
appendIfDebug :: Bool -> [DiscardedRate] -> [DiscardedRate] -> [DiscardedRate]
appendIfDebug isDebug a b = if isDebug then (a ++ b) else []
{-# INLINE appendIfDebug #-}

debug_showRateSystemIds :: [RatePlan] -> String
debug_showRateSystemIds rs = List.concat $ List.intersperse ", " $ List.map show $ List.map rate_systemId rs

-----------------------
-- BUNDLE RATE STATE --
-----------------------

-- | CallDate greather or equal to this value,
--   are to be considered in the next bundle-rate scheduling timeframe.
type NextScheduledLocalTime = CallDate

-- | A Map associating a rate_systemId with some value.
--   An IntMap is rather fast, because it is specialized for Int.
type RatePlanIdMap a = IMap.IntMap a

ratePlanIdMap_empty :: RatePlanIdMap a
ratePlanIdMap_empty = IMap.empty

type BundleStateByRatePlan = RatePlanIdMap (NextScheduledLocalTime, CompleteUserRateRefName, UnitIdMap BundleRecord)

-- | Store a state for each BundleRate, and for each UnitId.
--   The state is modified every time a CDR is processed.
type BundleState
       = (Maybe NextScheduledLocalTime
          -- ^ the (cached) minimum NextScheduledLocalTime of all RatePlanId
         , BundleStateByRatePlan)

bundleState_empty :: BundleState
bundleState_empty = (Nothing, IMap.empty)

-- | Used for activating the real BundleState, after the specified calldate.
bundleState_fake :: NextScheduledLocalTime -> BundleState
bundleState_fake callDate = (Just callDate, IMap.empty)

-- | A BundleState can be partitioned, for indipendent rating processing.
type BundlePartition
       = (Int
           -- ^ total partitions
         ,Int
           -- ^ this partition: from 0 to total partitions - 1
         )

-- | Create a single partition, containing all data.
bundlePartition_all :: BundlePartition
bundlePartition_all = (1, 0)

bundlePartition_contains :: BundlePartition -> Int -> Bool
bundlePartition_contains (totSplits, currSplit) toTest = mod toTest totSplits == currSplit
{-# INLINE bundlePartition_contains #-}

bundlePartition_containsUnitId :: RatingParams -> BundlePartition -> TimeFrame -> UnitId -> Bool
bundlePartition_containsUnitId env (totSplits, currSplit) (fromCallDate, toCallDate) unitId
    = let
          info = params_organizations env
          info1 = fromJust1 "ERR 7111" $ info_getDataInfoForUnitId info unitId fromCallDate False
          callDate1 = structure_from info1
          rootId = unit_id $ List.head $ info_getDataInfoParentHierarchy info info1 callDate1 True

          allRootIds :: [UnitId]
            = List.map (\di -> unit_id $ head $ info_getDataInfoParentHierarchy info di (structure_from di) True) $
                List.filter (\s -> structure_from s > callDate1 && structure_from s < toCallDate) $
                  fromJust1 "ERR 7112" $ IMap.lookup unitId (info_organizations info)

      in case List.all ((==) rootId) allRootIds of
           True -> bundlePartition_contains (totSplits, currSplit) rootId 
           False -> error $ "The organization " ++ Text.unpack (info_getParentHiearchyIds $ info_getDataInfoParentHierarchy info info1 callDate1 False) ++ " changes root parent organization in the billing time-frame from " ++ show fromCallDate ++ " to " ++ show toCallDate ++ ", but this can cause problems in case of bundle-rates because it is not clear how rating it, or how partition parallel rating processing. An organization can change its root parent organization only at the beginning of a new bundle-rate rating time-frame."


bundleState_partition :: RatingParams -> Int -> BundleState -> [BundleState]
bundleState_partition env totSplits (cachedCallDate, bundle)
  = List.map (splitBundle) [0.. (totSplits - 1)]
 where

  splitBundle :: Int -> BundleState
  splitBundle thisSplit
    = (cachedCallDate, IMap.map (\(x, y, unitIdMap) -> (x, y, splitUnidIdMap thisSplit unitIdMap)) bundle)

  splitUnidIdMap :: Int -> UnitIdMap BundleRecord -> UnitIdMap BundleRecord
  splitUnidIdMap thisSplit map1
    = IMap.filterWithKey (\unitId (p, bundleTimeFrame) -> bundlePartition_containsUnitId env (totSplits, thisSplit) bundleTimeFrame unitId) map1

-- | Join indipendent bundle-stats on a unique bundle-state.
--   @require all bundle-state have the same rate-plan-id, user-ref-name,
--   and differs only on the content of the UnitIdMap
--   @require there is at least a bundle-state
bundleState_join :: [BundleState] -> BundleState
bundleState_join (bundle:bundles)
  = List.foldl' joinBundles bundle bundles

 where

  joinBundles :: BundleState -> BundleState -> BundleState
  joinBundles (cachedDate1, bundle1) (cachedDate2, bundle2)
    = let cachedDate3 = min cachedDate1 cachedDate2
          bundle3
            = IMap.foldlWithKey'
                (\m1 rateId (t2, refName2, unitMap2)
                      -> case IMap.lookup rateId m1 of
                           Nothing
                             -> IMap.insert rateId (t2, refName2, unitMap2) m1
                           Just (t1, refName1, unitMap1)
                             -> IMap.insert
                                  rateId
                                  ( assert (t1 == t2) t1
                                  , assert (refName1 == refName2) refName1
                                  , assert (IMap.null $ IMap.intersection unitMap1 unitMap2) IMap.union unitMap1 unitMap2)
                                  m1)
                bundle1 bundle2

      in (cachedDate3, bundle3)

-- | Associate a BundleRate to its root/main rate, containing some fixed params.
type MapToRootBundleRate = RatePlanIdMap RootBundleRatePlan

-- | To call before starting rating, for creating a fast lookup structure.
mainRatePlan_deriveMapFromChildrenRateToMainRate :: MainRatePlan -> MapToRootBundleRate
mainRatePlan_deriveMapFromChildrenRateToMainRate mainRatePlan
  = result
 where

  bundleRates = mainRatePlan_bundleRates mainRatePlan

  result = List.foldl' processMainRate IMap.empty bundleRates

  processMainRate result1 mainRate
    = processBundleRate mainRate result1 (bundle_plan mainRate)

  processBundleRate :: RootBundleRatePlan -> MapToRootBundleRate ->  BundleRatePlan -> MapToRootBundleRate
  processBundleRate mainRate result1 rate
    = let rateId = bundle_systemId rate
          result2 = IMap.insert rateId mainRate result1
          result3
            = case bundle_children rate of
                Left _
                  -> result2
                Right children
                  -> List.foldl' (processBundleRate mainRate) result2 children
      in  result3

bundleState_updateCachedNextScheduledLocalTime :: BundleState -> BundleState
bundleState_updateCachedNextScheduledLocalTime (_, s)
  = let callDate
          = IMap.foldl' (\mT1 (t2, _, _)
                            -> case mT1 of
                                 Nothing -> Just t2
                                 Just t1 -> Just $ min t1 t2
                        ) Nothing s
    in (callDate, s)

bundleState_debugShow :: BundleState -> String
bundleState_debugShow (_, state1)
  = IMap.foldlWithKey'  (\s k (d, _, _) -> s ++ ", " ++ show k ++ " -> " ++ showLocalTime d) "" state1

-- | Fast test to use before calling `bundleState_serviceCdrs`
--   Because the income-rate-plan is constant on the entire rating time-frame,
--   for every closed bundle-rate, there is a corresponding new open bundle-rate.
bundleState_areThereServiceCdrs :: BundleState -> CallDate -> Bool
bundleState_areThereServiceCdrs (Nothing, _) callDate = False
bundleState_areThereServiceCdrs (Just minimumCallDate, _) callDate = callDate >= minimumCallDate
{-# INLINE bundleState_areThereServiceCdrs #-}

-- | An helper for calling bundleState_updateAndGetServiceCdrs
bundleState_updateToSomeCallDate
  :: RatingParams
  -> MainRatePlan
  -> BundlePartition
  -> BundleState
  -> CallDate
  -> IO BundleState

bundleState_updateToSomeCallDate env mainRatePlan bundlePartition state1 callDate = do
  ref <- newIORef (0, callDate, Set.empty, state1, Nothing, Nothing)
  _ <- bundleState_updateAndGetServiceCdrs env mainRatePlan bundlePartition ref callDate 
  (_, _, _, state2, _, _) <- readIORef ref
  return state2

-- | Close the time-frames, and open new-time frames, acconding the specified call-date.
bundleState_updateAndGetServiceCdrs
  :: RatingParams
  -> MainRatePlan
  -> BundlePartition
  -> IORef ( CountOfCDRS
           , CallDate
           , Set.Set CallDate
           , BundleState
           -- ^ the current bundle-state.
           -- It will be updated with the new state.
           , Maybe BundleState
           , Maybe BundleState
           )
  -> CallDate
  -> IO (Chunk ServiceCDR)
     -- ^ the ServiceCDRs of the  bundle rates

bundleState_updateAndGetServiceCdrs env mainRatePlan bundlePartition statsR callDate = do
  (countCDRS1, _, serviceDays1, bundleState1, maybeSavedBundleState, maybeDailyBundleState) <- readIORef statsR
  let   (bundleState2, serviceCDRS) = fRec bundleState1
  let serviceCDRS' = V.concat serviceCDRS
  let serviceDays2 = V.foldl' (\s x -> Set.insert (today $ cdr_calldate x) s) serviceDays1 serviceCDRS'
  modifyIORef' statsR (\_ -> (countCDRS1 + (V.length serviceCDRS'), callDate, serviceDays2, bundleState2, maybeSavedBundleState, maybeSavedBundleState))
  return serviceCDRS'

 where

  fRec :: BundleState -> (BundleState, [Chunk ServiceCDR])
  fRec bundleState1@(minimumCallDate1, state1)
    = case bundleState_areThereServiceCdrs (minimumCallDate1, state1) callDate of
        False
          -> (bundleState1, [])
        True
          -> let minCallDate = fromJust minimumCallDate1
                 (bundleState2, services2) = bundleState_updateAccordingRatingParams env mainRatePlan bundlePartition (Right minCallDate) bundleState1
                 (bundleState3, services3) = fRec bundleState2
                 -- call recursively until the calldate is not reached. In this way if there are holes in the rating, procces,
                 -- the ServiceCDRS are generated for all missing timeframes.
             in  (bundleState3, services2:services3)

bundleParams_scale :: BundleParams -> Rational -> BundleParams
bundleParams_scale r p
  = if p == 1
    then r
    else r {
             bundle_leftDuration = scaleI (bundle_leftDuration r)
           , bundle_leftCalls = scaleI (bundle_leftCalls r)
           , bundle_leftCost = case bundle_leftCost r of
                                 Nothing -> Nothing
                                 Just c -> Just $ c * p
           , bundle_initialCost = (bundle_initialCost r) * p
         }
 where

  scaleI :: Maybe Int -> Maybe Int
  scaleI Nothing = Nothing
  scaleI (Just v) = Just (fromInteger $ round $ (toRational v) * p)

-- | Return the number of seconds of time frame duration.
timeFrame_duration :: TimeFrame -> Rational
timeFrame_duration (fromCallDate, toCallDate)
  = let fromUTC = localTimeToUTC utc fromCallDate
        toUTC = localTimeToUTC utc toCallDate
        diff = diffUTCTime toUTC fromUTC
    in toRational $ diff

-- | Given  Cdr call-date return the timeframe in wich the CDR is contained.
timeFrame_fromCallDate :: RootBundleRatePlan -> CallDate -> TimeFrame
timeFrame_fromCallDate params callDate
  = timeFrame_fromCallDate1 (bundle_timeFrame params) callDate

-- | Given  Cdr call-date return the timeframe in wich the CDR is contained.
timeFrame_fromCallDate1 :: BundleRateTimeFrame -> CallDate -> TimeFrame
timeFrame_fromCallDate1 timeFrame callDate
  = case timeFrame of
      EveryDaysTimeFrame deltaDays d0 _
        -> error "Bundles using days as time-frame are implemented, but not yet tested. Contact the assistance for complete activation and support."
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

-- | Try to apply a bundle rate to a CDR, considering only the bundle limits conditions, and without considering the children rates,
--   and other rate params.
--
--   DEV NOTES: this code must be fast because it is called for each CDR.
--   @require: the CDR calldate is inside the BundleState time-frame, but not necessarly the unit-id
--   @require: BundleState is correctly initializated according the time-frame
bundle_canBeApplied
  :: RootBundleRatePlan
  -> BundleRatePlan
  -> BundleState
  -> (CDR,  BundleRateUnitId, BundleCallDuration)
  -- ^ the CDR to rate, the unit id owning the BundleRate, the CallDuration to apply
  -> Maybe BundleCallDuration
  -- ^ Nothing if the rate can not be applied, the duration of the call that can be rated from the bundle rate otherwise

bundle_canBeApplied rootBundleRate bundlePlan bundleState1@(_, state1) (cdr, unitId, duration1)
  = case IMap.lookup unitId unitRecs of
      Nothing
        -> Nothing
           -- the unit-id has no associated bundle-state
      Just (bundleParams, (unitStartingCallDate, unitEndingCallDate))
        -> case callDate >= unitStartingCallDate of
             False
               -> Nothing
                  -- the specific unit-id started a bundle time-frame after this CDR
             True
               -> if (testLeftCalls bundleParams)
                  then (testLeftDuration bundleParams)
                  else Nothing
 where

  callDate = cdr_calldate cdr

  rateId = bundle_systemId bundlePlan

  (_, _, unitRecs)
    = let mr = IMap.lookup rateId state1
      in  case mr of
            Just r -> r
            Nothing -> error ("b2: rateId " ++ show rateId ++ ",\nfor rate " ++ show bundlePlan ++ "\non state " ++ show state1) Nothing

  canBeSplit = bundle_canSplit rootBundleRate

  testLeftCalls rec1
    = case bundle_leftCalls rec1 of
        Nothing
          -> True
        Just 0
          -> False
        Just l
          -> l >= cdr_countOfCalls cdr

  testLeftDuration rec1
    = case bundle_leftDuration rec1 of
        Nothing
          -> Just duration1
             -- there is no limit on duration, so it is always respect
        Just leftDuration
          -> if leftDuration >= duration1
             then Just duration1
             else if canBeSplit
                  then Just leftDuration
                  else Nothing

-- | Create a new BundleRecord for a RootBundleRatePlan, and its children.
--
--   DEV NOTE: it works on children, because by requiremnt they share the same time-frame.
--
--   Take in consideration the activation date of a UnitId inside a bundle-time frame.
--   By design, it does not consider the ending date of a UnitId.
--
--   DEV NOTE: this function is called only at the end of each bundle time frame, so it can be also slow.
rootBundleRate_initBundleRecord
  :: RatingParams
  -> MainRatePlan
  -> BundlePartition
  -> RootBundleRatePlan
  -> NextScheduledLocalTime
  -- ^ the starting date of the time-frame
  -> (BundleState, Chunk ServiceCDR)

rootBundleRate_initBundleRecord env mainRatePlan bundlePartition rootBundleRate fromCallDate
  = let (newBundleState, newServices) =  List.foldl' processBundleRate (IMap.empty, []) (rates $ bundle_plan rootBundleRate)
    in  (bundleState_updateCachedNextScheduledLocalTime (Nothing, newBundleState), V.fromList newServices)

 where

  precision = params_currencyPrecision env
  rootRateId = bundle_systemId $ bundle_plan rootBundleRate
  rootName = fromJust1 "rp13" $ IMap.lookup rootRateId $ mainRatePlan_systemIdToUserIdPath mainRatePlan

  rates :: BundleRatePlan -> [BundleRatePlan]
  rates p
    = case bundle_children p of
        Left _
          -> [p]
             -- do not follow the external rate, and process only the root bundle rate plan
        Right children
          -> [p] ++ List.concatMap (\c -> rates c) children
             -- process both the root bundle rate plan, and its children, and the children of the children

  timeFrame@(bundleFromCallDate, toCallDate) = timeFrame_fromCallDate rootBundleRate fromCallDate

  timeFrameDuration = timeFrame_duration (bundleFromCallDate, toCallDate)

  unitInfo = params_organizations env

  bundlePriceCategoryIds = Set.fromList $ bundle_priceCategoryIds rootBundleRate

  -- | The unitIds with a possible direct assignment to the category.
  --   We are not sure that these are correct results, because we don't know if the activation date is inside time-frame.
  --   This is a fast operation to do, filtering the likely candidate.
  candidateUnitIds :: Set.Set UnitId
  candidateUnitIds
    = Set.fold (\bundleCategoryIdToSelect s
               -> let unitIds
                        = case IMap.lookup bundleCategoryIdToSelect (info_directPriceCategories unitInfo) of
                            Nothing
                              -> []
                            Just l
                              -> List.filter (bundlePartition_containsUnitId env bundlePartition timeFrame) $ List.map unit_id l
                  in List.foldl' (\s i -> Set.insert i s) s unitIds

               ) Set.empty bundlePriceCategoryIds

  -- | The unitids that are part of this bundle-rate, with the first date from which they are active inside the bundle time frame.
  activatedUnitIds :: UnitIdMap CallDate
  activatedUnitIds = Set.fold filterUnitId IMap.empty candidateUnitIds

  -- | Scan DataInfo in reverse order, searching the best result.
  --   A UnitId is selected if it has a direct assignement to the price-category, during the bundle rate timeframe.
  --   It is returned the first date of activation inside the bundle time frame, or the latest activation date.
  --   DEV NOTE: work efficiently if there are not too much direct info associated to a UnitInfo
  f :: [DataInfo] -> Maybe CallDate -> Maybe CallDate
  f [] mD = mD
  f (d:r) mD
    = let unitFromCallDate = structure_from d
          isInsideBundleTimeFrame = unitFromCallDate < toCallDate && unitFromCallDate >= bundleFromCallDate
          isProperAssignment
            = case structure_rateCategoryId d of
                Nothing
                  -> False
                     -- it is not a direct assignment
                Just categoryId
                  -> case Set.member categoryId bundlePriceCategoryIds of
                       False
                         -> False
                            -- it is a direct assignment, but not of the required category
                       True
                         -> case structure_exists d of
                              False
                                -> False
                                   -- the data-info was removed (does not exists any more)
                              True
                                -> True

          in if unitFromCallDate >= toCallDate
             then f r mD
             -- this info has no effect, because after the closing of the bundle time frame
             else if isInsideBundleTimeFrame
                  then if isProperAssignment
                       then f r (Just unitFromCallDate)
                       -- this is a date nearest to bundleFromCallDate, and it can be selected as temporary solution
                       else f r mD
                       -- search for another activation inside the time frame
                  else if isProperAssignment
                       then Just unitFromCallDate
                       -- this is the nearest activation date
                       else mD
                       -- the most recent change of history of the organization, pose it outside the bundle, so return the previous found best result,
                       -- without continuing the research

  -- | Add a UnitId to the map, if it respect the BundleRate time frame.
  filterUnitId :: UnitId -> UnitIdMap CallDate -> UnitIdMap CallDate
  filterUnitId unitId m
    = let dataInfos = fromJust1 "rp8" $ IMap.lookup unitId (info_organizations unitInfo)
      in case f dataInfos Nothing of
            Nothing
              -> m
            Just activationDate
              -> IMap.insert unitId activationDate m

  -- | Add info to rec1, adding a record for each UnitId respecting the BundleRatePlan.
  --   A BundleRatePlan share with its parent RootBundleRatePlan, the timeframe, and the activated UnitId.
  processBundleRate :: (BundleStateByRatePlan, [ServiceCDR]) -> BundleRatePlan -> (BundleStateByRatePlan, [ServiceCDR])
  processBundleRate (rec1, services1) ratePlan
    = let rateId = bundle_systemId ratePlan
          rateName = fromJust1 "rp10" $ IMap.lookup rateId (mainRatePlan_systemIdToUserIdPath mainRatePlan)
          bundleParams = bundle_bundleParams ratePlan

          createBundleRecord :: CallDate -> BundleRecord
          createBundleRecord unitFromCallDate
            = let duration = timeFrame_duration (max unitFromCallDate bundleFromCallDate, toCallDate)

                  proportion = (toRational duration) / (toRational timeFrameDuration)

                  params1 = bundleParams

                  params2
                    = case bundle_limitsAreProportionalsToActivationDate rootBundleRate of
                        False
                          -> params1
                        True
                          -> bundleParams_scale params1 proportion

              in (params2, (bundleFromCallDate, toCallDate))

         
          bundleRecords :: UnitIdMap BundleRecord
          bundleRecords = IMap.map createBundleRecord activatedUnitIds

          services2 :: [ServiceCDR]
          services2 = Data.Maybe.mapMaybe (\(u,b) -> toServiceCDRS2 u b) $ IMap.toList bundleRecords 

      in (IMap.insert rateId (toCallDate, rateName, bundleRecords) rec1, services1 ++ services2)

  {-# INLINE toServiceCDRS2 #-}
  toServiceCDRS2 :: UnitId -> BundleRecord -> Maybe ServiceCDR
  toServiceCDRS2 unitId (bundle, bundleTimeFrame)
      = generateServiceCDR unitId bundleTimeFrame (bundle_initialCost bundle)

  generateServiceCDR :: UnitId -> TimeFrame -> MonetaryValue -> Maybe ServiceCDR
  generateServiceCDR unitId (startTimeFrame, endTimeFrame) cost
      = let
            externalTelephoneNumber
              = serviceCdr_defaultExternalTelephoneNumber (bundle_serviceCDRType rootBundleRate) (bundle_serviceCDRDescription rootBundleRate)
            -- NOTE: the rate compilation procedure generate a telephone prefix associated to this telephone number.

            cdr = (cdr_empty startTimeFrame precision) {
                    cdr_countOfCalls = 0
                  -- NOTE: I'm using 0 because the total of calls is already in normal calls,
                  -- and if I set a value here, it will be added two times to the totals in call report.
                  , cdr_toCalldate = Just endTimeFrame
                  , cdr_isServiceCDR = True
                  , cdr_direction = CDR_outgoing
                  , cdr_errorDirection = CDR_none
                  , cdr_isRedirect = False
                  , cdr_billsec = Just 0
                  , cdr_duration = Just 0
                  -- NOTE: I'm using 0 because the total of calls is already in normal calls,
                  -- and if I set a value here, it will be added two times to the totals in call report.
                  , cdr_internalTelephoneNumber = ""
                  , cdr_organizationUnitId = Just $ unitId
                  , cdr_bundleOrganizationUnitId = Nothing
                  , cdr_income = cost
                  , cdr_channel = Just serviceCdr_defaultCommunicationChannel
                  , cdr_externalTelephoneNumber =  externalTelephoneNumber
                  , cdr_externalTelephoneNumberWithAppliedPortability = Just externalTelephoneNumber
                  -- DEV NOTE: this number is matched with the telephone prefix-table, so use a symbolic name
                  , cdr_displayedExternalTelephoneNumber = Just $ bundle_serviceCDRDescription rootBundleRate
                  -- DEV NOTE: this number is displayed in the call report, so use a user friendly name
                  , cdr_displayedMaskedExternalTelephoneNumber = Just $ bundle_serviceCDRDescription rootBundleRate
                  , cdr_debug_bundle_left_calls = Nothing
                  , cdr_debug_bundle_left_duration = Nothing
                  , cdr_debug_bundle_left_cost = Nothing
                  , cdr_debug_cost_rate = Just rootName
                  , cdr_debug_income_rate = Just rootName
                  }

        in if cost == 0
           then Nothing
           else Just cdr

-- | Update a BundleState:
--   * close time frames closed at NextScheduledLocalTime,
--   * create a new time frame for each new RateId,
--   * create a new time frame for each new UnitId,
--   * preserve information of unit-ids in not yet closed bundle-states
--   * generate service-cdrs of the newly opened bundle-rates 
--
--   The UnitId are initializated in a way proportional to their presence in the bundle time frame,
--   but only related to the date of when they enter in the bundle,
--   not the date from when they leave the time-frame.
--   This is sound, because a UntId can enter in a bundle, use all its resources,
--   then exit, but he must pay all the bundle, because the exit date is not so significative.
--   NOTE: in reality if also the limits of the bundle are proportional to the exit date,
--   it can make sense, as in case of initial entering date.
--   DEV-NOTES: this function is called only at the end of a time-frame, so it can be also slow.
--   DEV-NOTES: this function is used from `bundleState_updateAndGetServiceCdrs`, and should not called directly.
bundleState_updateAccordingRatingParams
  :: RatingParams
  -> CheckedMainRatePlan
  -> BundlePartition
  -- ^ init only the unit-ids of this partition
  -> Either NextScheduledLocalTime NextScheduledLocalTime
  -- ^ Right for the time frame to close: delete all time frame closed at this date, and open new time frames. This must be exactly the calldate
  --   of the time frame to close.
  --
  --   Left for creating a BundleState from an empty BundleState.
  --   Usually it is called only the first time Asterisell run.
  --   Consider as start timeframe the nearest timeframe, containing the specified CallDate, according the time frame of the rate.
  --   In this way all CDRS from this date, to next dates, can be processed, and they can update the BundleState.
  -> BundleState
  -- ^ the bundle state to update, in which there can be open time-frames to preserve
  -> (BundleState, Chunk ServiceCDR)

bundleState_updateAccordingRatingParams env mainRatePlan bundlePartition referenceCallDate bundleState1@(_, state1)
  = let (newState, newServices) = List.foldl' initRootRate (bundleState2, []) (mainRatePlan_bundleRates mainRatePlan)
    in  (bundleState_updateCachedNextScheduledLocalTime newState, V.concat newServices)
 where

  toName :: RateSystemId -> CompleteUserRateRefName
  toName rateId
    = fromJust1 "rp11" $ IMap.lookup rateId (mainRatePlan_systemIdToUserIdPath mainRatePlan)

  -- | Remove all closed time bundle-state time-frames, from state1.
  bundleState2 :: BundleState
  bundleState2
   = case referenceCallDate of
       Left _
         -> bundleState1
       Right endingTimeFrame
         -> (Nothing, IMap.filter (\(endCallDate, _, unitsState) -> not (endCallDate == endingTimeFrame)) state1)

  -- | Insert in BundleState a new RootBundleRatePlan.
  initRootRate :: (BundleState, [Chunk ServiceCDR]) -> RootBundleRatePlan -> (BundleState, [Chunk ServiceCDR])
  initRootRate (bundleState1@(_, state1), services1) bundleRate
   = let rateId = bundle_systemId $ bundle_plan bundleRate

         endingTimeFrame
           = case referenceCallDate of
               Right r
                 -> r
               Left d
                 -> fst $ timeFrame_fromCallDate bundleRate d

     in  case IMap.lookup rateId state1 of
           Just _
             -> ((Nothing, state1), services1)
           Nothing
             -> let ((_, initState2), services2) = rootBundleRate_initBundleRecord env mainRatePlan bundlePartition bundleRate endingTimeFrame
                in  ((Nothing, IMap.union initState2 state1), services2:services1)

-- | Update the bundle-map applying a BundleCallDuration.
--   DEV NOTES: this code must be fast because it is called for each CDR.
--   @require BundleCallDuration can be applied enterily to the BundleState, because it is already managed from bundle_canBeApplied function.
bundleState_update
  :: BundleState
  -> BundleRateSystemId
  -> BundleRateUnitId
  -> CountOfCDRS
  -> BundleCallDuration
  -> MonetaryValue
  -> BundleState

bundleState_update bundleState1@(cachedDate1, state1) rateId unitId countOfCalls callDuration cost
  = (cachedDate1, IMap.insert rateId (date1, rateName, IMap.insert unitId (rec2, date2) units1) state1)

 where

  (date1, rateName, units1) = fromJust1 "rp15" $ IMap.lookup rateId state1

  (rec1, date2) = fromJust1 "rp16" $ IMap.lookup unitId units1

  rec2 = rec1 {
           bundle_leftDuration
             = case bundle_leftDuration rec1 of
                 Nothing
                   -> Nothing
                 Just v
                   -> Just $ v - callDuration

         , bundle_leftCalls
             = case bundle_leftCalls rec1 of
                 Nothing
                   -> Nothing
                 Just v
                   -> Just $ v - countOfCalls

         , bundle_appliedCost = (bundle_appliedCost rec1) + cost
         }

-- ---------------------------
-- CALC CDR COST/INCOME

-- | This can be a Monetary value, or a value imported from the importer CDR (Left ()).
data RateCost =
       RateCost_cost MonetaryValue
     | RateCost_imported
       -- ^ the value imported in cost field
     | RateCost_expected
       -- ^ the value imported in expected field

-- | The params used for the default calc of CDRs.
--   The first Maybe level says if a param must override the parent param,
--   the next Maybe level (if present) represent the value of the param.
data CalcParams
  = CalcParams {

      calcParams_costForMinute :: Maybe MonetaryValue
      -- ^ the cost for minute.
      -- NOTE: the call can be rated also by second,
      -- but cost are showed by minute because otherwise
      -- prices are too high

    , calcParams_costOnCall :: Maybe RateCost
      -- ^ the initial cost of the call

    , calcParams_atLeastXSeconds :: Maybe Int
       -- ^ The minimum billable duration (in seconds)
       -- of a call. Call shorter than this duration will
       -- be billed at least for this minimum duration.

    , calcParams_durationDiscreteIncrements :: Maybe (Maybe Int)
      -- ^ rate every X seconds. A 0 duration call is rated
      --   the specified discrete increment,
      --   and so on.

    , calcParams_freeSecondsAfterCostOnCall :: Maybe Int
      -- ^ after the applying of the cost on call,
      --   do not consider in the cost the next specified seconds

    , calcParams_ceilToDecimalDigits :: Maybe (Maybe Int)
      -- ^ ceil the cost of the call to the specified digits.
      --   If left unspecified, use the maximum possible precision.

    , calcParams_floorToDecimalDigits :: Maybe (Maybe Int)
      -- ^ floor the cost of the call to the specified digits.
      --   If left unspecified, use the maximum possible precision.

    , calcParams_roundToDecimalDigits :: Maybe (Maybe Int)
      -- ^ round the cost of the call to the specified digits.
      --   If left unspecified, use the maximum possible precision.

    , calcParams_maxCostOfCall :: Maybe (Maybe MonetaryValue)
      -- ^ after calculating the cost of the call,
      --   limit it to this maximum cost

    , calcParams_minCostOfCall :: Maybe (Maybe MonetaryValue)
      -- ^ after calculating the cost of the call
      --   set it to this minimum value

    , calcParams_customCalc :: Maybe (CDR -> MonetaryValue -> MonetaryValue)
      -- ^ calculate the cost using some custom function. Apply after the calculation of other params, and the calculated value is passed to the function.
    }

-- | The params not overriding the parent params.
--
calcParams_empty :: CalcParams
calcParams_empty
  = CalcParams {
      calcParams_costForMinute = Nothing
    , calcParams_costOnCall = Nothing
    , calcParams_atLeastXSeconds = Nothing
    , calcParams_durationDiscreteIncrements = Nothing
    , calcParams_freeSecondsAfterCostOnCall = Nothing
    , calcParams_ceilToDecimalDigits = Nothing
    , calcParams_floorToDecimalDigits = Nothing
    , calcParams_roundToDecimalDigits = Nothing
    , calcParams_maxCostOfCall = Nothing
    , calcParams_minCostOfCall = Nothing
    , calcParams_customCalc = Nothing
    }

-- | The default initial values for a calcParams.
--
calcParams_defaultValues :: CalcParams
calcParams_defaultValues
  = CalcParams {
      calcParams_costForMinute = Just 0
    , calcParams_costOnCall = Just $ RateCost_cost 0
    , calcParams_atLeastXSeconds = Just 0
    , calcParams_durationDiscreteIncrements = Just Nothing
    , calcParams_freeSecondsAfterCostOnCall = Just 0
    , calcParams_ceilToDecimalDigits = Just Nothing
    , calcParams_floorToDecimalDigits = Just Nothing
    , calcParams_roundToDecimalDigits = Just Nothing
    , calcParams_maxCostOfCall = Just Nothing
    , calcParams_minCostOfCall = Just Nothing
    , calcParams_customCalc = Nothing
    }

-- | Add new specified params to the parent params, overriding them.
--   Override the first passed params, adding the second passed params to them.
calcParams_override :: CalcParams -> CalcParams -> CalcParams
calcParams_override x y
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

  ex :: (CalcParams -> Maybe a) -> CalcParams -> CalcParams -> Maybe a
  ex f x y
    = ov (f x) (f y)

  ov :: Maybe a -> Maybe a -> Maybe a
  ov Nothing Nothing = Nothing
  ov Nothing (Just y) = Just y
  ov (Just x) Nothing = Just x
  ov (Just x) (Just y) = Just y

calcParams_overrideList :: [CalcParams] -> CalcParams
calcParams_overrideList [] = error "ERR 05777"
calcParams_overrideList (first:rest)
  = List.foldl' (\old new -> calcParams_override old new) first rest

-- | Apply CalcParams, and calculate the cost of the call.
calcParams_calc :: RateRole -> CalcParams -> CDR -> MonetaryValue
calcParams_calc rateRole params cdr
  = let paramsWithDefaults = calcParams_override calcParams_defaultValues params
    in evalState (calc paramsWithDefaults) ()
 where

   cdrTotSec = fromJust1 "rp17" $ cdr_billsec cdr

   calc :: CalcParams -> State () MonetaryValue
   calc params
     = do let costForMinute = fromJust1 "rp18" $ calcParams_costForMinute params
          let costOnCall = fromJust1 "rp19" $ calcParams_costOnCall params
          let maxCostOfCall = fromJust1 "rp20" $ calcParams_maxCostOfCall params
          let minCostOfCall = fromJust1 "rp21" $ calcParams_minCostOfCall params
          let atLeastXSeconds = fromJust1 "rp22" $ calcParams_atLeastXSeconds params

          cost :: MonetaryValue  <- return $ fromIntegral 0
          totSec :: Int <- return $ cdrTotSec

          totSec <- return $ totSec - (fromJust1 "rp23" $ calcParams_freeSecondsAfterCostOnCall params)

          totSec
            <- return $ case (fromJust1 "rp24" $ calcParams_durationDiscreteIncrements params) of
                          Nothing
                            -> totSec
                          Just 0
                            -> totSec
                          Just delta
                            -> if totSec == 0
                               then delta
                               else let (t, r) =  divMod totSec delta
                                        t1 = if r > 0 then t + 1 else t
                                    in  t1 * delta

          totSec <- return $ if totSec < atLeastXSeconds then atLeastXSeconds else totSec

          cost <- return $ (costForMinute * fromIntegral totSec) / 60

          cost <- case costOnCall of
                    RateCost_cost c
                      -> return $ cost + c
                    RateCost_imported
                      -> case rateRole of
                           IncomeRate -> return $ cdr_income cdr
                           CostRate -> return $ cdr_cost cdr
                    RateCost_expected
                      -> case cdr_expectedCost cdr of
                           Nothing
                             -> return 0
                           Just c
                             -> return c

          cost <- return $ case maxCostOfCall of
                             Nothing -> cost
                             Just m -> if cost > m then m else cost

          cost <- return $ case minCostOfCall of
                             Nothing -> cost
                             Just m -> if cost < m then m else cost

          cost <- return $ case (fromJust1 "rp25" $ calcParams_roundToDecimalDigits params) of
                             Nothing -> cost
                             Just precision
                               -> if precision == 0
                                  then toRational $ round cost
                                  else let scale :: MonetaryValue = fromIntegral $ 10 ^ precision
                                           scaledCost :: MonetaryValue = cost * scale
                                       in  (toRational $ mathRound scaledCost) / scale


          cost <- return $ case (fromJust1 "rp25" $ calcParams_ceilToDecimalDigits params) of
                             Nothing -> cost
                             Just precision
                               -> if precision == 0
                                  then toRational $ ceiling cost
                                  else let scale :: MonetaryValue = fromIntegral $ 10 ^ precision
                                           scaledCost :: MonetaryValue = cost * scale
                                       in  (toRational $ ceiling scaledCost) / scale

          cost <- return $ case (fromJust1 "rp25" $ calcParams_floorToDecimalDigits params) of
                             Nothing -> cost
                             Just precision
                               -> if precision == 0
                                  then toRational $ floor cost
                                  else let scale :: MonetaryValue = fromIntegral $ 10 ^ precision
                                           scaledCost :: MonetaryValue = cost * scale
                                       in  (toRational $ floor scaledCost) / scale

          cost <- return $ case calcParams_customCalc params of
                             Nothing
                               -> cost
                             Just fun
                               -> fun cdr cost

          return cost

-- | User readable description of calc params.
calcParams_show :: CalcParams -> String
calcParams_show p
  = evalState f ()
 where

  f :: State () String
  f = do
       r <- case calcParams_freeSecondsAfterCostOnCall p of
              Nothing
                -> return $ ""
              Just 0
                -> return $ ""
              Just v
                -> return $ "do not consider the first " ++ show v ++ " seconds, "

       r <- case calcParams_durationDiscreteIncrements p of
              Nothing
                -> return $ r
              Just Nothing
                -> return $ r
              Just (Just 0)
                -> return $ r
              Just (Just v)
                -> return $ r ++ "consider call duration increments of " ++ show v ++ " seconds (e.g. from 0 to " ++ show (v - 1) ++ " seconds of real duration, " ++ show v ++ " seconds are billed, from " ++ show v ++ " to " ++ show (v * 2 - 1) ++ " seconds of real duration, " ++ show (v * 2) ++ " seconds are billed), "

       r <- case calcParams_atLeastXSeconds p of
              Nothing
                -> return $ r
              Just 0
                -> return $ r
              Just v
                -> return $ r ++ "bill at least " ++ show v ++ " seconds, "
       r <- case calcParams_costOnCall p of
              Nothing
                -> return $ r
              Just (RateCost_imported)
                -> return $ r ++ "initial cost of the call is imported from source, "
              Just (RateCost_expected)
                -> return $ r ++ "initial cost of the call is the expected value of source, "
              Just (RateCost_cost 0)
                -> return $ r
              Just (RateCost_cost v)
                -> return $ r ++ "initial cost of the call is " ++ monetaryValue_show v ++ ", "

       r <- case calcParams_costForMinute p of
              Nothing
                -> return $ r
              Just 0
                -> return $ r
              Just v
                -> return $ r ++ "multiplie duration for " ++ monetaryValue_show v ++ " and divide by 60, "

       r <- case calcParams_maxCostOfCall p of
              Nothing
                -> return $ r
              Just Nothing
                -> return $ r
              Just (Just v)
                -> return $ r ++ "the maximum cost will be in any case " ++ monetaryValue_show v ++ ", "

       r <- case calcParams_minCostOfCall p of
              Nothing
                -> return $ r
              Just Nothing
                -> return $ r
              Just (Just v)
                -> return $ r ++ "the minimum cost will be in any case " ++ monetaryValue_show v

       r <- case calcParams_roundToDecimalDigits p of
              Nothing
                -> return $ r
              Just Nothing
                -> return $ r
              Just (Just v)
                -> return $ r ++ "the cost will be rounded to " ++ show v ++ " decimal digits"

       r <- case calcParams_ceilToDecimalDigits p of
              Nothing
                -> return $ r
              Just Nothing
                -> return $ r
              Just (Just v)
                -> return $ r ++ "ceil on " ++ show v ++ " decimal digits"

       r <- case calcParams_floorToDecimalDigits p of
              Nothing
                -> return $ r
              Just Nothing
                -> return $ r
              Just (Just v)
                -> return $ r ++ "floor on " ++ show v ++ " decimal digits"

       return $ if List.null r then "free call" else r

instance Show CalcParams where
  show = calcParams_show

monetaryValueOrImported_show :: RateCost -> String
monetaryValueOrImported_show vi
  = case vi of
      RateCost_imported -> "imported"
      RateCost_expected -> "expected"
      RateCost_cost v -> monetaryValue_show v

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
      -- ^ from this date, CDRS are not yet billed and they can be freely re-rated
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
     , params_lastSavedBundleState = bundleState_empty
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
   }

-- ----------------------------------------------
-- Check code contracts (used during debugging)

ratingParams_respectCodeContracts :: RatingParams -> Either String ()
ratingParams_respectCodeContracts env
  = do rateChanges_respectCodeContracts (params_rateChanges env)
       info_respectCodeContracts (params_organizations env) (params_fromDate env)
       serviceParams_respectCodeContracts env
       return ()

-- | Return an error in case ServiceParams does not respect code contracts.
serviceParams_respectCodeContracts :: RatingParams -> Either String ()
serviceParams_respectCodeContracts s
  = do
       extractAndCheckReverseOrder "(err 1075) prices are not in reverse order of date" (servicePrice_fromDate) (params_servicePrices s)
       extractAndCheckReverseOrder2 "(err 1076) service assignmentes are not in reverse order of date" (assignedService_fromDate) (params_assignedServices s)

       return ()

 where

  extractAndCheckReverseOrder msgError extract map1
    = let l1 = List.map snd $ IMap.toAscList map1
          l2 = List.map (List.map extract) l1
          l3 = List.map isDescendingOrder l2
      in  unless (List.all id l3) (fail msgError)

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
-- SERVICE CDRS EXPORT --

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
type DBRateChanges = SMap.Map DBRateReferenceName [(LocalTime, DBRateId)]

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

-- | Load/init complete RatingParams.
--   DEV-NOTE: decide also how to split the rating time-frame, and how to sanitize the rating time-frame.
--   DEV-NOTE: it should work when there are main rate plans with bundle-rates, or when there are no saved bundle states.
--
--   The idea it is to support efficiently the incremental rating of CDRS, saving the previous calculated bundle-state,
--   and starting from here.
--   All other rating requests will be start from the begin of the bundle-state.
--   Signal errors if the billing-time frame does not contains entirely all the bundle-rates,
--   and other logical erors in the rate-plan.
--
ratePlan_loadRatingParams :: InitialRatingParams -> IO RatingParams
ratePlan_loadRatingParams p0 =
  withResource
    (db_openConnection (iparams_dbConf p0) False)
    (\conn -> execStateT (mainLoad conn) (ratingParams_empty p0))
    (db_releaseResource)
    (\exc -> SomeException $ AsterisellException $ "Error during loading of rating params: " ++ displayException exc)

  where

   mainLoad :: DB.MySQLConn -> StateT RatingParams IO ()
   mainLoad conn = do
      liftIO $ db_openTransaction conn
      let precisionDigits = iparams_currencyPrecision p0
      let configuredRateParsers = iparams_configuredRatePlanParsers p0
      let isDebugMode = iparams_isDebugMode p0

      --
      -- Load all Rating Params, excepts rates and telephone prefixes.
      -- These params are necessary for loading rates.
      --

      rateCategories <- liftIO $ rateCategories_load conn isDebugMode
      vendors <- liftIO $ vendors_load  conn isDebugMode
      channelTypes <- liftIO $ channelTypes_load conn isDebugMode
      channelsDomains <- liftIO $ channelDomains_load conn isDebugMode
      extensions <- liftIO $ extensions_load conn isDebugMode (iparams_organizationToIgnore p0)
      ratingCodes1 <- liftIO $ telephonePrefixes_loadRatingCodes conn isDebugMode
      holidays1 <- liftIO $ holidays_load conn isDebugMode
      unbilledCallsFrom <- liftIO $ defaultParams_unbilledCallsFrom conn "0275"

      modify' $ \p -> p {
                 params_rateCategories = rateCategories
               , params_vendors = vendors
               , params_channelTypes = channelTypes
               , params_channelDomains = channelsDomains
               , params_organizations = extensions
               , params_ratingCodes = ratingCodes1
               , params_holidays = holidays1
               , params_unbilledCallsFrom = unbilledCallsFrom
               }
      p <- get
      p' <- liftIO $ serviceParams_load conn p
      put p'

      --
      -- Derive (if exists) the bundle time-frame.
      --

      let ratesToImport1 :: HSet.HashSet Text.Text = HSet.fromList [mainRatePlanRef CostRate, mainRatePlanRef IncomeRate]
      p <- get
      p' <- liftIO $ ratePlan_loadRates conn p ratesToImport1
      put p'

      p <- get
      (bundleStartAtCallDate, previousBundleStartAtCallDate)
        <- case env_getRate p (mainRatePlanRef IncomeRate) (params_fromDate p) of
             Nothing
               -> liftIO $ throwIO $ AsterisellException $ "The \"main-income-rate\" is missing at date " ++ (show $ fromLocalTimeToMySQLDateTime $ params_fromDate p)

             Just (ratePlan, _)
               -> let t0 = mainRatePlan_getBundleRateStartCallDate ratePlan (params_fromDate p)

                      -- subtract 1 second
                      tz = minutesToTimeZone 0
                      t1 = addUTCTime (-1) (localTimeToUTC tz t0)
                      t2 = utcToLocalTime tz t1

                      t3 = mainRatePlan_getBundleRateStartCallDate ratePlan t2

                  in  return (t0, t3)

      --
      -- Check if the Main Rate Plan is well formed.
      --

      let checkP1 = ratingParams_empty $ p0 { iparams_fromDate = bundleStartAtCallDate }
      checkP2 <- liftIO $ calcStableTimeFrame conn checkP1
      when (isJust $ params_isShorterSafeTimeFrame checkP2)
           (throwIO $ AsterisellException
                      ("The CDRs must be rated from date " ++  show (fromLocalTimeToMySQLDateTime bundleStartAtCallDate) ++ " (taking in consideration the start of the bundle time-frame), but in this time-frame there are changes in the main income or cost rate plan (note: not the individual referenced CSV rates that can change, but the main rate plan), and Asterisell can not determine which rate plan to use for calculating bundles. Specify a common rate plan in the specified time-frame, that takes in consideration both old customers with the old rating plan, and new customers with different rating categories and different rating plans. At the end of the bigger bundle time-frame, you can load a completely different rating plan, that can ignore old types of rating plans, but until they can be virtually active, you must include them in rating plan of the bundle time-frame."))

      --
      -- Load main rate plans
      --

      p <- get
      incomeRatePlan <- liftIO $ loadMainRatePlan (mainRatePlanRef IncomeRate) p bundleStartAtCallDate
      costRatePlan <- liftIO $ loadMainRatePlan (mainRatePlanRef CostRate) p bundleStartAtCallDate

      modify' $ \p -> p { params_incomeRate = incomeRatePlan
                        , params_costRate = costRatePlan }

      --
      -- Init or load bundle-state, influencing also the rating time-frame,
      -- in case there is no saved bundle-state, because CDRS must be processed again from the beginning.
      --
      -- This code had to work also if there are no bundle-rates.
      --
      -- Check also (and signal the error) that all bundle-state starts and ends within the billing-time frame,
      -- because after a billing time-frame there can not be changes to rated CDRS, and it is all considered read-only,
      -- and changes in customers can create new service CDRS and so on.
      -- So this condition is reasonable enough in real usage scenario, and simplify the logic of the code.
      -- DEV-NOTE: if some customer request, we can improve the code, but it requires times/tests, so I stick with this for now.
      --

      p <- get
      -- If there is an already cached and saved bundle_state, use it
      let query1 = [str| SELECT
                       |   id
                       | , to_time
                       | , data_file
                       | FROM ar_bundle_state
                       | WHERE to_time <= ?
                       | AND to_time > ?
                       | AND to_time >= ?
                       | ORDER BY to_time
                       | DESC LIMIT 1
                       |]

      let beforeThisCallDateIsAnIllegalRerate
            = case (params_fromDate p) < unbilledCallsFrom of
                True -> params_fromDate p
                        -- ^ it can change already billed CDRS because it is explicitely requested,
                        -- but in any case not before the date of the request
                False -> unbilledCallsFrom
                         -- ^ it can not change CDRS that are already billed to customers,
                         -- but it is safe rating few CDRS in the past respect the requested date,
                         -- so we can reuse the incremetal bundle-state.

      let params1 = [toDBLocalTime $ params_fromDate p 
                     -- ^ consider `<= ?` because the date is exclusive, so all the new CDRS from this date (inclusive)
                     -- to next dates can be added to the old bundle-state. In other word, it contains all CDRS before to_time.
                     -- We are saying that the bundle-state can not in the future respect the requested rating date,
                     -- bun only in the past.$
                   , toDBLocalTime bundleStartAtCallDate
                    -- ^ consider `> ?` because otherwise it is better starting with an empty bundle.
                    -- We are saying that the main bundle is initializated at this date,
                    -- and if we have a bundle-state too much in the past, we can start with a fresh bundle-state.
                   , toDBLocalTime beforeThisCallDateIsAnIllegalRerate
                   ]

      (qsDef, qs) <- liftIO $ DB.query conn query1 params1
      maybeRow <- liftIO $ S.toList qs
      liftIO $ DB.skipToEof qs
      startWithNewBundleState <-
        case maybeRow of
         [[_, savedBundleCallDate, content]]
           -> case (B64.decode $ fromTextToByteString $ fromDBText content) of
                Left err
                  -> return True
                Right content2
                  -> case (DBS.decode $ LZ.decompress content2) of
                       Left err
                         -> return True
                       Right (b :: BundleState)
                         -> do modify' $ \p ->  p { params_lastSavedBundleState = b
                                                  , params_initial = (params_initial p) { iparams_fromDate = fromDBLocalTime savedBundleCallDate }
                                                  }
                               return False 

         [] -> return True
         unexpected -> throwIO $ AsterisellException ("Error 11775 in application code. Unexpected result: " ++ show unexpected ++ ", with column defs " ++ show qsDef)

      -- Start with the previous bundle-state,
      -- so it will be immediately invalidated during rating,
      -- and new service CDRS will be generated. 
      when startWithNewBundleState
           (case bundleStartAtCallDate >= beforeThisCallDateIsAnIllegalRerate of
              True -> modify' (\p -> let
                                          (bs, _) = bundleState_updateAccordingRatingParams
                                                      p
                                                      (params_incomeRate p)
                                                      bundlePartition_all
                                                      (Left previousBundleStartAtCallDate)
                                                      bundleState_empty

                                     in p { params_lastSavedBundleState = bs
                                          , params_initial = (params_initial p) { iparams_fromDate = bundleStartAtCallDate }
                                          })
              False -> throwIO $ AsterisellException
                          ("The CDRs can be rated only from date " ++  show (fromLocalTimeToMySQLDateTime unbilledCallsFrom) ++ ", because previous CDRS are already billed to customers. But there are bundle-rates activated at a previous date " ++ show (fromLocalTimeToMySQLDateTime bundleStartAtCallDate) ++ ", and this requires a rerating of already billed CDRS. The application up to date does not support this configuration, so you had to change the bundle-rates or your billing time-frame. It is required that all bundle-rates starts and ends inside the billing time-frame. Billing time-frame con be bigger than bundle-rates time-frames, but not the contrary."))

      --
      -- Adapt rating time-frame of CDRS and BundleState
      -- to a safe/stable one: a rating time-frame in which the main rate-plans do not change,
      -- so all CDRS in this rating-frame can be rated using the same method.
      -- In case it is not stable, the rating time-frame can be split.
      --

      p <- get
      p' <- liftIO $ calcStableTimeFrame conn p
      put p'

      --
      -- Configure `params_saveBundleStateImmediatelyAfter`
      --

      p <- get
      let lastCallDateQ = "SELECT calldate FROM ar_source_cdr ORDER BY calldate DESC LIMIT 1"
      (_, lastCallDateDS) <- liftIO $ DB.query_ conn lastCallDateQ 
      maybeLastCallDate <- liftIO $ S.toList lastCallDateDS
      liftIO $ DB.skipToEof lastCallDateDS
      lastCallDate1
        <- case maybeLastCallDate of
             [] -> return $ params_fromDate p
             [[d]] -> return $ fromDBLocalTime d

      let lastCallDate2
            = case (params_isShorterSafeTimeFrame p) of
                Nothing -> lastCallDate1
                Just r -> r

      modify' $ \p -> p { params_saveBundleStateImmediatelyAfter = calc_saveBundleStateImmediatelyAfter lastCallDate2 }

      --
      -- Load only the CSV rates referencend in main rate plans.
      --

      p <- get
      let allRatesToImport
            = List.foldl' mainRatePlan_externalRateReferences ratesToImport1 (IMap.elems $ params_rates p)

      p' <- liftIO $ ratePlan_loadRates conn p allRatesToImport
      put $ assert (isRight $ ratingParams_respectCodeContracts p') p'

      p <- get
      (when isDebugMode)
        (case ratingParams_respectCodeContracts p of
           Left err -> throwIO $ AsterisellException err
           Right () -> return ())

      cdrImporters <- liftIO $ deriveFastLookupCDRImportes conn

      modify' $ \p -> p { params_fastLookupCDRImporters = cdrImporters }

      --
      -- Update telephone prefix table with info derived from services and rates.
      --

      p <- get
      liftIO $ service_exportServiceCDRSTelephonePrefixes conn p
      case env_getRate p (mainRatePlanRef IncomeRate) (params_fromDate p) of
             Nothing
               -> throwIO $ AsterisellException $ "The \"main-income-rate\" is missing at date " ++ (show $ fromLocalTimeToMySQLDateTime $ params_fromDate p)

             Just (ratePlan, _)
               -> liftIO $ mainRatePlan_exportServiceCDRSTelephonePrefixes conn ratePlan

      (telephonePrefixes, idToTelephonePrefixes, ratingCodes2)
          <- liftIO $ telephonePrefixes_load conn isDebugMode
      -- NOTE: doing this only now, we are sure to load the last/updated telephone prefixes

      modify' $ \p -> p { params_telephonePrefixes = telephonePrefixes
                        , params_ratingCodes = ratingCodes2
                        , params_idToTelephonePrefix = idToTelephonePrefixes }

      return ()

   -- | Analyze changes in rate-plans, and find a time-frame in which all CDRS can be calculated using the same main rate-plan.
   calcStableTimeFrame :: DB.MySQLConn -> RatingParams -> IO RatingParams

   calcStableTimeFrame conn p1
     = let fromCallDate = params_fromDate p1

           q = [str| SELECT
                   |   from_time
                   | FROM ar_rate
                   | WHERE (internal_name = ? OR internal_name = ?)
                   | AND from_time > ?
                   | ORDER BY from_time
                   | LIMIT 1
                   |]

           values = [toDBText (mainRatePlanRef CostRate), toDBText (mainRatePlanRef IncomeRate), toDBLocalTime fromCallDate]

       in do (_, inS) <- DB.query conn (DB.Query q) values
             mr <- S.read inS
             case mr of
               Nothing
                 -> return $ p1 { params_isShorterSafeTimeFrame = Nothing }
               Just [r]
                 -> do DB.skipToEof inS
                       return $ p1 { params_isShorterSafeTimeFrame = Just $ fromDBLocalTime r }

   loadMainRatePlan :: DBRateReferenceName -> RatingParams -> CallDate -> IO MainRatePlan
   loadMainRatePlan rateRefName env fromDate = do
     case env_getRate env rateRefName fromDate of
       Nothing
         -> liftIO $
              throwIO $
                asterisellError_toException $
                  createError
                    Type_Critical
                    Domain_RATES
                    ("unknown referenced rate - " ++ Text.unpack rateRefName)
                    ("Unknown rate with name \"" ++ Text.unpack rateRefName ++ "\", active from date " ++ showLocalTime fromDate)
                    ("All CDRs from the specified calldate, will be not rated. The call report stats will signal the unrated calls.")
                    ("Add a rate specification, or change the starting/ending dates of current rate specifications.")

       Just (ratePlan, _)
         -> case mainRatePlan_assignUniqueSystemId ratePlan of
              Left err
                -> liftIO $ throwIO $ asterisellError_toException $
                       createError
                         Type_Critical
                         Domain_RATES
                         ("error in redefinition of rates - " ++ Text.unpack rateRefName ++ " at date " ++ showLocalTime fromDate)
                         ("The rate with name \"" ++ Text.unpack rateRefName ++ "\", defined for a CDR at date " ++ showLocalTime fromDate ++ ", can not replace correctly pending bundle rates, defined previously. " ++ err)
                         ("All the CDRs from this date, will be not rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                         ("Correct the rate specification.")

              Right checkedRatePlan
                -> return checkedRatePlan
                   -- NOTE: assume that the same name is assigned to the same sub-rate, so already saved bundle-state remain consistent
                   -- In case of redefinition of a main-rate-plan:
                   -- * a complete rating event is generated on the unbilled time-frame
                   -- * bundle-states start from the beginning in empty state
                   -- * this is correct only if bundle-state is initializated at the beginning of the rating time-frame


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

  -- MAYBE create a parallel stream job processor using many cores for parsing different rates

  rates :: CachedDBRates <- M.foldM (loadRate getRateStmt) cachedRates_empty (List.concat $ SMap.elems changes)

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

-- --------------------------------
-- EXTERNAL RATE PARSING SUPPORT

mainRatePlan_externalRateReferences :: HSet.HashSet ExternalRateReference -> MainRatePlan -> HSet.HashSet ExternalRateReference
mainRatePlan_externalRateReferences s p = f s p
 where

    f :: HSet.HashSet ExternalRateReference -> MainRatePlan -> HSet.HashSet ExternalRateReference
    f s1 p1
      = let s2 = List.foldl' ratePlan_externalRateReferences s1 (mainRatePlan_normalRates p1)
            s3 = List.foldl' bundleRate_externalRateReferences s2 (List.map bundle_plan $ mainRatePlan_bundleRates p1)
         in s3

    bundleRate_externalRateReferences :: HSet.HashSet ExternalRateReference -> BundleRatePlan -> HSet.HashSet ExternalRateReference
    bundleRate_externalRateReferences s1 p1
      = case bundle_children p1 of
          Left e
            -> HSet.insert e s1
          Right p2s
            -> List.foldl' bundleRate_externalRateReferences s1 p2s

ratePlan_externalRateReferences :: HSet.HashSet ExternalRateReference -> RatePlan -> HSet.HashSet ExternalRateReference
ratePlan_externalRateReferences s p = f s p
 where

   f :: HSet.HashSet ExternalRateReference -> RatePlan -> HSet.HashSet ExternalRateReference
   f s1 p1
     = let s2 = List.foldl' f s1 (rate_elsePart p1)
       in case rate_children p1 of
            Left e
              -> HSet.insert e s2
            Right rs
              -> List.foldl' f s2 rs

-- | A name for a rate format, as imported from the external database.
type RateFormatName = Text.Text

-- | Parse a rate specification, deriving the matching function.
type RatePlanParser = RatingParams -> BS.ByteString -> (Either String MainRatePlan)

instance Show RatePlanParser where
    show s = "<rate plan parser>"

type ConfiguredRatePlanParsers = Map.Map RateFormatName RatePlanParser

-- | The logical type associated to main rate plans.
ratePlanSpecificationType :: Text.Text
ratePlanSpecificationType = Text.pack "rate-plan-specification"

-- | The reference name of an external rate.
type ExternalRateReference = Text.Text

-- | The result of compilation of external rate plans.
type ExternalRatePlans = Map.Map ExternalRateReference RatePlan

configuredRatePlanParsers_get :: ConfiguredRatePlanParsers -> RateFormatName -> Maybe RatePlanParser
configuredRatePlanParsers_get conf1 l = Map.lookup l conf1

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

------------------------------
-- GENERIC PARSING OF RATES --
------------------------------
-- MAYBE remove this section

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

-- | Generate all ServiceCDRs until the calldate is not reached.
--
--   DEV-NOTE: there are not so many CDRs of this type (proportional to customers), so they are generated using a list, and not pipes.
service_generate
  :: RatingParams
  -> CallDate
  -- ^ the initial rating call date (it can be not exactly the beginning of a time-frame)
  -> CallDate
  -- ^ the final rating calldate (it can be not exactly the beginning of a time-frame)
  -> Either String [ServiceCDR]

service_generate env rateFromDate rateToDate
  = cdrsOrError

 where

  precision = params_currencyPrecision env

  cdrsOrError = IMap.foldlWithKey' processServiceInAllTimeFrames (Right []) (params_assignedServices env)

  processServiceInAllTimeFrames :: Either String [CDR] -> Int -> UnitIdMap [AssignedService] -> Either String [CDR]
  processServiceInAllTimeFrames (Left err) _ _  = Left err
  processServiceInAllTimeFrames (Right cdrs1) serviceId assignedServices
    = let service = fromJust1 ("s1: unknown serviceId " ++ show serviceId ++ ", on data: " ++ show (params_services env)) $ IMap.lookup serviceId (params_services env)

          serviceSchedule = service_schedule service

          allTimeFrames
            = timeFrames_allInsideRatingPeriod serviceSchedule rateFromDate rateToDate

          cdrs2OrError = List.foldl' extractError (Right []) $ List.map processServiceInTimeFrame allTimeFrames

          extractError (Left err) _ = Left err
          extractError (Right r1) (Left err) = Left err
          extractError (Right r1) (Right r2) = Right $ r1 ++ r2

          processServiceInTimeFrame :: TimeFrame -> Either String [CDR]
          processServiceInTimeFrame (timeFrameStart2, timeFrameEnd2)
            = let cdrs3 = IMap.foldlWithKey' processUnit  (Right []) assignedServices
                  timeFrameDuration2 = timeFrame_duration (timeFrameStart2, timeFrameEnd2)

                  processUnit :: Either String [CDR] -> UnitId -> [AssignedService] -> Either String [CDR]
                  processUnit (Left err) _ _  = Left err
                  processUnit (Right cdrs4) unitId assignements
                    = let -- DEV-NOTE: this code is based on fact that assignments are in reverse order of applicability date.

                          calculatedServicesOrErrors = processAssignment assignements ([], Nothing)

                          calculatedServices = case calculatedServicesOrErrors of
                                                 Right (r, _) -> r
                                                 _ -> error "never used"

                          calculatedCdrs = List.concatMap calcService calculatedServices

                          processAssignment :: [AssignedService] -> ([CalcService], Maybe CallDate) -> Either String ([CalcService], Maybe CallDate)
                          processAssignment [] result = return result
                          processAssignment (assignement:rest) (calcs1, maybePartialDate)
                            = let items = assignedService_nrOfItems assignement
                                  assignmentCallDate = assignedService_fromDate assignement

                                  -- The reference date to use for searching in the price-list
                                  priceListDate
                                    = case service_priceChangeWithPriceList service of
                                        True
                                          -> max assignmentCallDate timeFrameStart2
                                             -- consider the price at the moment of activation or rating: it is the more precise price
                                        False
                                          -> -- In this case the price is the price paid from the customer the first time he activated the service.
                                             -- In case the service was set to 0 in the past, and then activated again, consider the price of the new activation.
                                             let f resultCallDate [] = resultCallDate
                                                 f resultCallDate (ass:rest)
                                                   = case (assignedService_fromDate ass) >= assignmentCallDate of
                                                       True
                                                         -> f resultCallDate rest
                                                            -- discard assignment in the future respect current assignment
                                                       False
                                                         -> case assignedService_nrOfItems ass of
                                                              0 -> resultCallDate
                                                                   -- in this case the previous used prices can be discarded, and the new price is determined from this point
                                                              _ -> f (assignedService_fromDate ass) rest
                                                                   -- go to the first assignment (the first price used)

                                             in f assignmentCallDate assignements

                                  -- The price of the service, using priceListDate as reference
                                  maybePrice :: Maybe MonetaryValue
                                    = let f [] = Nothing
                                          f (price:rest)
                                            = case (servicePrice_fromDate price) <= priceListDate of
                                                True -> Just $ servicePrice_price price
                                                False -> f rest

                                      in case IMap.lookup serviceId (params_servicePrices env) of
                                           Nothing
                                             -> Nothing

                                           Just prices
                                             -> f prices
                                                -- search the more recent price (according priceListDate) in the prices list

                                  calc2 price
                                    = CalcService {
                                        calcService_nrOfItems = items
                                      , calcService_price = price
                                      , calcService_discount = assignedService_discount assignement
                                      , calcService_timeFrame
                                          = (case service_priceIsProportionalToActivationDate service of
                                               True -> max assignmentCallDate timeFrameStart2
                                               False -> timeFrameStart2
                                            , case maybePartialDate of
                                                Nothing -> timeFrameEnd2
                                                Just d -> case service_priceIsProportionalToActivationDate service of
                                                            True -> min d timeFrameEnd2
                                                            False -> timeFrameEnd2
                                            )
                                      }


                                  -- Test if the current assignment can be applied in the current time frame,
                                  -- and if there can be results continuing processing.
                                  (canBeApplied, continueProcessing) :: (Bool, Bool)
                                    = case service_isAppliedOnlyOneTime service of
                                        True
                                          -> ((assignmentCallDate >= timeFrameStart2 && assignmentCallDate < timeFrameEnd2), True)
                                             -- process only the assignment in the first time-frame
                                        False
                                          -> case assignmentCallDate >= timeFrameEnd2 of
                                               True
                                                 -> (False, True)
                                                    -- this assignment was done in the future respect current time frame, and it does not affect it
                                               False
                                                 -> case maybePartialDate of
                                                      Nothing
                                                        -> (True, True)
                                                           -- the assignment is in the past, but it is still actual in the current time frame
                                                      Just partialDate
                                                        -> case partialDate <= timeFrameStart2 of
                                                             True
                                                               -> (False, False)
                                                                  -- the assignment is in the past respect the timeframe, and already overwritten from a more recent assignment
                                                             False
                                                               -> (True, True)
                                                                  -- the assignment is in the past, but it extend also to the current time-frame, until a more recent assignment take effect

                                  nextPartialDate
                                    = case canBeApplied of
                                        True -> Just assignmentCallDate
                                        False -> Nothing

                              in do calcs2
                                      <- case canBeApplied of
                                           False
                                             -> return calcs1
                                           True
                                             -> do price
                                                     <- case maybePrice of
                                                          Nothing -> throwError $ "There is no price for service " ++ (show $ service_id service) ++ ", " ++ (show $ service_name service) ++ ", at date " ++ showLocalTime priceListDate ++ ", in its price list."
                                                          Just p -> return p

                                                   return $ (calc2 price):calcs1
                                                   case service_priceIsProportionalToActivationDate service of
                                                     True -> return $ (calc2 price):calcs1
                                                             -- in case of prices depending from the activation date,
                                                             -- keep note of all different activations, and sum them.

                                                     False -> let maxItems = List.maximum $ items:(List.map calcService_nrOfItems calcs1)
                                                              in return $ [(calc2 price) { calcService_nrOfItems = maxItems } ]
                                                              -- in case of prices not depending from activation date,
                                                              -- keep note of the maximum number of activated items in the time frame period,
                                                              -- using the price of the first activation in the time-frame.

                                    case continueProcessing of
                                      False
                                        -> return $ (calcs2, Just assignmentCallDate)
                                      True
                                        -> processAssignment rest (calcs2, nextPartialDate)

                          calcService :: CalcService -> [CDR]
                          calcService c
                            = let duration = timeFrame_duration $ calcService_timeFrame c
                                  proportion
                                    = case (service_priceIsProportionalToActivationDate service) of
                                        True -> (toRational duration) / (toRational timeFrameDuration2)
                                        False -> toRational 1

                                  scaledPrice = (calcService_price c) * proportion
                                  discountSum =  scaledPrice * (calcService_discount c)
                                  income = (scaledPrice - discountSum) * (toRational $ calcService_nrOfItems c)

                                  externalTelephoneNumber = service_defaultExternalTelephoneNumber service
                                  -- NOTE: the rate compilation procedure generate a telephone prefix associated to this telephone number.

                                  serviceInfo
                                    = let descr = (Text.unpack $ service_description service)
                                          maybeDescr = case List.null descr of
                                                         True -> ""
                                                         False -> " - " ++ descr

                                      in  Text.pack $ (Text.unpack $ service_name service) ++ maybeDescr

                                  cdr = (cdr_empty timeFrameStart2 precision) {
                                          cdr_countOfCalls = calcService_nrOfItems c
                                        , cdr_toCalldate = Just timeFrameEnd2
                                        , cdr_isServiceCDR = True
                                        , cdr_direction = CDR_outgoing
                                        , cdr_errorDirection = CDR_none
                                        , cdr_isRedirect = False
                                        , cdr_duration = Just 0
                                        -- NOTE: I'm using 0 because the total of calls is already in normal calls,
                                        -- and if I set a value here, it will be added two times to the totals in call report.
                                        , cdr_billsec = Just 0
                                        , cdr_internalTelephoneNumber = ""
                                        , cdr_organizationUnitId = Just $ unitId
                                        , cdr_bundleOrganizationUnitId = Nothing
                                        , cdr_income = income
                                        , cdr_cost = 0
                                        , cdr_channel = Just serviceCdr_defaultCommunicationChannel
                                        , cdr_externalTelephoneNumber =  externalTelephoneNumber
                                        , cdr_externalTelephoneNumberWithAppliedPortability = Just externalTelephoneNumber
                                        -- DEV NOTE: this number is matched with the telephone prefix-table, so use a symbolic name
                                        , cdr_displayedExternalTelephoneNumber = Just $ serviceInfo
                                        , cdr_displayedMaskedExternalTelephoneNumber = Just $ serviceInfo
                                        , cdr_debug_bundle_left_calls = Nothing
                                        , cdr_debug_bundle_left_duration = Nothing
                                        , cdr_debug_bundle_left_cost = Nothing
                                        }

                              in if income == 0
                                 then []
                                 else if ((cdr_calldate cdr) >= rateFromDate && (cdr_calldate cdr) < rateToDate)
                                      then [cdr]
                                           -- NOTE: keep in synchro this condition with Cmd_deleteCDRS condition.
                                      else []

                      in case calculatedServicesOrErrors of
                           Left err
                             -> Left err
                           Right _
                             -> Right $ cdrs4 ++ calculatedCdrs

              in cdrs3

      in case cdrs2OrError of
           Left err
             -> Left err
           Right cdrs2
             -> Right $ cdrs1 ++ cdrs2

service_defaultExternalTelephoneNumber :: Service -> Text.Text
service_defaultExternalTelephoneNumber service
  = serviceCdr_defaultExternalTelephoneNumber (service_name service) (service_description service)

-- | Export the telephone prefixes to use for service-cdrs.
service_exportServiceCDRSTelephonePrefixes :: DB.MySQLConn -> RatingParams -> IO ()
service_exportServiceCDRSTelephonePrefixes conn sp = do
  s1 :: S.InputStream Service <- S.fromList $ IMap.elems $ params_services sp
  s2 :: S.InputStream TelephonePrefixRecord
     <- S.map
          (\s -> let prefix = service_defaultExternalTelephoneNumber s
                 in  TelephonePrefixRecord {
                       tpr_prefix = prefix
                     , tpr_matchOnlyExactDigits = Just $ Text.length prefix
                     , tpr_name = service_name s
                     , tpr_rating_code = ""
                     , tpr_geographic_location = service_name s
                         , tpr_operator_type = "Service"
                         , tpr_display_priority = 10
                         }
          ) s1

  updateS <- telephonePrefixes_update conn
  S.connect s2 updateS
  return ()

-- ----------------------
-- UNIT TESTS

tt_ratePlanTests
  = [ HUnit.TestCase $ HUnit.assertEqual "time frame " secondsInADay (timeFrame_duration (date1, date2))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (secondsInADay + 60 * 60) (timeFrame_duration (date1, date3))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-08-06 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-08-06 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-08-30 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-09-04 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-09-04 23:59:59"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-09-05 00:00:00", d "2014-10-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-09-05 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-12-05 00:00:00", d "2015-01-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-12-05 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-12-05 00:00:00", d "2015-01-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5 midnight)) (d "2014-12-06 00:00:00"))

    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-11 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-12 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-13 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-17 23:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-18 00:00:00", d "2014-08-25 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-18 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-18 00:00:00", d "2014-08-25 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-19 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-07-28 00:00:00", d "2014-08-04 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-07-28 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-07-28 00:00:00", d "2014-08-04 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1 midnight)) (d "2014-08-01 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "CDR calc" cdrO1V (calcParams_calc IncomeRate cpO1 cdrO1)
    ]
 where

  date1 = fromJust $ fromMySQLDateTimeToLocalTime "2014-08-26 00:00:00"
  date2 = fromJust $ fromMySQLDateTimeToLocalTime "2014-08-27 00:00:00"
  date3 = fromJust $ fromMySQLDateTimeToLocalTime "2014-08-27 01:00:00"
  secondsInADay = 60 * 60 * 24

  d :: String -> CallDate
  d s = fromJust $ fromMySQLDateTimeToLocalTime s

  tf :: BundleRateTimeFrame -> RootBundleRatePlan
  tf f
    = RootBundleRatePlan {
        bundle_serviceCDRType = Text.pack ""
      , bundle_serviceCDRDescription = Text.pack ""
      , bundle_timeFrame = f
      , bundle_priceCategoryIds = []
      , bundle_limitsAreProportionalsToActivationDate = False
      , bundle_canSplit = False
      , bundle_onlyForCallsWithACost = False
      , bundle_plan
          = BundleRatePlan {
              bundle_systemId  = 1
            , bundle_bundleParams
                = BundleParams {
                    bundle_initialCost = 0
                  , bundle_leftCost = Nothing
                  , bundle_leftCalls = Nothing
                  , bundle_leftDuration = Nothing
                  , bundle_appliedCost = 0
                  }
            , bundle_rateParams
                = RateParams {
                    rate_userId = ""
                  , rate_match = matchFun_initial
                  }
            , bundle_children = Right []
            }
      }

  cp1 = calcParams_defaultValues

  cp2 = calcParams_empty {
          calcParams_costOnCall = Just $ RateCost_cost 2
        }

  cp3 = calcParams_empty {
          calcParams_costForMinute = Just 1
        }

  cpO1 = calcParams_overrideList [cp1, cp2, cp3]

  cpO2 = calcParams_overrideList [cp1, cp2]

  cpO3 = calcParams_overrideList [cp2, cp3]

  cpO4 = calcParams_overrideList [cp2]

  cdrO1 = (cdr_empty date1 4) {
            cdr_direction = CDR_outgoing
          , cdr_billsec = Just 60
          }

  cdrO1V = toRational 3

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
    }

tt_servicesTests

  = [ isExpectedResult
        "simple case 1"
        cdrs1_1
        [(fromJust $ fromMySQLDateTimeToLocalTime "2014-02-01 00:00:00", 1, toRational 20)]
    , isExpectedResult
        "simple case 2"
        cdrs1_2
        [(fromJust $ fromMySQLDateTimeToLocalTime "2014-02-01 00:00:00", 1, toRational 20)
        ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-03-01 00:00:00", 1, toRational 20)
        ]

    , isExpectedResult
        "simple case 3"
        cdrs1_3
        []

    , isExpectedError "simple case 4" error2

    , isExpectedResult "complex case 1" cdrs3_1 expected3_1
    , isExpectedResult "complex case 2" cdrs4_1 expected4_1
    ]

 where

  precisionDigits = 4

  env = ratingParamsForTest precisionDigits

  serviceMap :: [Service] -> ServiceIdMap Service
  serviceMap ss = IMap.fromList $ List.map (\s -> (service_id s, s)) ss

  priceListMap :: [ServicePrice] -> ServiceIdMap [ServicePrice]
  priceListMap ss
    = let sameService s1 s2 = (servicePrice_serviceId s1) == (servicePrice_serviceId s2)
          l1 = List.groupBy sameService ss
          l2 = List.map (\(e:r) -> ((servicePrice_serviceId e), (e:r))) l1
          l3 = List.map (\(i, l) -> (i, List.reverse $ List.sortBy (\x y -> compare (servicePrice_fromDate x) (servicePrice_fromDate y)) l)) l2
      in  IMap.fromList l3

  assignmentMap :: [AssignedService] -> ServiceIdMap (UnitIdMap [AssignedService])
  assignmentMap ss
    = let ss1 = List.foldl' serviceParams_insertAssignment IMap.empty ss
          ss2 = IMap.map (\m -> IMap.map (\s -> List.reverse $ List.sortBy (\x y -> compare (assignedService_fromDate x) (assignedService_fromDate y)) s) m) ss1
      in ss2

  generate p fromDate toDate
    = service_generate p (fromJust1 "sa1" $ fromMySQLDateTimeToLocalTime fromDate) (fromJust1 "sa2" $ fromMySQLDateTimeToLocalTime toDate) 

  isExpectedResult
    :: String
    -- ^ test title
    -> [ServiceCDR]
    -- ^ calculated result
    -> [(CallDate, UnitId, MonetaryValue)]
    -- ^ expected result
    -> HUnit.Test

  isExpectedResult msg c1 e1
    = let extract s = ( cdr_calldate s
                      , fromJust1 "sa3" $ cdr_organizationUnitId s
                      , cdr_income s
                      )

          c2 = List.sort $ List.map extract c1
          e2 = List.sort e1

      in  HUnit.TestCase $ HUnit.assertEqual msg e2 c2

  isExpectedError :: String -> Either String a -> HUnit.Test
  isExpectedError msg a
    = let f (Left _) = True
          f _ = False
      in  HUnit.TestCase $ HUnit.assertBool msg (f a)

  s1
    = Service {
         service_id = 1
       , service_name = "1"
       , service_description = ""
       , service_priceIsProportionalToActivationDate = False
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1 midnight
       }

  sp1
    = ServicePrice {
          servicePrice_id = 1
        , servicePrice_serviceId = service_id s1
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2013-01-01 00:00:00"
        , servicePrice_price = toRational 10
        }

  unit1 = 1

  sa1
    = AssignedService {
        assignedService_id = 1
      , assignedService_serviceId = service_id s1
      , assignedService_unitId = unit1
      , assignedService_nrOfItems = 2
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-01-01 00:00:00"
      , assignedService_discount = fromRational 0
     }

  p1
    = env {
        params_services = serviceMap [s1]
      , params_servicePrices = priceListMap [sp1]
      , params_assignedServices = assignmentMap [sa1]
      }

  (Right cdrs1_1)
    = generate p1 "2014-02-01 00:00:00" "2014-03-01 00:00:00"

  (Right cdrs1_2)
    = generate p1 "2014-02-01 00:00:00" "2014-04-01 00:00:00"

  (Right cdrs1_3)
    = generate p1 "2013-11-01 00:00:00" "2013-12-01 00:00:00"

  -- do not specify a needed price list
  sp2
    = ServicePrice {
          servicePrice_id = 1
        , servicePrice_serviceId = service_id s1
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-01-15 00:00:00"
        , servicePrice_price = toRational 10
        }

  sa2
    = AssignedService {
        assignedService_id = 1
      , assignedService_serviceId = service_id s1
      , assignedService_unitId = unit1
      , assignedService_nrOfItems = 2
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-01-03 00:00:00"
      , assignedService_discount = fromRational 0
     }

  p2
    = env {
        params_services = serviceMap [s1]
      , params_servicePrices = priceListMap [sp2]
      , params_assignedServices = assignmentMap [sa2]
      }

  error2
    = generate p2 "2014-01-01 00:00:00" "2014-02-01 00:00:00"

  -- Test different combinations of services, with different parameters.
  -- I'm using a month with 30 days for having round values.

  s3_1
    = Service {
         service_id = 31
       , service_name = "3_1"
       , service_description = "Service depend from activation date."
       , service_priceIsProportionalToActivationDate = True
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1 midnight
       }

  s3_2
    = Service {
         service_id = 32
       , service_name = "3_2"
       , service_description = "Service price change with time."
       , service_priceIsProportionalToActivationDate = False
       , service_priceChangeWithPriceList = True
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1 midnight
       }

  s3_3
    = Service {
         service_id = 33
       , service_name = "3_3"
       , service_description = "Service price change with time, and depend from activation date."
       , service_priceIsProportionalToActivationDate = True
       , service_priceChangeWithPriceList = True
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1 midnight
       }

  s3_4
    = Service {
         service_id = 34
       , service_name = "3_4"
       , service_description = "Service is applied only one time."
       , service_priceIsProportionalToActivationDate = True
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = True
       , service_schedule = MonthlyTimeFrame 1 midnight
       }

  s3_5
    = Service {
         service_id = 35
       , service_name = "3_5"
       , service_description = "Weekly service."
       , service_priceIsProportionalToActivationDate = False
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1 midnight
       }

  generateCommonSp1 serviceId
    = [
        ServicePrice {
          servicePrice_id = 1
        , servicePrice_serviceId = serviceId
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
        , servicePrice_price = toRational 50
        }

      , ServicePrice {
          servicePrice_id = 2
        , servicePrice_serviceId = serviceId
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-20 00:00:00"
        , servicePrice_price = toRational 100
        }

      , ServicePrice {
          servicePrice_id = 3
        , servicePrice_serviceId = serviceId
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-05-16 00:00:00"
        , servicePrice_price = toRational 200
        }
      ]

  sa3_1
    = AssignedService {
        assignedService_id = 1
      , assignedService_serviceId = service_id s3_1
      , assignedService_unitId = 1
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  sa3_2
    = AssignedService {
        assignedService_id = 2
      , assignedService_serviceId = service_id s3_4
      , assignedService_unitId = 4
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  sa3_3
    = AssignedService {
        assignedService_id = 3
      , assignedService_serviceId = service_id s3_2
      , assignedService_unitId = 2
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  sa3_4
    = AssignedService {
        assignedService_id = 4
      , assignedService_serviceId = service_id s3_3
      , assignedService_unitId = 3
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  p3
    = env {
        params_services = serviceMap [s3_1, s3_2, s3_3, s3_4, s3_5]
      , params_servicePrices = priceListMap $ (List.concatMap generateCommonSp1 $ List.map service_id $ [s3_1, s3_2, s3_3, s3_4, s3_5])
      , params_assignedServices = assignmentMap [sa3_1, sa3_2, sa3_3, sa3_4]
      }

  (Right cdrs3_1)
    = generate p3 "2014-04-01 00:00:00" "2014-06-01 00:00:00"

  expected3_1
    = [(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 1, toRational 22.5)
      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 1, toRational 45)
       -- price proportional to activation date, and with discount applied
       -- next month there is a full activation time frame
       -- next month the old price list is applied

      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 3, toRational 22.5)
      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 3, toRational 90)
       -- next month there is a full activation date
       -- next month the new price list is appliedy

      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 4, toRational 22.5)
       -- it is applied only one month

      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 2, toRational 45)
      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 2, toRational 90)
       -- use the new price for calculating the cost in the second time frame
      ]

  -- Test multiple assignments of a price in the same timeframe

  p4
    = env {
        params_services = serviceMap [s3_1, s3_2, s3_3, s3_4, s3_5]
      , params_servicePrices = priceListMap $ (List.concatMap generateCommonSp1 $ List.map service_id $ [s3_1, s3_2, s3_3, s3_4, s3_5])
      , params_assignedServices
          = assignmentMap [
              AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 1
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 1
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

            -- test assignment to 0
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 11
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 11
              , assignedService_nrOfItems = 0
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 11
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00"
              , assignedService_discount = fromRational 0
              }

              -- use a service not proportional, but the price that is changing
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_2
              , assignedService_unitId = 2
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_2
              , assignedService_unitId = 2
              , assignedService_nrOfItems = 2
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

              -- proportional timeframe and variable price list
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_3
              , assignedService_unitId = 3
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_3
              , assignedService_unitId = 3
              , assignedService_nrOfItems = 2
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

              -- no proportional timeframe and no variable price list
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_5
              , assignedService_unitId = 5
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_5
              , assignedService_unitId = 5
              , assignedService_nrOfItems = 2
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

            ]

      }

  (Right cdrs4_1)
    = generate p4 "2014-04-01 00:00:00" "2014-06-01 00:00:00"

  expected4_1
    = [(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 1, toRational 35)
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 1, toRational 15)
       -- combine two time frames, but with fixed price
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 1, toRational 50)
       -- entire time frame, using the original price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 11, toRational 35)
      -- rate until units are not 0

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 11, toRational 100)
      -- recognize a setting to 0 of a service, and start with the new price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 2, toRational 100)
        -- consider 2 items on the entire time frame, but using the last price
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 2, toRational 200)
        -- consider 2 items using the new price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 3, toRational 35)
        -- consider 1 item
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 3, toRational 60)
        -- consider 2 items using the new price, and proportional timeframe
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 3, toRational 200)
        -- consider 2 items using the new price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 5, toRational 100)
        -- consider 2 items using the initial price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 5, toRational 100)
        -- consider 2 items using the old price

      ]
