{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, FlexibleContexts  #-}

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


-- | Manage rate plans, and rate CDRs.
--   Import also rates from PHP world, to Haskell world.
--
module Asterisell.RatePlan (
  RateRole(..)
  , ConfiguredRatePlanParsers
  , EnvParams(..)
  , ExtensionsToExport
  , ratingEnv_load
  , initialClassificationParams_empty
  , parseRatePlanUsingParsecParser
  , RatingEnv(..)
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
  , CalcParams(..)
  , calcParams_empty
  , calcParams_defaultValues
  , calcParams_override
  , calcParams_overrideList
  , calcParams_calc
  , bundleState_pendingServiceCdrs
  , env_empty
  , env_getRate
  , parseRatePlanUsingAttoParsec
  , mainRatePlan_assignSharedUniqueSystemId
  , bundleState_initAccordingRatingEnv
  , bundleState_serviceCdrs
  , RatingDebugInfo(..)
  , BundleRateSystemId
  , BundleRateUnitId
  , CallDate
  , RatePlanIdMap
  , UnitIdMap
  , TimeFrame
  , CSVFormat_TelephonePrefix1(..)
  , timeFrame_duration
  , mainRatePlan_exportToCSVTheServiceCDRSTelephonePrefixes
  , params_respectCodeContracts
  , tt_ratePlanTests
  , timeFrame_fromCallDate
  , timeFrame_fromCallDate1
  , env_respectCodeContracts
  , timeFrames_allInsideRatingPeriod
  , testParserUsingAttoParsec
) where

import Asterisell.Cdr
import Asterisell.Error
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy

import Data.List as List
import Control.Monad.State.Strict as State
import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Ord as Ord

import qualified Data.Map.Strict as SMap
import qualified Data.Map as Map
import Data.Vector as V (length)

import qualified Data.ByteString.Lazy as BS
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

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char (ord)

import qualified Test.HUnit as HUnit

import Pipes
import Pipes.ByteString as PB
import qualified Pipes.Prelude as PP
import Pipes.Safe as PS
import qualified Pipes.Safe.Prelude  as PS
import qualified Pipes.Csv as PC
import Pipes.Csv ((.!))
import qualified Data.Csv as CSV

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.Prim((<|>), (<?>))
import Text.Parsec.ByteString.Lazy as Parsec
import qualified Text.Parsec.Char as Parsec

import qualified Data.Attoparsec.Text.Lazy as LazyAttoParsec
import qualified Data.Text.Lazy.IO as LazyIO
import qualified Data.Text.Lazy as LazyText

import System.IO as IO

import Debug.Trace

import Data.Maybe
import Data.Set as Set
import Data.IntMap as IMap
import Data.Hashable
import qualified Data.Serialize as DBS
import qualified Data.Serialize.Text as DBS
import GHC.Generics

import Text.Heredoc
import Control.DeepSeq

--------------------
-- MATCH STRENGHT --
--------------------



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

instance Show RateRole where
  show IncomeRate = "income"
  show CostRate = "cost"

-- | The complete name of a rate, joining all the UserRateRefName of the rate hierarchy.
--   Something like "/root/outgoing/normal-price/mobile-line".
type CompleteUserRateRefName = Text.Text

-- | Decide if a rate can be applied to a CDR.
type FilterFun = RatingEnv -> CDR -> Maybe MatchStrenght



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
filterFun_matchOneOfIds :: [Int] -> (CDR -> Int) -> FilterFun
filterFun_matchOneOfIds ids funId env cdr
  = if List.elem (funId cdr) ids
    then Just matchStrenght_initial
    else Nothing

-- | Decide if a rate can be applied to a CDR,
--   returning also the corresponding CalcParams.
type MatchFun = RatingEnv -> CDR -> Maybe (MatchStrenght, CalcParams)

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
type ReverseRatePathAndCalcParams = [(RateSystemId, String, CalcParams)]

-- | The most specific RateSystemId
reverseRatePath_specificId :: ReverseRatePath -> RateSystemId
reverseRatePath_specificId = List.head

-- | The most specific RateSystemId
reverseRatePathAndCalcParams_specificId :: ReverseRatePathAndCalcParams -> RateSystemId
reverseRatePathAndCalcParams_specificId x
  = let (r, _, _) = List.head x
    in r

-- | A reference to an internal rate.
type InternalRateReference = String

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
    } deriving (Show)

-- | Return the calldate from which you can start with a empty BundleState, because it is the init of bundle timeframe.
--   There can be different BundleState with different time frame.
mainRatePlan_getBundleRateStartCallDate
  :: MainRatePlan
  -> CallDate
     -- ^ at this date you want start the rating, but you are asking if you must include other previous CDRS, before starting with an empty bundle-state
  -> (CallDate
      -- ^ the initial call date where you can start with an empty BundleState.
     , CallDate
       -- ^ before this date (exclusive) you can not save the BundleState, because there can be intra BundleState in dirty status.
       --   After this date you can save the BundleState, because all the intra BundleState are initializated with an empty state, and they are clean.
     )
mainRatePlan_getBundleRateStartCallDate plan rateFromDate
  = let allDates = List.map (\rb -> fst $ timeFrame_fromCallDate rb rateFromDate) (mainRatePlan_bundleRates plan)
    in case List.null allDates of
         True -> (rateFromDate, rateFromDate)
         False -> (List.minimum allDates, List.maximum allDates)

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
  } deriving(Show)

-- | A bundle rate plan, with children.
data BundleRatePlan
  = BundleRatePlan {
      bundle_systemId :: RateSystemId
    , bundle_rateParams :: RateParams
    , bundle_bundleParams :: BundleParams
    , bundle_children :: Either ExternalRateReference [BundleRatePlan]
    } deriving(Show)

-- | BundleParams that can be changed from any root and children bundle-rate.
data BundleParams
  = BundleParams {
      bundle_initialCost :: ! MonetaryValue
    , bundle_minCost :: ! MonetaryValue
    , bundle_leftCalls :: !(Maybe Int)
      -- ^ how many calls can be processed from the bundle.
      --   Nothing if this is not a limit.
    , bundle_leftDuration :: !(Maybe Int)
      -- ^ how many seconds can be processed from the bundle.
      --   Nothing if this is not a limit.
    , bundle_appliedCost :: !MonetaryValue
      -- ^ the cost of calls associated to the BundleRate.
  } deriving(Show, Generic)

instance NFData RatingEnv where
    rnf b = seq b ()

instance NFData BundleParams where
     rnf b = seq b ()

-- | RatePlan specification.
data RatePlan
  = RatePlan {
      rate_systemId :: RateSystemId
    , rate_params :: RateParams
    , rate_elsePart :: [RatePlan]
    , rate_children :: Either ExternalRateReference [RatePlan]
    } deriving(Show)

-- | The params of a normal rate-plan.
data RateParams
  = RateParams {
      rate_userId :: String
    -- ^ user assigned human readable short name/id
    , rate_match :: MatchFun
  } deriving(Show)

-- | The params are the initial limits of the BundleRatePlan, at the beginning of the time-frame.
--   A BundleRate can process all the calls in the TimeFrame.
type BundleRecord = (BundleParams, TimeFrame)

data BundleRateTimeFrame
  = WeeklyTimeFrame Int
    -- 1 for Monday, 7 for Sunday
  | MonthlyTimeFrame Int
  -- ^ start scheduling from the specified day of the month
  | EveryDaysTimeFrame Int CallDate
  -- ^ schedule every specified days, starting from the calldate
 deriving(Eq, Show)

type CountOfCalls = Int

-- | A timeframe
type TimeFrame
  = (CallDate
     -- ^ starting time frame (inclusive)
    , CallDate
    -- ^ ending time frame (exclusive).
    )

type CallDate = LocalTime

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
type DiscardedRate = (String -- ^ the name of the rate
                     , String  -- ^ the reason of discard
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

rootBundleRatePlan_userId :: RootBundleRatePlan -> String
rootBundleRatePlan_userId p = rate_userId $ bundle_rateParams $ bundle_plan p

-- | Add support for binary serialization to LocalTime.
--   TODO use a faster method in future, using directly the fields.
--   TODO try generic approach
instance DBS.Serialize LocalTime where
  put l
    = do DBS.put $ show l

  get
    = do s :: String <- DBS.get
         return $ read s

timeFrames_allInsideRatingPeriod
  :: BundleRateTimeFrame
  -- ^ the schudule to use
  -> CallDate
  -- ^ rate from this date
  -> CallDate
  -- ^ rate to this date
  -> Bool
  -- ^ True for including also open time frames
  -> [TimeFrame]
  -- ^ all the timeframes inside the rating period

timeFrames_allInsideRatingPeriod serviceSchedule rateFromDate rateToDate includeOpenTimeFrames
  = result

 where

   result
     = List.takeWhile isValidTimeFrame $ List.iterate nextTimeFrame $ timeFrame_fromCallDate1 serviceSchedule rateFromDate

   nextTimeFrame :: TimeFrame -> TimeFrame
   nextTimeFrame (start1, end1)
     = timeFrame_fromCallDate1 serviceSchedule end1

   isValidTimeFrame (serviceFromCallDate, serviceToCallDate)
     = case includeOpenTimeFrames of
         True
           -> serviceFromCallDate < rateToDate
         False
           -> serviceToCallDate <= rateToDate

-- | Associate a unique or shared RateSystemId to each part of a MainRatePlan.
--   Share the same RateSystemId with previous defined rates, in order to support
--   the update of BundleRates without loosing previous status.
mainRatePlan_assignSharedUniqueSystemId :: MainRatePlan -> BundleState -> Either String MainRatePlan
mainRatePlan_assignSharedUniqueSystemId mainPlan1 bundleState
  = do
      -- all the open timeframe in bundle-state
      let legacyFromNameToId :: Map.Map CompleteUserRateRefName RateSystemId
            = IMap.foldWithKey (\i (_, name, _) m -> Map.insert name i m) Map.empty bundleState

      -- This is the new rate plan with initial unique system-ids, that can be in conflict with the id of bundleState.
      tempMainPlan
        <- mainRatePlan_assignUniqueSystemId mainPlan1

      let tempFromNameToId :: Map.Map CompleteUserRateRefName RateSystemId
            = IMap.foldWithKey (\i name m -> Map.insert name i m) Map.empty (mainRatePlan_systemIdToUserIdPath tempMainPlan)

      let maxId :: RateSystemId
            = mainRatePlan_maxSystemId tempMainPlan

      -- If there is a common rate name between the bundleState and mainPlain1,
      -- then map the id in mainPlan1 associated to the name, to the id in bundleState.
      -- The id in bundleState can conflict with another id in mainPlan1, so use a new fresh id for it.
      let (substitutions, newMaxId) :: (RatePlanIdMap RateSystemId, RateSystemId)
            = let sharedNames = Map.intersectionWith (\legacyId tempId -> (legacyId, tempId)) legacyFromNameToId tempFromNameToId
              in Map.fold (\ (legacyId, tempId) (substitutions1, maxId1)
                               -> let maxId2 = maxId1 + 1
                                      substitutions2 = IMap.insert legacyId maxId2 substitutions1
                                      substitutions3 = IMap.insert tempId legacyId substitutions2
                                  in  (substitutions3,maxId2)) (IMap.empty, maxId) sharedNames

      let newMainPlan1
            = (processMainRatePlan substitutions tempMainPlan) { mainRatePlan_maxSystemId = newMaxId }

      let missingRates
            = Map.keys (Map.difference legacyFromNameToId tempFromNameToId)

      case missingRates of
        [] -> case mainRatePlan_completeDerivedInfo newMainPlan1 of
                Left err
                  -> throwError err
                Right r
                  -> return r

        _ -> throwError $ "The new rate plan must define also open bundle-rates from the previous rate plan: " ++ List.intercalate ", " (List.map Text.unpack missingRates)

 where

      substitute :: RatePlanIdMap RateSystemId -> RateSystemId -> RateSystemId
      substitute substitutions id1
            = case IMap.lookup id1 substitutions of
                Nothing
                  -> id1
                Just id2
                  -> id2

      -- Apply the substitutions to the tempMainPlan
      processMainRatePlan :: RatePlanIdMap RateSystemId -> MainRatePlan -> MainRatePlan
      processMainRatePlan substitutions p
            = let br = List.map (processRootBundleRatePlan substitutions) (mainRatePlan_bundleRates p)
                  nr = List.map (processNormalRate substitutions) (mainRatePlan_normalRates p)
              in mainRatePlan_empty {
                   mainRatePlan_bundleRates = br
                 , mainRatePlan_normalRates = nr
                 }

      processRootBundleRatePlan :: RatePlanIdMap RateSystemId -> RootBundleRatePlan -> RootBundleRatePlan
      processRootBundleRatePlan substitutions p
            = let br = processBundleRatePlan substitutions (bundle_plan p)
              in  p { bundle_plan = br }

      processBundleRatePlan :: RatePlanIdMap RateSystemId -> BundleRatePlan -> BundleRatePlan
      processBundleRatePlan substitutions p
            = let chs
                    = case bundle_children p of
                        Left e
                          -> Left e
                        Right ch
                          -> Right $ List.map (processBundleRatePlan substitutions) ch
              in p { bundle_systemId = substitute substitutions (bundle_systemId p)
                   , bundle_children = chs
                   }

      processNormalRate :: RatePlanIdMap RateSystemId -> RatePlan -> RatePlan
      processNormalRate substitutions p
            = let chs
                    = case rate_children p of
                        Left e
                          -> Left e
                        Right ch
                          -> Right $ List.map (processNormalRate substitutions) ch
                  elsePart
                    = List.map (processNormalRate substitutions) (rate_elsePart p)

              in p { rate_systemId = substitute substitutions (rate_systemId p)
                   , rate_children = chs
                   , rate_elsePart = elsePart
                   }


-- | Assign a unique RateSystemId to all rates, and complete also other derived info.
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
    = do let name = Text.concat [n, (Text.pack "/"), Text.pack $ rate_userId $ bundle_rateParams p]
         insertUniqueValue (bundle_systemId p) name
         case bundle_children p of
           Left _
             -> return ()
           Right children
             -> mapM_ (processBundleRatePlan name) children

  processNormalRate n p
    = do let name = Text.concat [n, (Text.pack "/"), Text.pack $ rate_userId $ rate_params p]
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
    , info_ratingDetails :: Maybe String
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
discardedRates_toDetails :: [DiscardedRate] -> String
discardedRates_toDetails drs
  = List.concatMap (\(n, reason) -> "\nThe rate " ++ n ++ " was not applied because " ++ reason) drs

-- | Calculate the cost of the rate, updating the BundleState.
--   The algo is composed of two phases:
--   * select the rate to apply
--   * apply the rate
--   The bundle-state is applied only for normal-cdrs:
--   * CDRS cdr_isImportedServiceCDR are not added to BundleState
--   * derived service CDRS are not rated in this phase, but only generated, and this is enforced by algo construction
--
--   @require: bundleState1 is correctly initializated according the time-frame
mainRatePlan_calcCost
  :: Bool
     -- ^ True if it must generate detailed debug info
  -> RateRole
  -> RatingEnv
  -> BundleState
  -> CheckedMainRatePlan
  -> CDR
  -- ^ the call to rate
  -> Either AsterisellError
            ( MonetaryValue
              -- ^ the cost/income of the CDR
            , Maybe BundleRateUnitId
              -- ^ in case of BundleRate application, the used UnitId
            , BundleState
              -- ^ the new BundleState after processing the CDR
            , RatingDebugInfo
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
                                      -> let info2 = info1 { info_residualAppliedRate = Just $ rateSystemId residualRatePath }
                                             info3 = addToDebugInfo info2 unselectedRates2
                                         in Right (bundleCost + (applyNormalRate (s, residualRatePath) (Just residualDuration)), Just bundleRateUnitId, finalBundleState, info3)
                   Right (Nothing, unselectedRates)
                     -> normalResult unselectedRates
                        -- DEV NOTE: reuse the already calculated value, for bundle_onlyForCallsWithACost

 where

  callDate = cdr_calldate cdr

  callBillsec = fromJust1 "rp1" $ cdr_billsec cdr

  organizationsInfo = params_organizations $ env_params env

  cdrOrganizationUnitId = fromJust1 "rp1" $ cdr_organizationUnitId cdr

  normalResult unselectedRates1
    = case normalCost of
        Left err
          -> Left err
        Right (cost, bundleState, selectedRatePath, unselectedRates2)
          -> Right ( cost
                   , Nothing
                   , bundleState
                   , createDebugInfo (rateSystemId selectedRatePath) Nothing (appendIfDebug unselectedRates1 unselectedRates2)
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

  hasNormalCost
    = case normalCost of
        Left _ -> False
        Right (m, _, _, _) -> m > 0

  createRateDetails :: [DiscardedRate] -> Maybe String
  createRateDetails dsr
    = if isDebug then (Just $ discardedRates_toDetails dsr) else Nothing

  createDebugInfo :: RateSystemId -> Maybe RateSystemId -> [DiscardedRate] -> RatingDebugInfo
  createDebugInfo sId msId dsr
    = (ratingDebugInfo_empty sId msId) { info_ratingDetails = createRateDetails dsr }

  addToDebugInfo :: RatingDebugInfo -> [DiscardedRate] -> RatingDebugInfo
  addToDebugInfo info dsr
    = let details1 = info_ratingDetails info
          details2 = createRateDetails dsr
          details3 = case (details1, details2) of
                       (Nothing, Nothing) -> Nothing
                       (Just s1, Nothing) -> Just s1
                       (Just s1, Just s2) -> Just (s1 ++ s2)
                       (Nothing, Just s2) -> Just s2

      in  info { info_ratingDetails = details3 }

  -- | Reduce the load on the system, collecting info only in debug mode. Haskell lazy feature, can reduce the needing for this,
  --   but I'm using it as safety measure.
  appendIfDebug :: [DiscardedRate] -> [DiscardedRate] -> [DiscardedRate]
  appendIfDebug a b = if isDebug then (a ++ b) else []

  -- | The rate name to use for debug info.
  ratePathName :: ReverseRatePathAndCalcParams -> String
  ratePathName rs
    = List.concatMap (\(_,n, _) -> "/" ++ n) (List.reverse rs)

  -- | Return the first RateSystemId that is not 0.
  --   There can be RateSystemId at 0 because they are the referenced external rates.
  --   @require not $ List.null n
  rateSystemId :: ReverseRatePathAndCalcParams -> RateSystemId
  rateSystemId n@((i, _, _):rest) = if (i == 0) then rateSystemId rest else i

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
                           -> Right (maybeResult, appendIfDebug unselectedRates1 unselectedRates2)

  -- | Select the BundleRate associated to a BundleRateUnitId, that can be a parent of the CDR UnitId.
  selectBundleRateForUnitId :: BundleRateUnitId -> SelectedBundleRateOrError
  selectBundleRateForUnitId bundleRateUnitId
    = case info_getDirectPriceCategory organizationsInfo bundleRateUnitId callDate of
        Nothing
          -> Right (Nothing, [("all bundle rates", "the organization " ++ show bundleRateUnitId ++ " has no direct price category assignation.")])
        Just priceCategoryId
          -> case findBundle priceCategoryId of
              Right Nothing
                -> Right (Nothing, [("all bundle rates", "there is no bundle rate with price category " ++ show priceCategoryId)])
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
                      = rateCategories_code (params_rateCategories $ env_params env) priceCategoryId
                in Left $ createError
                            Type_Error
                            Domain_RATES
                            ("conflicting bundle rates on price category - " ++ show rateRole ++ "-" ++ show priceCategoryId)
                            ("There is more than one " ++ show rateRole ++ " bundle rate on price-category \"" ++ priceCategoryName ++ "\", during rating of CDR " ++ rate_showDebug env cdr)
                            ("All similar CDRs can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                            ("Correct the rate specification, or contact the assistance.")

  selectInsideMainBundleRateForUnitId :: RootBundleRatePlan -> BundleRateUnitId -> PriceCategoryId -> SelectedBundleRateOrError
  selectInsideMainBundleRateForUnitId mainPlan bundleRateUnitId priceCategoryId
    = case (bundle_onlyForCallsWithACost mainPlan && (not hasNormalCost)) of
        True
          -> Right (Nothing, [(rootBundleRateUserName, "the call has cost 0")])

        False
          -> case selectCurrentRateOrBestChild (matchStrenght_initial, [], (callBillsec, 0)) (Just $ Left (mainPlan, bundleRateUnitId, bundle_plan mainPlan)) children of
               Left err
                 -> Left err
               Right (Nothing, unselectedRates)
                 -> Right (Nothing, unselectedRates)
               Right (Just r, unselectedRates)
                 -> Right $ (Just r, unselectedRates)

   where

     rootBundleRateUserName = "/" ++ rootBundleRatePlan_userId mainPlan
     children = case bundle_children $ bundle_plan mainPlan of
                  Left r -> Left r
                  Right r -> Right $ List.map Left r

  selectNormalRate :: SelectedBundleRateOrError
  selectNormalRate
    = case selectCurrentRateOrBestChild (matchStrenght_initial, [], (callBillsec, 0)) Nothing (Right $ List.map Right $ mainRatePlan_normalRates mainRatePlan) of
         Right (Nothing, unselectedRates)
          -> Left $ createError
                      Type_Error
                      Domain_RATES
                      ("missing rate for - " ++ show rateRole ++ "-" ++ (cdr_missingRateKey cdr))
                      ("There is no " ++ show rateRole ++ " (no-bundle) rate for rating CDRs like " ++ rate_showDebug env cdr ++ "\n\n" ++ discardedRates_toDetails unselectedRates)
                      ("All similar CDRs can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                      ("Correct the rate specification, or contact the assistance.")
         r -> r

  -- | Select the specified rate if applicable, or the best of its children, in a recursive way.
  --   This function has "strange" parameters, because in this way I can reuse in many points of the code,
  --   both for bundle rates and normal rats, and in recursive calls.
  selectCurrentRateOrBestChild
     :: (MatchStrenght, ReverseRatePathAndCalcParams, CallDuration)
        -- ^ the parent path. So this does not comprise current rate
     -> Maybe (Either (RootBundleRatePlan, BundleRateUnitId, BundleRatePlan) RatePlan)
        -- ^ the current rate to evaluate. Nothing for root rate.
     -> Either ExternalRateReference [Either BundleRatePlan RatePlan]
        -- ^ the children of the current rate to evaluate
        -- @require children are the children of maybeCurrentRate
        -- @require if maybeCurrentRate is Nothing, then this is the list of all root rates that are non-bundles
     -> SelectedBundleRateOrError
        -- ^ the best inner-most selected rate

  selectCurrentRateOrBestChild (parentStrenght, parentPath, callDuration) maybeCurrentRate currentChildren
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
                             in  (ratePathName parentPath ++ "/" ++ userId, userId, systemId)
                        Left (mainRatePlan, unitId, currentPlan)
                          -> let userId = rate_userId $ bundle_rateParams currentPlan
                                 systemId = bundle_systemId currentPlan
                             in  (ratePathName parentPath ++ "/" ++ userId, userId, systemId)

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
                               -> case selectCurrentRateOrBestChild (parentStrenght, parentPath, callDuration) Nothing (Right elseParts) of
                                    Left err
                                      -> Left err
                                    Right (Nothing, unselectedRates2)
                                      -> Right (Nothing, appendIfDebug unselectedRates1 unselectedRates2)
                                    Right (Just finalResult, unselectedRates2)
                                      -> Right (Just finalResult, appendIfDebug unselectedRates1 unselectedRates2)

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
                                       -> case selectCurrentRateOrBestChild (parentStrenght, parentPath, callDuration) Nothing (Right elseParts) of
                                            Left err
                                              -> Left err
                                            Right (Nothing, unselectedRates2)
                                              -> Right (Nothing, appendIfDebug unselectedRates1 unselectedRates2)
                                            Right (Just finalResult, unselectedRates2)
                                              -> Right (Just finalResult, appendIfDebug unselectedRates1 unselectedRates2)

                              errorOrResult -> errorOrResult
    where

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
                                         in  selectCurrentRateOrBestChild (matchStrenght_initial, currentPath, callDuration) (Just c) cs
                                             -- DEV-NOTE: I'm using a 0 strenght, because I want select the best matching child, before combining with the strenght of current rate
                                  ) children


                     selectChild :: SelectedBundleRateOrError -> SelectedBundleRateOrError -> SelectedBundleRateOrError
                     selectChild (Left err1) (Left err2) = (Left err1)
                     selectChild (Left err1) (Right _) = (Left err1)
                     selectChild (Right _) (Left err2) = (Left err2)
                     selectChild (Right (Nothing, unselectedRates1)) (Right (result2, unselectedRates2))
                       = Right (result2, appendIfDebug unselectedRates1 unselectedRates2)
                     selectChild (Right (result1, unselectedRates1)) (Right (Nothing, unselectedRates2))
                       = Right (result1, appendIfDebug unselectedRates1 unselectedRates2)
                     selectChild (Right (Just ((oldStrenght, oldPath, oldDuration), unitId), unselectedRates1)) (Right (Just ((newStrenght, newPath, newDuration), _), unselectedRates2))
                       = let unselectedRates3 = appendIfDebug unselectedRates1 unselectedRates2
                         in case match_ord oldStrenght newStrenght of
                              Just EQ
                                -> let name1 = ratePathName oldPath
                                       name2 = ratePathName newPath
                                   in Left $ createError
                                               Type_Error
                                               Domain_RATES
                                               ("virtual rate conflict - " ++ show rateRole ++ "-" ++ name1 ++ "-" ++ name2)
                                               ("The " ++ show rateRole ++ " rate \"" ++ name1 ++ "\", conflicts with rate \"" ++ name2 ++ "\", because it matches the CDR with the same strenght, and the system have no hints about which is the best rate to apply. The CDR has calldate is " ++ showLocalTime (cdr_calldate cdr) ++ ". The CDR content, after import, and initial processing is " ++ rate_showDebug env cdr)
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
                                   else (("The " ++ show rateRole ++ " rate " ++ rateName ++ " is applicable ")
                                        ,("\n\nbut none of its more specific children rates can be applied.\nFor avoiding ambiguites in rating specifications, one and only one children rate must be selected, but in this case none can be selected.\n\n"))

                           in if isNormalRate
                              then (Left $ createError
                                             Type_Error
                                             Domain_RATES
                                             ("not applicale children rate - " ++ show rateRole ++ "-" ++ rateName)
                                             (errorMessage1 ++ " to a CDR like this: \n" ++ rate_showDebug env cdr ++ errorMessage2)
                                             ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                                             ("Improve the rate specification."))
                              else (Right (Nothing, unselectedRates))

                      Right (Just ((bestChildrenStrenght, bestChildrenPath, bestDuration), unitId), unselectedRates)
                        -> Right (Just ((match_combineWithChild currentStrenght bestChildrenStrenght, bestChildrenPath, bestDuration), unitId), unselectedRates)

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

  -- | Given an ExternalRateReference, retrieve the external children.
  getExternalChildren :: ExternalRateReference -> Either AsterisellError [RatePlan]
  getExternalChildren externalRateReference
    = case env_getRate env externalRateReference callDate of
        Nothing
          -> Left $ createError
                      Type_Error
                      Domain_RATES
                      ("unknown external rate name - " ++ show rateRole ++ "-" ++ externalRateReference)
                      ("The " ++ show rateRole ++ " external rate reference \"" ++ externalRateReference ++ "\", does not exists at date " ++ showLocalTime callDate ++ ". The CDR content is " ++ rate_showDebug env cdr)
                      ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                      ("Correct the rate specification, or contact the assistance.")

        Just (mainRatePlan, _)
          -> case List.null (mainRatePlan_bundleRates mainRatePlan) of
               False
                 -> Left $ createError
                             Type_Error
                             Domain_RATES
                             ("external rate name with bundle params - " ++ show rateRole ++ "-" ++ externalRateReference)
                             ("The " ++ show rateRole ++ " external rate reference \"" ++ externalRateReference ++ "\", exists at date " ++ showLocalTime callDate ++ ", but it has bundle children, and it can not be embeded correctly in a rate plan.")
                             ("CDRS can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                             ("Correct the rate specification, or contact the assistance.")
               True
                 -> Right $ mainRatePlan_normalRates mainRatePlan

debug_showRateSystemIds :: [RatePlan] -> String
debug_showRateSystemIds rs = List.concat $ List.intersperse ", " $ List.map show $ List.map rate_systemId rs

-----------------------
-- BUNDLE RATE STATE --
-----------------------

-- | LocalTime greather or equal to this value,
--   are to be considered in the next bundle-rate scheduling timeframe.
type NextScheduledLocalTime = CallDate

-- | A Map associating a rate_systemId with some value.
--   An IntMap is rather fast, because it is specialized for Int.
type RatePlanIdMap a = IMap.IntMap a

ratePlanIdMap_empty :: RatePlanIdMap a
ratePlanIdMap_empty = IMap.empty

-- | Store a state for each BundleRate, and for each UnitId.
--   The state is modified every time a CDR is processed.
type BundleState = RatePlanIdMap (NextScheduledLocalTime, CompleteUserRateRefName, UnitIdMap [BundleRecord])

bundleState_empty :: BundleState
bundleState_empty = IMap.empty

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

-- TODO consider using a sorted set, for fast lookup, and mantained in synchro every time
-- we add or remove an element, also because this function is called for every CDR
bundleState_minimumNextScheduledLocalTime :: BundleState -> Maybe NextScheduledLocalTime
bundleState_minimumNextScheduledLocalTime s
  = IMap.foldl' (\mT1 (t2, _, _)
                  -> case mT1 of
                       Nothing -> Just t2
                       Just t1 -> Just $ min t1 t2
               ) Nothing s

bundleState_debugShow :: BundleState -> String
bundleState_debugShow state1
  = IMap.foldlWithKey'  (\s k (d, _, _) -> s ++ ", " ++ show k ++ " -> " ++ showLocalTime d) "" state1

-- | Close the time-frames, and open new-time frames, acconding the specified call-date.
--   This is a fast operation to do, and it must be performed for every CDR.
--   This function coordinates all other functions calculating the bundle-rate,
--   and it is the top-down entry.
--   DEV NOTES: the code of this function must be fast, because it is called for each CDR.
bundleState_serviceCdrs
  :: RatingEnv
  -> MainRatePlan
  -> BundleState
  -> CallDate
  -- ^ the call date of the next CDR to process.
  --  It supposes that the CDRS are processed ordered by calldate,
  --  and so this calldate can be used for determining which bundle rates can be closed.
  -> ( Maybe CallDate
       -- ^ the calldate of the previous closed bundle-state if a new BundleState was opened.
     , BundleState
     -- ^ open new bundle rates, in case
     , [ServiceCDR]
     -- ^ the ServiceCDRs of the closed bundle rates
     )

bundleState_serviceCdrs env mainRatePlan state0 callDate
  = fRec state0

 where

  fRec :: BundleState -> (Maybe CallDate, BundleState, [ServiceCDR])
  fRec state1
    = case bundleState_minimumNextScheduledLocalTime state1 of
        Nothing
          -> (Nothing, state1, [])
        Just minCallDate
          -> if callDate >= minCallDate
             then let serviceCdrs2 = generateServiceCDRs state1 minCallDate
                      state2 = bundleState_initAccordingRatingEnv env mainRatePlan (Right minCallDate) state1
                      (_, state3, serviceCdrs3) = fRec state2
                      -- call recursively until the calldate is not reached. In this way if there are holes in the rating, procces,
                      -- the ServiceCDRS are generated for all missing timeframes.
                  in  (Just minCallDate, state3, serviceCdrs2 ++ serviceCdrs3)
             else (Nothing, state1, [])
                  -- the CDR is inside an open time-frame.

  -- | Process a bundle-rate time frame, generating the associated ServiceCDRs.
  generateServiceCDRs
    :: BundleState
    -> NextScheduledLocalTime
       -- ^ the time frame to process
    -> [ServiceCDR]

  generateServiceCDRs state1 startCallDate
    = let
          -- | Process a bundle rate.
          processBundleRate :: RootBundleRatePlan -> [ServiceCDR]
          processBundleRate rate
            = let rateId = bundle_systemId $ bundle_plan rate

                  (rateNextScheduled, _, _) = fromJust1 "rp7" $ IMap.lookup rateId state1

                  closedServices = bundleState_closeServiceCDRsAccordingRatePlan env mainRatePlan rate state1 (Just startCallDate)

              in  if rateNextScheduled == startCallDate
                  then closedServices
                  else []
                       -- some main bundle-rates can have different time-frames (shorter or longer)
                       -- than current considered time-frame. So they are skipped, because they will be
                       -- processed at the next recursive calls of `recursiveSolution`,
                       -- or because they were already processed.

      in  List.concatMap processBundleRate (mainRatePlan_bundleRates mainRatePlan)
          -- process each main bundle rate

-- | Generate all pending service CDRS for open time-frames of bundle-rates.
--   These ServiceCDRS are related to not yet closed time-frames, so they are temporary/pending.
--   DEV NOTES: this function can be not fast, because it is called only one time, at the end of rating process.
bundleState_pendingServiceCdrs
  :: RatingEnv
  -> MainRatePlan
  -> BundleState
  -> [ServiceCDR]

bundleState_pendingServiceCdrs env mainRatePlan state0
  = let
          processBundleRate :: RootBundleRatePlan -> [ServiceCDR]
          processBundleRate rate
            = bundleState_closeServiceCDRsAccordingRatePlan env mainRatePlan rate state0 Nothing

    in  List.concatMap processBundleRate (mainRatePlan_bundleRates mainRatePlan)

bundleParams_scale :: BundleParams -> Rational -> BundleParams
bundleParams_scale r p
  = if p == 1
    then r
    else r {
             bundle_leftDuration = scaleI (bundle_leftDuration r)
           , bundle_leftCalls = scaleI (bundle_leftCalls r)
           , bundle_initialCost = (bundle_initialCost r) * p
           , bundle_minCost = (bundle_minCost r) * p
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
      EveryDaysTimeFrame deltaDays d0
        -> -- DEV NOTE: this code/timeframe up to date not tested and not supported.
           -- TODO support this
           let deltaSeconds :: NominalDiffTime = fromInteger $ (toInteger deltaDays) * 24 * 60 * 60
               dd0 = localTimeToUTC utc d0
               dd1 = localTimeToUTC utc callDate
               totalDiff = diffUTCTime dd1 dd0
               totalDays = totalDiff / deltaSeconds

               lapsedCompleteTimeFrame = totalDays / (fromInteger $ toInteger deltaDays)
               startingCallDateOfCurrentTimeFrame = addUTCTime (lapsedCompleteTimeFrame * deltaSeconds) dd0
               endingCallDateOfCurrentTimeFrame = addUTCTime deltaSeconds startingCallDateOfCurrentTimeFrame
           in (utcToLocalTime utc startingCallDateOfCurrentTimeFrame, utcToLocalTime utc endingCallDateOfCurrentTimeFrame)

      MonthlyTimeFrame dayOfMonth
        -> let (yyyy, mm, dd) = toGregorian $ localDay callDate

               (newYYYY, newMM) = if mm < 12 then (yyyy, mm + 1) else (yyyy + 1, 1)
               (oldYYYY, oldMM) = if mm > 1 then (yyyy, mm - 1) else (yyyy - 1, 12)

               newDate = fromGregorian newYYYY newMM dayOfMonth
               oldDate = fromGregorian oldYYYY oldMM dayOfMonth
               currDate = fromGregorian yyyy mm dayOfMonth

           in if dd < dayOfMonth
              then (toCallDate oldDate, toCallDate currDate)
              else (toCallDate currDate, toCallDate newDate)

      WeeklyTimeFrame dayOfWeek
        -> let (_, _, cdrDayOfWeek) = toWeekDate (localDay callDate)

               diffDays = dayOfWeek - cdrDayOfWeek
               -- from cdr date to the day of week when start the new timeframe (it can be negative or positive)

               startDate = addDays (toInteger diffDays) (localDay callDate)

               prevDate = addDays (-7) startDate
               nextDate = addDays 7 startDate

           in if diffDays <= 0
              then (toCallDate startDate, toCallDate nextDate)
              else (toCallDate prevDate, toCallDate startDate)

 where

  toCallDate :: Day -> CallDate
  toCallDate d = LocalTime {
                   localDay = d
                 , localTimeOfDay = midnight
                 }


-- | Try to apply a bundle rate to a CDR, considering only the bundle limits conditions, and without considering the children rates,
--   and other rate params.
--
--   DEV NOTES: this code must be fast because it is called for each CDR.
--   @require: the CDR calldate is inside the BundleState time-frame, but not necessaril the unit-id
--   @require: BundleState is correctly initializated according the time-frame
bundle_canBeApplied
  :: RootBundleRatePlan
  -> BundleRatePlan
  -> BundleState
  -> (CDR,  BundleRateUnitId, BundleCallDuration)
  -- ^ the CDR to rate, the unit id owning the BundleRate, the CallDuration to apply
  -> Maybe BundleCallDuration
  -- ^ Nothing if the rate can not be applied, the duration of the call that can be rated from the bundle rate otherwise

bundle_canBeApplied rootBundleRate bundlePlan state1 (cdr, unitId, duration1)
  = case IMap.lookup unitId unitRecs of
      Nothing
        -> Nothing
           -- the unit-id has no associated bundle-state
      Just [(bundleParams, (unitStartingCallDate, unitEndingCallDate))]
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
    = case IMap.lookup rateId state1 of
        Just r -> r
        Nothing -> error $ "error code b2: rateId " ++ show rateId ++ ",\nfor rate " ++ show bundlePlan ++ "\non state " ++ show state1

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

-- | Create a BundleRecord for a RootBundleRatePlan, and its children.
--   DEV NOTE: it works on children, because they share the same time-frame.
--
--   Take in consideration the activation date of a UnitId inside a bundle-time frame.
--   By design, it does not consider the ending date of a UnitId.
--
--   DEV NOTE: this function is called only at the end of each bundle time frame, so it can be also slow.
rootBundleRate_initBundleRecord
  :: RatingEnv
  -> MainRatePlan
  -> RootBundleRatePlan
  -> NextScheduledLocalTime
  -- ^ the starting date of the time-frame
  -> BundleState

rootBundleRate_initBundleRecord env mainRatePlan rootBundleRate fromCallDate
  = List.foldl' processBundleRate IMap.empty (rates $ bundle_plan rootBundleRate)

 where

  rates :: BundleRatePlan -> [BundleRatePlan]
  rates p
    = case bundle_children p of
        Left _
          -> [p]
             -- do not follow the external rate, and process only the root bundle rate plan
        Right children
          -> [p] ++ List.concatMap (\c -> rates c) children
             -- process both the root bundle rate plan, and its children, and the children of the children

  (bundleFromCallDate, toCallDate) = timeFrame_fromCallDate rootBundleRate fromCallDate

  timeFrameDuration = timeFrame_duration (bundleFromCallDate, toCallDate)

  unitInfo = params_organizations $ env_params env

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
                              -> List.map (\d -> unit_id d) l
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
  processBundleRate :: BundleState -> BundleRatePlan -> BundleState
  processBundleRate rec1 ratePlan
    = let rateId = bundle_systemId ratePlan
          rateName = fromJust1 "rp10" $ IMap.lookup rateId (mainRatePlan_systemIdToUserIdPath mainRatePlan)
          bundleParams = bundle_bundleParams ratePlan

          createBundleRecord :: CallDate -> [BundleRecord]
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


              in [(params2, (bundleFromCallDate, toCallDate))]

      in IMap.insert rateId (toCallDate, rateName, IMap.map createBundleRecord activatedUnitIds) rec1

-- | Init a BundleState with the initial values according the CallDate.
--
--   BundleState is a map.
--
--   > RateId -> UnitId -> BundleState.
--
--   Update the bundle state in this way:
--   * close time frames closed at NextScheduledLocalTime,
--   * create a new time frame for each new RateId,
--   * create a new time frame for each new UnitId.
--
--   Add an initial BundleState for every RootBundleRatePlan, that is a new rate, or a rate with a closed time-frame.
--   Leave the already present BundleState otherwise.
--
--   The UnitId are initializated in a way proportional to their presence in the bundle time frame,
--   but only related to the date of when they enter in the bundle,
--   not the date from when they leave the time-frame.
--   This is sound, because a UntId can enter in a bundle, use all its resources,
--   then exit, but he must pay all the bundle, because the exit date is not so significative.
--   NOTE: in reality if also the limits of the bundle are proportional to the exit date,
--   it can make sense, as in case of initial entering date.
--   DEV NOTES: this function is called only at the end of a time-frame, so it can be also slow,
--   because it is called some order of magnitude less than CDR processing functions.
bundleState_initAccordingRatingEnv
  :: RatingEnv
  -> CheckedMainRatePlan
  -> Either NextScheduledLocalTime NextScheduledLocalTime
  -- ^ Right for the time frame to close: delete all time frame closed at this date, and open new time frames. This must be exactly the calldate
  --   of the time frame to close.
  --
  --   Left for creating a BundleState from an empty BundleState.
  --   Usually it is called only the first time Asterisell run.
  --   Consider as start timeframe the nearest timeframe, containing the specified CallDate, according the time frame of the rate.
  --   In this way all CDRS from this date, to next dates, can be processed, and they can update the BundleState.
  -> BundleState
  -> BundleState

bundleState_initAccordingRatingEnv env mainRatePlan referenceCallDate state1
  = List.foldl' initRootRate state2 (mainRatePlan_bundleRates mainRatePlan)
 where

 toName :: RateSystemId -> CompleteUserRateRefName
 toName rateId
   = fromJust1 "rp11" $ IMap.lookup rateId (mainRatePlan_systemIdToUserIdPath mainRatePlan)

 -- Remove all closed time bundle-state time-frames, from state1.
 state2 :: BundleState
 state2
   = case referenceCallDate of
       Left _
         -> state1
       Right endingTimeFrame
         -> IMap.filter (\(endCallDate, _, unitsState) -> not (endCallDate == endingTimeFrame)) state1

 -- | Insert in BundleState a new RootBundleRatePlan.
 initRootRate :: BundleState -> RootBundleRatePlan -> BundleState
 initRootRate state1 bundleRate
   = let rateId = bundle_systemId $ bundle_plan bundleRate

         endingTimeFrame
           = case referenceCallDate of
               Right r
                 -> r
               Left d
                 -> fst $ timeFrame_fromCallDate bundleRate d

     in  case IMap.lookup rateId state1 of
           Just _
             -> state1
           Nothing
             -> let initState2 = rootBundleRate_initBundleRecord env mainRatePlan bundleRate endingTimeFrame
                in  IMap.union initState2 state1

-- | Generate ServiceCDRs for a BundleRate timeframe that can be considered closed.
--   Group and sum all the service-cdrs associatet to the same organization and RootBundleRatePlan, generating only one service-cdr.
--   In case of services generated for open bundle-rates, it uses as to_calldate, the calldate of the next time frame, for being sure that
--   the service-cdr will be deleted every time a new rating process start. Otherwise service CDRs can be generated repeated:
--   * one time for open bundle rates
--   * another time when the bundle-rate is closed
--   * (see #1341 for more details)
--   DEV NOTE: this function it is not needed to be very fast, because it is called only at the end of the time frame
bundleState_closeServiceCDRsAccordingRatePlan
  :: RatingEnv
  -> CheckedMainRatePlan
  -> RootBundleRatePlan
  -> BundleState
  -> Maybe NextScheduledLocalTime
  -- ^ the calldate of the new timeframe. All the ServiceCDRs until this date must be closed.
  --   If Nothing generate ServiceCDRS for all open bundle state, because it must be generate
  --   temporary/pending ServiceCDRS.
  -> [ServiceCDR]
  -- ^ the associated ServiceCDRs

bundleState_closeServiceCDRsAccordingRatePlan env mainRatePlan rootBundleRate state1 maybeTimeFrameToClose
  = Map.elems $ Map.map (fromJust1 "rp12") $ Map.filter (isJust) serviceCDRS

  where

    rootId = bundle_systemId $ bundle_plan rootBundleRate
    rootName = fromJust1 "rp13" $ IMap.lookup rootId $ mainRatePlan_systemIdToUserIdPath mainRatePlan
    precision = params_currencyPrecision $ env_params env

    groupedIncomes :: Map.Map (UnitId, TimeFrame) (MonetaryValue, Int)
    groupedIncomes
      = IMap.foldlWithKey' groupFilteringRate Map.empty state1

    groupFilteringRate :: Map.Map (UnitId, TimeFrame) (MonetaryValue, Int) -> RateSystemId -> (NextScheduledLocalTime, CompleteUserRateRefName, UnitIdMap [BundleRecord]) -> Map.Map (UnitId, TimeFrame) (MonetaryValue, Int)
    groupFilteringRate g1 rateId (nextTimeFrame, completeName, unitRecords)
      = let closedTimeFrame
              = case maybeTimeFrameToClose of
                  Nothing
                    -> True
                  Just timeFrameToClose
                    -> nextTimeFrame == timeFrameToClose

            rootIdOfRateId = bundle_systemId $ bundle_plan $ fromJust1 "rp14" $ IMap.lookup rateId $ mainRatePlan_systemIdToRootBundleRate mainRatePlan

        in if rootIdOfRateId == rootId && closedTimeFrame
           then IMap.foldlWithKey' (groupByUnit nextTimeFrame) g1 unitRecords
             -- process each unit-id
           else g1
                -- skip this

    groupByUnit :: CallDate -> Map.Map (UnitId, TimeFrame) (MonetaryValue, Int) -> UnitId -> [BundleRecord] -> Map.Map (UnitId, TimeFrame) (MonetaryValue, Int)
    groupByUnit callTime g1 unitId [(bundle, (startTimeFrame, endTimeFrame))]
      = let cost1 = (bundle_minCost bundle) - (bundle_appliedCost bundle)
            cost2 = if cost1 > 0 then cost1 else 0
            cost3 = cost2 + (bundle_initialCost bundle)

            isExpectedStartTimeFrame1
              = case maybeTimeFrameToClose of
                  Nothing
                    -> True
                  Just timeFrameToClose
                    -> timeFrameToClose == startTimeFrame

            isExpectedStartTimeFrame2
              = callTime == startTimeFrame

            endTimeFrameToUse
              = case maybeTimeFrameToClose of
                  Nothing
                    -> snd $ timeFrame_fromCallDate rootBundleRate endTimeFrame
                  Just _
                    -> endTimeFrame

            g2 = Map.insertWith (\(o1, o2) (n1, n2) -> (o1 + n1, o2 + n2)) (unitId, (startTimeFrame, endTimeFrameToUse)) (cost3, 1) g1

        in  Exception.assert (isExpectedStartTimeFrame1 && isExpectedStartTimeFrame2) g2

    serviceCDRS :: Map.Map (UnitId, TimeFrame) (Maybe ServiceCDR)
      = Map.mapWithKey generateByUnitId groupedIncomes

    generateByUnitId :: (UnitId, TimeFrame) -> (MonetaryValue, Int) -> Maybe ServiceCDR
    generateByUnitId (unitId, (startTimeFrame, endTimeFrame)) (cost, count)
      = let
            externalTelephoneNumber
              = serviceCdr_defaultExternalTelephoneNumber (bundle_serviceCDRType rootBundleRate) (bundle_serviceCDRDescription rootBundleRate)
            -- NOTE: the rate compilation procedure generate a telephone prefix associated to this telephone number.

            cdr = (cdr_empty startTimeFrame precision) {
                    cdr_countOfCalls = count
                  , cdr_toCalldate = Just endTimeFrame
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

-- | Update the bundle-map applying a BundleCallDuration.
--   DEV NOTES: this code must be fast because it is called for each CDR.
--   @require BundleCallDuration can be applied enterily to the BundleState, because it is already managed from bundle_canBeApplied function.
bundleState_update
  :: BundleState
  -> BundleRateSystemId
  -> BundleRateUnitId
  -> CountOfCalls
  -> BundleCallDuration
  -> MonetaryValue
  -> BundleState

bundleState_update bundleState1 rateId unitId countOfCalls callDuration cost
  = IMap.insert rateId (date1, rateName, IMap.insert unitId [(rec2, date2)] units1) bundleState1

 where

  (date1, rateName, units1) = fromJust1 "rp15" $ IMap.lookup rateId bundleState1

  [(rec1, date2)] = fromJust1 "rp16" $ IMap.lookup unitId units1

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

--------------------------
-- CALC CDR COST/INCOME --
--------------------------

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
calcParams_overrideList [] = error "unexpected condition"
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
                                    in t1 * delta
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

----------------
-- ENV PARAMS --
----------------

data EnvParams
  =  EnvParams {
      params_rateCategories :: ! RateCategories
    , params_vendors :: ! Vendors
    , params_channelTypes :: ! ChannelTypes
    , params_channelDomains :: ! ChannelDomains
    , params_telephonePrefixes :: ! TelephonePrefixes
    , params_organizations :: ! Info
    , params_numberOfDigitsToMask :: Int
    , params_defaultTelephonePrefixToNotDisplay :: Maybe String
    , params_currencyPrecision :: Int
    } deriving(Show)

initialClassificationParams_empty
  = EnvParams {
      params_rateCategories = rateCategories_empty
    , params_vendors = Map.empty
    , params_channelTypes = Map.empty
    , params_channelDomains = trie_empty
    , params_telephonePrefixes = trie_empty
    , params_organizations = info_empty
    , params_numberOfDigitsToMask = 0
    , params_defaultTelephonePrefixToNotDisplay = Nothing
    , params_currencyPrecision = 4
  }

params_respectCodeContracts :: EnvParams -> Either String ()
params_respectCodeContracts p
  = do info_respectCodeContracts (params_organizations p)
       return ()

-------------------------
-- SERVICE CDRS EXPORT --
-------------------------

-- | A CSV format used internally from Asterisell.
data CSVFormat_TelephonePrefix1
  = CSVFormat_TelephonePrefix1 {
      t1_prefix :: Text.Text
    , t1_matchOnlyExactDigits :: Int
    , t1_name :: Text.Text
    , t1_geographic_location :: Text.Text
    , t1_operator_type :: Text.Text
    , t1_display_priority :: Int
 } deriving(Show)

instance CSV.ToRecord CSVFormat_TelephonePrefix1 where
     toRecord p
       = CSV.record [
           CSV.toField $ t1_prefix p
         , CSV.toField $ t1_matchOnlyExactDigits p
         , CSV.toField $ t1_name p
         , CSV.toField $ t1_geographic_location p
         , CSV.toField $ t1_operator_type p
         , CSV.toField $ t1_display_priority p
         ]

instance CSV.FromRecord CSVFormat_TelephonePrefix1 where
     parseRecord v
         | V.length v == 6
              = CSVFormat_TelephonePrefix1 <$>
                v .! 0 <*>
                v .! 1 <*>
                v .! 2 <*>
                v .! 3 <*>
                v .! 4 <*>
                v .! 5
         | otherwise = mzero

-- | Export the telephone prefixes to use for service-cdrs.
mainRatePlan_exportToCSVTheServiceCDRSTelephonePrefixes :: MainRatePlan -> BS.ByteString
mainRatePlan_exportToCSVTheServiceCDRSTelephonePrefixes mp
  = CSV.encode $ List.map f $ mainRatePlan_bundleRates mp
 where

  f :: RootBundleRatePlan -> CSVFormat_TelephonePrefix1
  f p
    = let sName = bundle_serviceCDRDescription p
          sType = bundle_serviceCDRType p
      in CSVFormat_TelephonePrefix1 {
              t1_prefix = serviceCdr_defaultExternalTelephoneNumber sType sName
            , t1_matchOnlyExactDigits = 1
            , t1_name = sName
            , t1_geographic_location = sName
            , t1_operator_type = sType
            , t1_display_priority = 10
           }

-----------------------------------------
-- IMPORT LIST OF RATES FROM PHP WORLD --
-----------------------------------------

-- | The ID used in PHP world: "ar_rate.id" field value.
--   Every PHP rate, has a unique RateId.
--   Note that RatePlan can be composed of nested rates, having many RateSystemId
--   that are id in a different namespace, and with the RatePlan scope.
type DBRateId = Int

-- | Store all the rate plans that must be used in current rating pass.
--   Because rating passage are using a limited time-frame, it is feasible
--   loading all the rates in advance.
type CachedDBRates = SMap.Map DBRateId MainRatePlan

cachedRates_empty = SMap.empty

-- | "ar_rate.internal_name" used from user for identifying a rate
--   and its successive versions, in a unique name.
--   There can be different rates with the same ReferenceName,
--   but they are valid on distinct time-frames.
type DBRateReferenceName = String

-- | Store for each DBRateReferenceName, the date from wich is valid,
--   in order to load it if it is not appropiate for the CDR to rate.
--   @ensure: the list of LocalTime and DBRateId are in descending order of date.
type DBRateChanges = SMap.Map DBRateReferenceName [(LocalTime, DBRateId)]

rateChanges_empty :: DBRateChanges
rateChanges_empty = Map.empty

rateChanges_respectCodeContracts :: DBRateChanges -> Either String ()
rateChanges_respectCodeContracts map1
  = let map2 = Map.map (\l1 -> isDescendingOrder $ List.map fst l1) map1
    in  case List.all id (List.map snd $ Map.toList map2) of
          True -> Right ()
          False -> Left $ dbc_error "rp105"

-- | Add a value into the head of the list.
--   @require: recent values are inserted before older values (descending order on application date)
--   @ensure: recent values are near the head of the list
rateChanges_insert :: DBRateChanges -> DBRateReferenceName -> DBRateId -> LocalTime -> DBRateChanges
rateChanges_insert changes rateRef rateId localTime
  = Map.insertWith (\newValue oldValue -> oldValue ++ newValue) rateRef [(localTime, rateId)] changes

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

-- | Used for annotating in the type system, an automatic conversion from LocalTime.
data LocalTimeFromMySQLTimeStamp = LocalTimeFromMySQLTimeStamp LocalTime

instance Show LocalTimeFromMySQLTimeStamp where
  show (LocalTimeFromMySQLTimeStamp t) = showLocalTime t

instance PC.FromField LocalTimeFromMySQLTimeStamp where
  parseField s
    = case fromMySQLDateTimeAsTextToLocalTime (decodeUtf8 s) of
        Nothing
          -> mzero
        Just t
          -> pure $ LocalTimeFromMySQLTimeStamp t

data MaybeId = MaybeId (Maybe Int)

instance Show MaybeId where
  show (MaybeId Nothing) = show "NULL"
  show (MaybeId (Just v)) = show v

instance PC.FromField MaybeId where
  parseField bs
    = let ts = decodeUtf8 bs
      in  if ts == "\\N"
          then pure $ MaybeId Nothing
          else case fromTextToInt ts of
                 Nothing
                   -> fail $ "Unrecognized number \"" ++ (Text.unpack ts) ++ "\""
                 Just i
                   -> pure $ MaybeId $ Just i

-- | A CSV format used from PHP world for sending the changes in rate plans.
data CSVFormat_RateChanges
  = CSVFormat_RateChanges {
       rpc1_id :: !DBRateId
     , rpc1_formatName :: !Text.Text
     , rpc1_fromTime :: !LocalTimeFromMySQLTimeStamp
     , rpc1_internalName :: !Text.Text
     , rpc1_maybeChangedRateId :: !MaybeId
     , rpc1_shortDescription :: !Text.Text
    } deriving (Show)

instance PC.FromRecord CSVFormat_RateChanges where
     parseRecord v
       = case V.length v == 6 of
           True
             -> CSVFormat_RateChanges <$>
                  v .! 0 <*>
                  v .! 1 <*>
                  v .! 2 <*>
                  v .! 3 <*>
                  v .! 4 <*>
                  v .! 5
           False
             -> fail $ "There are " ++ show (V.length v) ++ " fields, instead of expected number of fields."

-- | Load a rate plan environment form the PHP world,
--   parse them, and convert to proper rates in the Haskell World.
--   @require: rate stream is ordered by date of change (first older changes, later more recent changes)
ratingEnv_load
  :: FilePath
     -- ^ rate plan changes
  -> FilePath
     -- ^ rate plan base file name
  -> LocalTime
     -- ^ from call date
  -> Maybe LocalTime
     -- ^ to call date
  -> ConfiguredRatePlanParsers
  -> EnvParams
  -> Maybe Text.Text
     -- ^ load only the rate with the specified name
  -> (PS.SafeT IO) (Either AsterisellError RatingEnv)

ratingEnv_load fileName specificRateFileName fromDate maybeToDate ratePlanParsers params maybeOnlyRateWithName
  = do PS.withFile fileName ReadMode (\handle -> do

         -- this is a pipe that convert a file to a stream of bytes
         let sourceFile = PB.fromHandle handle

         -- this is a pipe that convert the stream of bytes to a stream of records.
         -- Rates are ordered respect starting application date.
         let sourceRecords :: Producer (Either String CSVFormat_RateChanges) (PS.SafeT IO) ()
             sourceRecords = PC.decodeWith csvOptions PC.NoHeader sourceFile

         -- Insert into the change of rates, only rates inside the rating time frame
         maybeRatePlanChanges :: Either AsterisellError (Int, DBRateChanges, Map.Map DBRateId CSVFormat_RateChanges)
           <- PP.fold calcRatePlanChanges (Right (1, rateChanges_empty, Map.empty)) (id) sourceRecords

         case maybeRatePlanChanges of
           Left err
             -> return $ Left err
           Right (_, ratePlanChanges, rateCSVFormat)
             -> do maybeAllRates <- foldM importReferencedRatePlan (Right Map.empty) (Map.assocs rateCSVFormat)
                   case maybeAllRates of
                     Left err
                       -> return $ Left err
                     Right allRates
                       -> return $ Right $ RatingEnv {
                                             env_changes = ratePlanChanges
                                           , env_rates = allRates
                                           , env_params = params
                                           , env_extensionsToExport = trie_empty
                                                     }

         )

 where

   csvOptions = PC.defaultDecodeOptions

   importReferencedRatePlan (Left err) _
     = do return (Left err)

   importReferencedRatePlan (Right cachedRates) (rateId, csv)
     = do  let rateFileName = specificRateFileName ++ show rateId ++ ".rate"
           let formatName = rpc1_formatName csv
           let rateRefName = rpc1_internalName csv
           case configuredRatePlanParsers_get ratePlanParsers (Text.unpack formatName) of
             Nothing
               -> return $ Left $ createError
                                    Type_Critical
                                    Domain_RATES
                                    ("unknown rate format - " ++ (Text.unpack formatName))
                                    ("The rate format \"" ++ (Text.unpack formatName) ++ "\" used for the definition of rate \"" ++ (Text.unpack rateRefName) ++ "\" is unknown, and the rate specification can not be parsed. The call report will not report the CDRs with problems, because this is a critical error preventing the import and rating of all the CDRs in the timeframe.")
                                    ("The CDRs can not be rated.")
                                    ("Use a correct rate format name, or contact the assistance, for adding the support for this new rate format in the application code.")

             Just rateParser
               -> do maybeRatePlan <- rateParser rateFileName params
                     case maybeRatePlan of
                       Left err
                         -> return $ Left $ err {
                                              asterisellError_key = asterisellError_key err ++ " - " ++ show rateId
                                            , asterisellError_type = Type_Critical
                                            , asterisellError_domain = Domain_RATES
                                            , asterisellError_description = "Error during parsing of rate id " ++ show rateId ++ ", with name \"" ++ (Text.unpack rateRefName) ++ "\"." ++ asterisellError_description err
                                            , asterisellError_effect = "CDRs will be not rated. " ++ asterisellError_effect err
                                            , asterisellError_proposedSolution = "Fix the rate specification. " ++ asterisellError_proposedSolution err
                                                }
                       Right ratePlan1
                         -> return $ Right $ Map.insert rateId ratePlan1 cachedRates


   calcRatePlanChanges (Left err) _ = Left err

   calcRatePlanChanges (Right (line, changes1, cache1)) maybeRatePlan
     = case maybeRatePlan of
         Left err -> Left $ createError
                              Type_Critical
                              Domain_RATES
                              ("rate changes error")
                              ("Error parsing the system generated file \"" ++ fileName ++ "\", with changes in rates, at line " ++ show line ++ ". " ++ err)
                              ("CDRs will be not rated. The call report will not report the CDRs with problems, because this is a critical error preventing the import and rating of all the CDRs in the timeframe.")
                              ("This is an error in the application. Contact the assistance.")
         Right ratePlan
           -> let (LocalTimeFromMySQLTimeStamp ratePlanStartFrom) = rpc1_fromTime ratePlan

                  -- test if this rate replace a "new" rate specification with the same name.
                  -- The searched rate is new because they are loaded in reverse order of date,
                  -- so the old start-date is the end-date of this rate.
                  ratePlanMaybeEndTo :: Maybe LocalTime
                  ratePlanMaybeEndTo
                    = case Map.lookup (Text.unpack rateRef) changes1 of
                        Nothing
                          -> Nothing
                        Just []
                          -> Nothing
                        Just ((r, _):_)
                          -> Just r

                  rateId = rpc1_id ratePlan
                  -- this is the PHP world id of the rate, and it is unique

                  rateRef = rpc1_internalName ratePlan
                  -- this is the PHP world internal/symbolic name of the rate, and it is unique,
                  -- or if it change, then replace an old rate, with a new version.

                  changes2 = rateChanges_insert changes1 (Text.unpack rateRef) rateId ratePlanStartFrom

                  cache2 = Map.insert rateId ratePlan cache1

                  excludeRate1
                    = case maybeToDate of
                        Nothing
                          -> False
                        Just toDate
                          -> ratePlanStartFrom > toDate
                             -- exclude rates that are applicable after the ending of the rating event time frame

                  excludeRate2
                    = case ratePlanMaybeEndTo of
                        Nothing
                          -> False
                        Just ratePlanEndTo
                          -> ratePlanEndTo <= fromDate
                             -- exclude rates having a new version before the starting of the rating event time frame

                  excludeRate3
                    = case maybeOnlyRateWithName of
                        Nothing -> False
                        Just onlyRateWithName -> not (onlyRateWithName == rateRef)
                                                 -- exclude rates with a different name

              in if excludeRate1 || excludeRate2 || excludeRate3
                 then Right (line + 1, changes1, cache1)
                 else Right (line + 1, changes2, cache2)

-----------------------------------
-- EXTERNAL RATE PARSING SUPPORT --
-----------------------------------

-- | A name for a rate format, as imported from the external database.
type RateFormatName = String

-- | Parse a rate specification, deriving the matching function.
--   The returned error message will be enriched from the caller,
--   with the rate file name.
--   The EnvParams are passed, because they can be used
--   for validating in a static way the rate file.
type RatePlanParser = FilePath -> EnvParams -> (PS.SafeT IO) (Either AsterisellError MainRatePlan)

type ConfiguredRatePlanParsers = Map.Map RateFormatName RatePlanParser

-- | The logical type associated to main rate plans.
ratePlanSpecificationType :: Text.Text
ratePlanSpecificationType = Text.pack "rate-plan-specification"

-- | The reference name of an external rate.
type ExternalRateReference = String

-- | The result of compilation of external rate plans.
type ExternalRatePlans = Map.Map ExternalRateReference RatePlan

configuredRatePlanParsers_get :: ConfiguredRatePlanParsers -> RateFormatName -> Maybe RatePlanParser
configuredRatePlanParsers_get conf1 l
  = Map.lookup l conf1

----------------
-- RATING ENV --
----------------

-- | Extensions to export to an external database.
type ExtensionsToExport = Trie ()

-- | A running environment to use during rating.
--   In this environment there are:
--   *  only read-only values, that do not change during rating of CDRs;
--   *  values not depending from the current rate used for rating a CDR;
data RatingEnv
  = RatingEnv {
      env_changes :: ! DBRateChanges
    , env_rates :: ! CachedDBRates
    , env_params :: ! EnvParams
    , env_extensionsToExport :: ExtensionsToExport
    } deriving(Show)

env_empty params = RatingEnv {
       env_changes = rateChanges_empty
     , env_rates = cachedRates_empty
     , env_params = params
     , env_extensionsToExport = trie_empty
  }

env_respectCodeContracts :: RatingEnv -> Either String ()
env_respectCodeContracts env
  = do rateChanges_respectCodeContracts (env_changes env)
       params_respectCodeContracts (env_params env)
       return ()

-- | Retrieve a RatePlan, using PHP reference name.
env_getRate
  :: RatingEnv
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
  = let rateChanges = env_changes env
    in case rateChanges_getRateId rateChanges refName cdrTime of
         Nothing
           -> Nothing
         Just (i, t)
           -> Just (fromJust1 "rp30" $ Map.lookup i (env_rates env), t)

------------------------------
-- GENERIC PARSING OF RATES --
------------------------------

parseRatePlan :: (EnvParams -> Parsec.Parser MainRatePlan) -> FilePath -> EnvParams -> (PS.SafeT IO) (Either AsterisellError MainRatePlan)
parseRatePlan parserToCall fileName env
  = do !maybeResult <- liftIO $! Parsec.parseFromFile (parserToCall env) fileName
       case maybeResult of
            Left err
              -> return $ Left $ createError
                                   Type_Critical
                                   Domain_RATES
                                   ("error parsing - " ++ fileName)
                                   ("The rate file \"" ++ fileName ++ "\" has a bad format: " ++ show err)
                                   ("The CDRs in the time frame of this rate will be not rated. The call report will not report the CDRs with problems, because this is a critical error preventing the import and rating of all the CDRs in the timeframe.")
                                   ("Correct the format of the rate file, or contact the assistance if there are no problems in the format of the file.")

            Right result
              -> return $ Right result


parseRatePlanUsingParsecParser :: (EnvParams -> Parsec.Parser MainRatePlan) -> RatePlanParser
parseRatePlanUsingParsecParser parserToCall fileName env
  = do !r <- parseRatePlan parserToCall fileName env
       case r of
         Left err
           -> return $! Left err
         Right ratePlan
           -> return $! Right ratePlan

-- | Parse a file using Attoparsec, and chunk by chunk,
--   so it can be used for big files, without using too much RAM.
parseRatePlanUsingAttoParsec :: (EnvParams -> LazyAttoParsec.Parser MainRatePlan) -> RatePlanParser
parseRatePlanUsingAttoParsec  parserToCall fileName envParams
  = do !r <- openFileAndParse (parseRate True)
       case r of
         Left msg
           -> do err <- countLineWithError
                 let lineWithError
                       = case err of
                           Left _ -> "unspecified"
                           Right l -> show l
                 return $ Left $ createError
                                        Type_Critical
                                        Domain_RATES
                                        ("error parsing rate file - " ++ fileName)
                                        ("The rate file \"" ++ fileName ++ "\" has a bad format at line " ++ lineWithError ++ ". " ++ msg)
                                        ("The CDRs in the time frame of this rate will be not rated. The call report will not report the CDRs with problems, because this is a critical error preventing the import and rating of all the CDRs in the timeframe.")
                                        ("Correct the format of the rate file, or contact the assistance.")


         Right rs
           -> return $ Right rs

 where

  myEitherResult :: LazyAttoParsec.Result r -> Either String r
  myEitherResult (LazyAttoParsec.Done t r) = if LazyText.null t then Right r else Left "Parse error."
  myEitherResult err = LazyAttoParsec.eitherResult err

  openFileAndParse :: LazyAttoParsec.Parser a -> (PS.SafeT IO) (Either String a)
  openFileAndParse p =
    PS.bracket
      (do handle <- IO.openFile fileName ReadMode
          IO.hSetEncoding handle IO.utf8_bom
          return handle
      )

      (hClose)

      (\handle -> do
          fileContent <- liftIO $ LazyIO.hGetContents handle
          return $! myEitherResult $! LazyAttoParsec.parse p fileContent
          -- NOTE: force a strict evaluation, otherwise the file is closed
          -- before it is completeley parsed.
      )

  parseRate True  = (parserToCall envParams) <* LazyAttoParsec.endOfInput
  parseRate False = (parserToCall envParams)

  countLineWithError :: (PS.SafeT IO) (Either String Int)
  countLineWithError = do
    totLines <- openFileAndParse (countLines 1)
    remainingLines <- openFileAndParse (parseRate False *> countLines 1)
    case (totLines, remainingLines) of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right totLines', Right remainingLines') -> return $ Right $ totLines' - remainingLines' + 1

countLines :: Int -> LazyAttoParsec.Parser Int
countLines n =
  LazyAttoParsec.option n (snl *> LazyAttoParsec.endOfLine *> countLines (n + 1))
 where
   snl = LazyAttoParsec.skipWhile (\c -> not $ (LazyAttoParsec.isEndOfLine c))

-- TODO old code to delete

-- | Parse a file using Attoparsec, and chunk by chunk,
--   so it can be used for big files, without using too much RAM.
parseRatePlanUsingAttoParsecOld :: (EnvParams -> LazyAttoParsec.Parser MainRatePlan) -> RatePlanParser
parseRatePlanUsingAttoParsecOld  parserToCall fileName envParams
  = do r <- openFileAndParse (parseRate True)
       case r of
         Left msg
           -> do err <- countLineWithError
                 let lineWithError
                       = case err of
                           Left _ -> "unknown"
                           Right l -> show l
                 return $ Left $ createError
                                        Type_Critical
                                        Domain_RATES
                                        ("error parsing rate file - " ++ fileName)
                                        ("The rate file \"" ++ fileName ++ "\" has a bad format at line " ++ lineWithError ++ ". " ++ msg)
                                        ("The CDRs in the time frame of this rate will be not rated. The call report will not report the CDRs with problems, because this is a critical error preventing the import and rating of all the CDRs in the timeframe.")
                                        ("Correct the format of the rate file, or contact the assistance.")

         Right rs
           -> return $ Right rs
 
 where

  myEitherResult :: LazyAttoParsec.Result r -> Either String r
  myEitherResult (LazyAttoParsec.Done t r) = if LazyText.null t then Right r else Left "Parse error."
  myEitherResult err = LazyAttoParsec.eitherResult err
   
  openFileAndParse :: LazyAttoParsec.Parser a -> (PS.SafeT IO) (Either String a)
  openFileAndParse p = do
    PS.bracket
      (do handle <- IO.openFile fileName ReadMode
          IO.hSetEncoding handle IO.utf8_bom
          return handle
      )

      (hClose)

      (\handle
         -> do fileContent <- liftIO $ LazyIO.hGetContents handle
               return $ myEitherResult $ LazyAttoParsec.parse p fileContent
      )

  countLines :: Int -> LazyAttoParsec.Parser Int 
  countLines n
     = do LazyAttoParsec.skipWhile (\c -> not $ LazyAttoParsec.isEndOfLine c)
          (Control.Applicative.<|>)
            (do LazyAttoParsec.endOfInput
                return n)
            (do LazyAttoParsec.endOfLine
                countLines (n + 1))

  parseRate :: Bool -> LazyAttoParsec.Parser MainRatePlan
  parseRate complete = do
    r <- (parserToCall envParams)
    isEOF <- LazyAttoParsec.atEnd
    case (complete, isEOF) of
      (True, False) -> fail "Parsing error."
      _ -> do return r
      
  countLineWithError :: (PS.SafeT IO) (Either String Int)
  countLineWithError = do 
    totLines <- openFileAndParse (countLines 1)
    remainingLines <- openFileAndParse
                            (do parseRate False
                                countLines 1)
    case (totLines, remainingLines) of
      (Left err, _) -> return $ Left err
      (_, Left err) -> return $ Left err
      (Right totLines', Right remainingLines') -> return $ Right $ totLines' - remainingLines'

-- | Test the parser.
testParserUsingAttoParsec :: (Show a) => LazyAttoParsec.Parser a -> String -> IO ()
testParserUsingAttoParsec p fileName
  = do handle <- IO.openFile fileName ReadMode
       IO.hSetEncoding handle IO.utf8_bom
       fileContent <- liftIO $ LazyIO.hGetContents handle
       let !r = LazyAttoParsec.parse (p <* LazyAttoParsec.endOfInput) fileContent
       case LazyAttoParsec.eitherResult r of
         Left msg
           -> putStrLn $ "Error: \n" ++ msg
         Right rs
           -> putStrLn $ "Result: \n" ++ show r

-- | Show a CDR content, with also additional initializated content.
--   This make sense only if the CDR has passed the first initialization pass, and there are errors in the next rating part.
rate_showDebug :: RatingEnv -> CDR -> String
rate_showDebug env1 cdr
  =    (cdr_showDebug cdr)
    ++ (addLine "organization-ids" organizationIds)
    ++ (addLine "price-category" priceCategoryName)
    ++ (addLine "communication-channel-type" channelTypeName)
    ++ (addLine "vendor" vendorName)

 where

    addLine l v
      = "\n   " ++ l ++ ": " ++ v

    env = env_params env1

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
             -> fromJust1 "rpa1" $ Map.lookup id rateCategories

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
            -> fromJust1 "rpa2" $ Map.lookup id channelTypes

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
                   -> n

----------------
-- UNIT TESTS --
----------------

tt_ratePlanTests
  = [ HUnit.TestCase $ HUnit.assertEqual "time frame " secondsInADay (timeFrame_duration (date1, date2))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (secondsInADay + 60 * 60) (timeFrame_duration (date1, date3))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-08-06 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-08-06 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-08-30 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-09-04 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-05 00:00:00", d "2014-09-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-09-04 23:59:59"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-09-05 00:00:00", d "2014-10-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-09-05 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-12-05 00:00:00", d "2015-01-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-12-05 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-12-05 00:00:00", d "2015-01-05 00:00:00") (timeFrame_fromCallDate (tf (MonthlyTimeFrame 5)) (d "2014-12-06 00:00:00"))

    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-11 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-12 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-13 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-11 00:00:00", d "2014-08-18 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-17 23:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-18 00:00:00", d "2014-08-25 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-18 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-08-18 00:00:00", d "2014-08-25 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-19 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-07-28 00:00:00", d "2014-08-04 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-07-28 00:00:00"))
    , HUnit.TestCase $ HUnit.assertEqual "time frame " (d "2014-07-28 00:00:00", d "2014-08-04 00:00:00") (timeFrame_fromCallDate (tf (WeeklyTimeFrame 1)) (d "2014-08-01 00:00:00"))
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
                  , bundle_minCost = 0
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
