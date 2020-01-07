{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- -------------------------------------------------------------------
-- Calculate cost/income of a call using the algo
-- described in the Asterisell manual, and in `RateEngine` module.
--
-- Read RateEngine module contracts.
--
module Asterisell.ApplyRatePlan (
  mainRatePlan_calcCost
  , ratingParamsForTest
  , bundleState_areThereServiceCdrs
  , RateCost(..)
  , SelectedRate
  , AppliedRate
  , AppliedNormalAndBundleRate
  , AppliedCostAndIncomeRate
  , ApplyRatesProcessState
  , applied_noRate
  , process_calcRates
  , process_applyRates
  , ratingParams_empty
  , calcParams_calc
  , bundleState_updateAndGetServiceCdrs
  , tt_ratePlanTests
  , service_exportServiceCDRSTelephonePrefixes
  , service_defaultExternalTelephoneNumber
  , service_generate
  , serviceParams_load
  , ratePlan_loadRatingParams
  , tt_servicesTests
  , params_peakCodes
  , serviceCDRS_rate
  , serviceCDRS_rateIO
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
import Asterisell.RatePlan

import Data.List as L
import Control.Monad.State.Strict as State
import Control.Monad as M
import Control.Monad.IO.Class       (liftIO)
import Data.Time.LocalTime
import Data.Time.Clock
import Control.Monad.Except
import Data.IORef
import qualified Data.Text as Text
import qualified Test.HUnit as HUnit
import qualified Data.ByteString.Base64 as B64
import qualified Codec.Compression.QuickLZ as LZ
import Control.DeepSeq as DeepSeq
import Data.Maybe
import Data.Set as Set
import Data.IntSet as ISet
import Data.HashSet as HSet
import Data.IntMap.Strict as IMap
import qualified Data.Serialize as DBS
import Data.Either
import qualified System.IO.Streams as S
import qualified Data.Vector as V
import Database.MySQL.Base as DB
import Text.Heredoc
import Control.Exception.Safe
import qualified Data.HashTable.IO as IMH
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Debug.Trace

-- -----------------------------------------------
-- Main Rating Processes

-- | The best selected rate, with overridden calc params, according the parent hierarchy.
type SelectedRate
       = ( RateSystemId  -- ^ in case of ExternalRate, this is the most specific rate in MainRatePlan
         , RatePlan      -- ^ the most specific RatePlan
         , CalcParams    -- ^ the CalcParams to apply
         , MatchStrenght
         )

type AppliedRate
       = ( MonetaryValue -- ^ the cost/income of the CDR
         , UnitId        -- ^ in case of BundleRate application, the UnitId responsible for the bundle
         , RateSystemId  -- ^ the used rate
         )

type AppliedNormalAndBundleRate = (Maybe AppliedRate, Maybe AppliedRate)

type AppliedCostAndIncomeRate = (AppliedNormalAndBundleRate, AppliedNormalAndBundleRate)

applied_noRate :: AppliedNormalAndBundleRate
applied_noRate = (Nothing, Nothing)
{-# INLINE applied_noRate #-}

-- | Analyze CDR and calculate normal and bundle rates, without applying them to the CDR.
--   Raise exceptions only in case of critical errors compromising the entire rating process.
--   Normal rating errors are signaled generating a CDR with an error.
process_calcRates
  :: RatingParams
  -> ParsedCDR1
  -> IO (ParsedCDR1, AppliedCostAndIncomeRate, [ExpandedExtension])
  -- @ensure there is AppliedCostAndIncome if and only if there is no AsterisellError
  -- @ensure there can be not correct cdr_toError info, because this is applied from the process_errors

process_calcRates env (ParsedCDR1(sourceCDR, Just err, cdr1)) = do
  return $ (ParsedCDR1(sourceCDR, Just err, cdr1), (applied_noRate, applied_noRate), [])

process_calcRates env (ParsedCDR1(sourceCDR@(providerName, callDate, formatId, rawCDR), Nothing, cdr1)) = do

  let !fromCallDate = params_fromDate env
  let !cdrImporters = params_fastLookupCDRImporters env
  let !isOfficialRatingEvent = params_isRateUnbilledCallsEvent env
  let !precisionDigits = params_currencyPrecision env

  let (!cdr2, !isNormalCDR)
          = if cdr_isServiceCDR cdr1
            then (cdr1 { cdr_direction = CDR_system}, False)
                 -- NOTE: by definition a service CDR imported from a ar_source_cdr is a imported service CDR,
                 -- and it has a direction CDR_system by default.
             else (cdr1, True)

  case cdr_initialClassification env (Just callDate) cdr2 of
    Left err -> return (ParsedCDR1(sourceCDR, Just err, cdr2), (applied_noRate, applied_noRate), [])
    Right (cdr3, expandedExtensions) -> do
      case cdr_direction cdr3 == CDR_ignored of
        True -> return (ParsedCDR1(sourceCDR, Nothing, cdr3), (applied_noRate, applied_noRate), expandedExtensions)
        False -> do
          case mainRatePlan_calcCost
                 IncomeRate
                 env
                 (params_incomeRate env)
                 (testCdr True cdr3) of
            Left err -> return (ParsedCDR1(sourceCDR, Just err, cdr3), (applied_noRate, applied_noRate), expandedExtensions)
            Right appliedIncome ->
              case mainRatePlan_calcCost CostRate env (params_costRate env) cdr3 of
                Left err -> return (ParsedCDR1(sourceCDR, Just err, cdr3), (applied_noRate, applied_noRate), expandedExtensions)
                Right appliedCost -> return (ParsedCDR1(sourceCDR, Nothing, cdr3), (appliedCost, appliedIncome), expandedExtensions)
 where

  testCdr :: Bool -> CDR -> CDR
  testCdr isRated cdr = pAssert "ERR 008" (cdr_directionIsWellFormed isRated cdr) cdr
  {-# INLINE testCdr #-}

type ApplyRatesProcessState
       = (BundleState
         -- ^ the initial BundleState
         , Maybe BundleState
         -- ^ the BundleState to save at the end of the rating process
         -- NOTE: it is saved a BundleState not immediately at the end of rating process,
         -- but few minutes before, so it can be reused at next rating process
         , Maybe BundleState
         -- ^ the BundleState to save at the beginning of the last day of rating process.
         --   It is like previous BundleState, but instead of 30m before the ending, it is at begin of the day.
         --   Having this BundleState make more efficient incremental rerating of resellers (they receive daily status update from providers)
         , CallDate
           -- ^ the max found calldate
         , ConflictingUnitIds
         )

-- | Apply calculated rates, accordingly BundleState, and keep updated it.
--   Generate also BundleRate service CDRS.
--   Raise exceptions only in case of critical errors compromising the entire rating process.
--   Normal rating errors are signaled generating a CDR with an error.
--
--   DEV-NOTE: this job had to be rather fast because it will receive all CDRS on a single thread.
--   It can not be parallel because the BundleState depends from the order of CDRS.
--
--   @require it will receive also as a CDR-to-ignore, the last calldate to process, so all bundle-rate services are generated.
--   Otherwise if some customer has no calls at the end of the time-frame, some service CDRS can be not generated.
--   @ensure for each inpuct Chunk (except Nothing), only one out/result Chunk is written, in accordance with OrderedStream API.
--   @ensure for Nothing input Chunk only two out/result Chunks are generated: final data, and Nothing
process_applyRates
  :: RatingParams
  -> ApplyRatesProcessState
  -> OrderedStream (ParsedCDR1, AppliedCostAndIncomeRate, [ExpandedExtension])
  -- ^ input
  -> OrderedStream ParsedCDR1
  -- ^ output
  -- @ensure CDRS and ServiceCDRS are generated ordered by date
  -- @ensure propagate EOF
  -> OrderedStream ExpandedExtension
  -- ^ output
  -- @ensure propagate EOF
  -> IO (Async ApplyRatesProcessState)

process_applyRates env state0 inChan outChan outExtChan = async $ mainProcess True state0

 where

  !isDebugMode = params_isDebugMode env
  !imr = params_incomeRate env
  !cmrp =  mainRatePlan_systemIdToUserIdPath $ params_costRate env
  !imrp =  mainRatePlan_systemIdToUserIdPath $ params_incomeRate env

  !saveBundleStateImmediatelyAfter = params_saveBundleStateImmediatelyAfter env
  !saveBundleStateAtBeginOfThisDay = toBeginOfTheDay saveBundleStateImmediatelyAfter

  dbg :: CDR -> RateRole -> RateSystemId -> Maybe Text.Text
  dbg cdr rateRole rateId = DeepSeq.force $ if isDebugMode then (Just $ Text.pack $ rateName cdr rateRole rateId) else Nothing
  {-# INLINE dbg #-}

  rateName :: CDR -> RateRole -> RateSystemId -> String
  rateName cdr r i
   = Text.unpack $ fromJust1 ("err 117 for " ++ show r ++ ", id: " ++ show i) $ IMap.lookup i (case r of CostRate -> cmrp; IncomeRate -> imrp)
  {-# INLINE rateName #-}

  sendExtensions :: [ExpandedExtension] -> IO ()
  sendExtensions [] = return ()
  sendExtensions es = orderedStream_put outExtChan $ Just $ V.fromList es

  mainProcess isFirst state1 = do
    maybeCdrs1 <- takeMVar inChan
    case maybeCdrs1 of
      Nothing -> do
        orderedStream_put outChan Nothing
        orderedStream_put outExtChan Nothing
        return state1

      Just cdrs1 -> do
        ((state2, serviceCDRS2), cdrs2) <-
            mapAccumLM rateCDRSAndServices (state1, if isFirst then (V.toList $ params_initialBundleRateServices env) else []) (V.toList cdrs1)

        case L.null serviceCDRS2 of
          True -> orderedStream_put outChan (Just (V.fromList cdrs2))
          False -> do
            -- In this case we order cdrs2 and serviceCDRS2.
            -- NOTE: this is a rare event because it happens only at the end of a BundleTimeFrame.
            let cdrs3
                  = L.sortBy
                      (\(ParsedCDR1 (_, _, cdr1)) (ParsedCDR1 (_, _, cdr2)) -> compare (cdr_calldate cdr1) (cdr_calldate cdr2))
                      (cdrs2 ++ (L.map serviceCDR_toParsedCDR1 serviceCDRS2))
            orderedStream_put outChan (Just (V.fromList cdrs3))
        mainProcess False state2

  -- | Update BundleState, generate ServiceCDRS and rate CDR.
  --   @ensure CDRS are generated ordered by calldate.
  rateCDRSAndServices
    :: (ApplyRatesProcessState, [ServiceCDR])
    -> (ParsedCDR1, AppliedCostAndIncomeRate, [ExpandedExtension])
    -> IO ((ApplyRatesProcessState, [ServiceCDR]), ParsedCDR1)

  rateCDRSAndServices (state1, services1) (parsedCDR1@(ParsedCDR1(sourceCDR, Just err, cdr1)), ((Nothing, Nothing), (Nothing, Nothing)), extensions) = do
    sendExtensions extensions
    (state2, services2) <- updateBundleState state1 (cdr_calldate cdr1)
    return ((state2, services1 ++ services2), ParsedCDR1(sourceCDR, Just err, cdr1))

  rateCDRSAndServices (state1, services1) (parsedCDR1@(ParsedCDR1(_, Nothing, cdr1)), ((Nothing, Nothing), (Nothing, Nothing)), extensions) = do
    sendExtensions extensions
    (state2, services2) <- updateBundleState state1 (cdr_calldate $ pAssert "ERR 011" (cdr_direction cdr1 == CDR_ignored) cdr1)
    return ((state2, services1 ++ services2), parsedCDR1)

  rateCDRSAndServices (state1, services1) (ParsedCDR1(_, _, cdr1), ((Just normalCostRate, Just bundleCostRate), _), _) = do
    return $ pError "err 61775: a CostRate can not have a BundleRate. Unexpected condition in the code."

  rateCDRSAndServices
    (state1, services1)
    (ParsedCDR1(sourceCDR@(providerName, callDate, formatId, rawCDR), Nothing, cdr1)
    , ( (Just normalCostRate@(cost, _, costRateId), Nothing)
      , (Just normalIncomeRate@(normalIncome, _, normalIncomeRateId), maybeBundleIncomeRate))
    , extensions) = do

      sendExtensions extensions
      (state2, services2) <- updateBundleState state1 (cdr_calldate cdr1)

      let (!cdr2, !isNormalCDR)
              = if cdr_isServiceCDR cdr1
                then (cdr1 { cdr_direction = CDR_system}, False)
                     -- NOTE: by definition a service CDR imported from a ar_source_cdr is a imported service CDR,
                     -- and it has a direction CDR_system by default.
                 else (cdr1, True)

      case cdr_direction cdr2 == CDR_ignored of
        True -> return ((state2, services2), ParsedCDR1(sourceCDR, Nothing, cdr2))
        False -> do
          let !cdr3 = cdr2 { cdr_cost = cost, cdr_debug_cost_rate = dbg cdr2 CostRate costRateId }

          (state3, income, maybeBundleUnitId, incomeRateId) <-
            case maybeBundleIncomeRate of
              Nothing -> return (state2, normalIncome, Nothing, normalIncomeRateId)
              Just (bundleIncome, bundleUnitId, bundleRateId) -> do
                let ((d1, bundleRecord1), ms1, ms2, mcd, mus2) = state2
                case (bundleRecord_applyIfPossible imr bundleRecord1 bundleUnitId cdr3 bundleRateId) of
                  Nothing ->
                    return (state2, normalIncome, Nothing, normalIncomeRateId)
                  Just bundleRecord2 ->
                    return (((d1, bundleRecord2), ms1, ms2, mcd, mus2), bundleIncome, Just bundleUnitId, bundleRateId)

          let !cdr4 = cdr3 { cdr_income = income, cdr_bundleOrganizationUnitId = maybeBundleUnitId, cdr_debug_income_rate = dbg cdr3 IncomeRate incomeRateId }
          return ((state3, services1 ++ services2), ParsedCDR1(sourceCDR, Nothing, cdr4))

  -- | Update the rating status, and rate the corresponding bundle rates service CDRS.
  --   ThrowIO a critical error if the service CDR can not be initializated correctly.
  --   @ensure ServiceCDRS are generated ordered by CallDate
  updateBundleState :: ApplyRatesProcessState -> CallDate -> IO (ApplyRatesProcessState, [ServiceCDR])
  updateBundleState state1@(bundleState1, mx, my, _, conflicts1) callDate = do
    (!bundleState2, !serviceCDRS2, !conflicts2) <-
      case bundleState_maybeUpdateAndGetServiceCDRS env imr bundleState1 callDate of
        Nothing -> return (bundleState1, [], conflicts1)
        Just (bundleState', serviceCDRS', conflicts') -> do
          return (bundleState', serviceCDRS', ISet.union conflicts1 conflicts')

    return $ DeepSeq.force
               ((bundleState2
                , case mx of
                    Just _ -> mx
                    Nothing -> if callDate >= saveBundleStateImmediatelyAfter then (Just bundleState2) else Nothing
                , case my of
                    Just _ -> my
                    Nothing -> if callDate >= saveBundleStateAtBeginOfThisDay then (Just bundleState2) else Nothing
                , callDate
                , conflicts2)
               , serviceCDRS2)
   -- TODO try to remove DeepSeq and add `!` and profile again

-- --------------------------------
-- CDR Initial classification

-- | Like serviceCDRS_rate but use `throwIO`.
serviceCDRS_rateIO
  :: RatingParams
  -> [ServiceCDR]
  -> IO [ServiceCDR]

serviceCDRS_rateIO env cdrs = do
  case serviceCDRS_rate env cdrs of
    Left err -> throwIO $ asterisellError_toException err
    Right r -> return r
{-# INLINE serviceCDRS_rateIO #-}

-- | Send the specified ServiceCDR but before apply a rating normalization process.
--   The services CDRS are proportional to the number of customers, so they can be stored and processed in RAM.
--   Usually if the code is correct, they do not generate errors,
--   so instead of generating an AsterisellError for every possible ServiceCDR
--   generate only a global error.
serviceCDRS_rate
  :: RatingParams
  -> [ServiceCDR]
  -> Either AsterisellError [ServiceCDR]

serviceCDRS_rate env services1
  = M.mapM (\ cdr -> fst <$> cdr_initialClassification env Nothing cdr) services1
{-# INLINE serviceCDRS_rate #-}

-- | Apply initial classification to the CDR, completing the fields that are not completed from
--   the initial custom CDR importer, or complete fields of ServiceCDR.
cdr_initialClassification :: RatingParams -> Maybe CallDate -> CDR -> Either AsterisellError (CDR, [ExpandedExtension])
cdr_initialClassification env maybeCallDateToVerify cdr1 = do

  let !callDateToVerify = case maybeCallDateToVerify of
                            Nothing -> cdr_calldate cdr1
                            Just c -> c

  case (cdr_direction cdr1 == CDR_error || cdr_direction cdr1 == CDR_ignored) of
    True -> return (cdr1, [])
    False -> do
      (cdr2, isExtensionalMatch) <- pass1 cdr1
      case cdr_direction cdr2 == CDR_ignored of
        True -> return (cdr2, [])
        False -> do
          !cdr4 <- execStateT pass2 cdr2
          when (not (cdr_calldate cdr4 == callDateToVerify))
               (throwError $
                  createError
                    Type_Error
                    Domain_RATES
                    ("changed rating classification code")
                    ("This CDR was first imported, using the call date " ++ fromLocalTimeToMySQLDateTime callDateToVerify ++ ", but now during rating phase, the parsed calldate is " ++ fromLocalTimeToMySQLDateTime (cdr_calldate cdr1) ++ ". Probably the code interpreting the CDR format was changed in the past, and now during rerating there is some mismatch.")
                    ("All CDRs of this type will be not rated, because the database content is potentially corrupted.")
                    ("Extract calls at the specified calldate (the first one of the list), and of the same provider/type/version, and resubmit them. They will be deleted from the database, and resubmitted with the correct call date (the second one of the list).")
               )

          let ee = case isExtensionalMatch of
                     True -> [ExpandedExtension {
                                 ee_organizationId = fromJust1 "p557" $ cdr_organizationUnitId cdr4
                                 , ee_specificExtensionCode = fromTextToMySQLResult $ cdr_internalTelephoneNumber cdr4
                                 }
                             ]
                     False -> []

          return (cdr4, ee)

 where

  !info = params_organizations env

  pass1 :: CDR -> Either AsterisellError (CDR, IsExtensionalMatch)
  pass1 cdr1 = do
    let !channelDomains = params_channelDomains env
    let !organizationsInfo = params_organizations env

    let !cdrTime = cdr_calldate cdr1

    let !cdr2 = cdr1 { cdr_precision = (params_currencyPrecision env)
                     , cdr_billsec = ov cdr1 cdr_billsec (cdr_duration cdr1)
                     , cdr_duration = ov cdr1 cdr_duration (cdr_billsec cdr1)
                     }

    when (isNothing $ cdr_billsec cdr2)
         (throwError $ createRateError cdr2
                                       ("CDR not consistent - " ++ cdr_uniqueErrorKey cdr2)
                                       ("This CDR has no duration or billsec field specified.")
                                       ("The CDR will not be rated.")
                                       ("If there are many CDRs with this problem, contact the assistance.")
         )

    -- Use a standard method for retrieving the default Vendor

    (!maybeVendorId, !maybeChannelTypeId)
          <- case (cdr_channel cdr2) of
              Nothing
                -> return (Nothing, Nothing)
              Just channelName
                -> case channelDomains_match channelDomains (fromTextToByteString channelName) cdrTime of
                     [(r1, r2)] -> return (Just r1, Just r2)
                     [] -> throwError $ createRateError cdr2 ("unknown channel name - " ++ Text.unpack channelName)
                                                        ("Channel \"" ++ Text.unpack channelName ++ "\" is not defined in the Channel Domain table.")
                                                        ("All CDRs using the same channel domain will be not rated.")
                                                        ("Configure better the Channel Domain Table. Check also the validity time frame for the domains.")

                     _ ->  throwError $ createRateError cdr2 ("too much channels - " ++ Text.unpack channelName)
                                                        ("There is more than one valid entry in the Channel Domain Table, for the Channel Domain \"" ++ Text.unpack channelName ++ "\".")
                                                        ("All CDRs using the same channel domain will be not rated.")
                                                        ("Configure better the Channel Domain Table. Check also the validity time frame for the domains.")


    let !cdr3 = cdr2 { cdr_vendorId = ov cdr2 cdr_vendorId maybeVendorId
                     , cdr_communicationChannelTypeId = ov cdr2 cdr_communicationChannelTypeId maybeChannelTypeId
                     }

    when   (isNothing $ cdr_vendorId cdr3)
           (throwError $ createRateError cdr3 ("CDRs without vendors ")
                                              ("This CDR has no vendor.")
                                              ("CDRs of the same type will be not rated.")
                                              ("This is probably an error in the code. Contact the assistance.")
           )

    when (isNothing $ cdr_communicationChannelTypeId cdr3)
         (throwError $ createRateError cdr3  ("CDRs without channel type")
                                             ("This CDR has no communication channel type.")
                                             ("CDRs of the same type will be not rated.")
                                             ("This is probably an error in the code. Contact the assistance.")
         )

    -- Complete info about the internal extensions/organization associated to the call.

    (!unitId, !unitInfo, !isExtensionalMatch)
          <- case cdr_organizationUnitId cdr3 of
               Just i
                 -> return $ (i, fromJust1 ("error 1: there is no info for unitId " ++ show i ++ ", at date " ++ showLocalTime cdrTime) $ info_getDataInfoForUnitId organizationsInfo i cdrTime False, True)

               Nothing
                 -> do let accountCode = cdr_internalTelephoneNumber cdr3
                           accountCodeS = Text.unpack $ accountCode

                       when (Text.length accountCode == 0)
                            (throwError $ createRateError cdr3 ("unknown ArAsteriskAccount NULL")
                                                               ("Asterisk VoIP code can not be empty.")
                                                               ("All these CDRs with empty VoIP code will not be rated.")
                                                               ("This is an error in the CDR format. Contact the assistance.")
                            )

                       (unitInfo, isExtensionalMatch)
                         <- case info_getDataInfoForExtensionCode organizationsInfo (fromTextToByteString accountCode) cdrTime of
                              Right Nothing
                                -> throwError $ createRateError
                                                  cdr3
                                                  ("unknown accountcode - " ++ accountCodeS)
                                                  ("\"" ++ accountCodeS ++ "\" VoIP account code is not associated to an extension at call-date " ++ showLocalTime cdrTime)
                                                  ("All CDRs with this VoIP code will not rated.")
                                                  ("Complete the VoIP account codes.")
                              Left err
                                -> throwError err
                              Right (Just r)
                                -> return r

                       return $ (unit_id unitInfo, unitInfo, isExtensionalMatch)

    let dataInfoParents = info_getDataInfoParentHierarchy organizationsInfo unitInfo cdrTime False
    let parentIds = info_getParentIdHierarchy dataInfoParents
    let parentIdsText = info_getParentHiearchyIds dataInfoParents
    let maybeBillableDataInfo = info_getBillableDataInfo dataInfoParents
    let organizationName = info_getFullName dataInfoParents 0 False False False

    when (isNothing maybeBillableDataInfo)
         (throwError $ createRateError cdr3 ("unknown billable organization for " ++ Text.unpack parentIdsText)
                                            ("Organization unit with id " ++ show unitId ++ ", and name " ++ organizationName ++ " has not a billable parent.")
                                            ("These CDRs can not be assigned to a billable organization, and they can not be billed correctly.")
                                            ("Define a billable organization in the organization hierarchy.")
         )

    case info_isOrganizationToIgnore info parentIds of
      True -> return $ (cdr3 { cdr_direction = CDR_ignored}, True)
      False -> do
        let !maybeRateCategoryId = info_getRateCategoryId dataInfoParents

        when (isNothing maybeRateCategoryId)
             (throwError $ createRateError cdr3 ("unknown rate category for " ++ Text.unpack parentIdsText)
                                                ("Organization unit with id " ++ show unitId ++ ", and name " ++ organizationName ++ " has not a price category.")
                                                ("These CDRs can not be billed.")
                                                ("Define a rate/price category in some part of the organization hierarchy.")
             )

        let !billableDataInfo = fromJust1 "2" maybeBillableDataInfo

        let !cdr4 = cdr3 { cdr_organizationUnitId = ov cdr3 cdr_organizationUnitId (Just unitId)
                         , cdr_cachedParentIdHierarchy = ov cdr3 cdr_cachedParentIdHierarchy (Just parentIds)
                         , cdr_billableOrganizationUnitId = ov cdr3 cdr_billableOrganizationUnitId (Just $ unit_id billableDataInfo)
                         , cdr_priceCategoryId = ov cdr3 cdr_priceCategoryId maybeRateCategoryId
                         }
        --
        -- Complete info about telephone numbers
        --

        let !externalTelephoneNumber = cdr_externalTelephoneNumber cdr3

        when (Text.length externalTelephoneNumber == 0)
             (throwError $ createRateError cdr3 ("CDR without external telephone number" ++ cdr_uniqueErrorKey cdr4 )
                                                ("This CDR has no external telephone number.")
                                                ("This CDR has no external telephone number.")
                                                ("The problem can be in the CDR record or in the application configuration. Contact the assistance.")
             )

        return (cdr4, isExtensionalMatch)

  pass2 :: StateT CDR (Either AsterisellError) ()
  pass2
    = do cdr1 <- get
         let telephonePrefixes = params_telephonePrefixes env
         let numberOfDigitsToMask = params_digitsToMask env

         -- Manage telephone prefix only in case there is an already specified ported telephone number:
         -- * in this way if there is an error in the logic, it will be displayed with full details
         -- * other matches will be done in the rating phase, after the ported number is matched with this CDR.

         when ((isNothing $ cdr_telephonePrefixId cdr1) && (isJust $ cdr_externalTelephoneNumberWithAppliedPortability cdr1))
              (do -- Associate telephone operator prefix, using the ported telephone number,
                  -- because it identify the real type of telephone number.
                  let ported1 = fromJust1 "d1" $ cdr_externalTelephoneNumberWithAppliedPortability cdr1
                  let ported = Text.unpack ported1
                  case trie_match telephonePrefixes (fromTextToByteString ported1) of
                    Just (_, _, tp)
                      -> do modify (\c -> c { cdr_telephonePrefixId = Just $ tp_id tp
                                            , cdr_ratingCode = tp_ratingCode tp })
                            return ()
                    Nothing
                      -> let key = L.take (min 4 (L.length ported)) ported
                             -- prepare a missing prefix key:
                             -- it is a balance between displaying many errors for missing number prefix,
                             -- and avoid displaying all missing telephone numbers.

                         in throwError $ createRateError cdr1 ("no telephone operator prefix - " ++ key)
                                                              ("There is no a telephone operator prefix entry associated to the destination number \"" ++ ported ++ "\"")
                                                              ("CDRs with destination number of the same type will not be rated.")
                                                              ("Complete the Telephone Prefixes Table.")
                   )

         -- Displayed telephone numbers
         cdr1 <- get
         when (isNothing $ cdr_displayedExternalTelephoneNumber cdr1)
              (do let external = cdr_externalTelephoneNumber cdr1
                  modify (\c -> c {  cdr_displayedExternalTelephoneNumber = Just external })
                  return ()
              )

         cdr1 <- get
         when (isNothing $ cdr_displayedMaskedExternalTelephoneNumber cdr1) $ do
           let !external1 = fromJust1 "4" $ cdr_displayedExternalTelephoneNumber cdr1
           let !result =
                 case cdr_direction cdr1 of
                   CDR_internal -> external1
                   _ ->
                     -- remove the default prefix, if it is part of the telephone number
                     let !external2 =
                           case params_defaultTelephonePrefixToNotDisplay env of
                             Nothing -> external1
                             Just defaultPrefix ->
                               case Text.stripPrefix defaultPrefix external1 of
                                 Nothing -> external1
                                 Just r -> r

                     in case numberOfDigitsToMask > 0 of
                          False -> external2
                          True ->
                            case (Text.length external2) <= numberOfDigitsToMask of
                              True -> external2 -- do not mask very short telephone numbers
                              False -> Text.append
                                         (Text.take (Text.length external2 - numberOfDigitsToMask) external2)
                                         maskText


           modify (\c -> c {  cdr_displayedMaskedExternalTelephoneNumber = Just result })
           return ()

         -- NOTE: the real result is the CDR saved/modified in the state
         return ()

  createRateError cdr key descr effect solution
    = createError Type_Error Domain_RATES key descr effect solution
  {-# INLINE createRateError #-}

  !numberOfDigitsToMask = params_digitsToMask env

  !maskText = Text.pack $ L.replicate numberOfDigitsToMask 'X'

  ov :: CDR -> (CDR -> Maybe a) -> Maybe a -> Maybe a
  ov cdr1 f v2 =
    case f cdr1 of
      Just v1 -> Just v1
      Nothing -> v2
  {-# INLINE ov #-}

-- --------------------------------
-- Select a rate and apply it

-- | Calculate the cost of the call, with and without BundleRate application.
--   BundleLimits are not checked, because this is deferred to another thread.
mainRatePlan_calcCost
  :: RateRole
  -> RatingParams
  -> CheckedMainRatePlan
  -> CDR
  -- ^ the call to rate
  -> Either AsterisellError AppliedNormalAndBundleRate

mainRatePlan_calcCost rateRole env mainRatePlan cdr = do
  let !callDate = cdr_calldate cdr

  let !rootUnitId = fromJust1 "err 724454" $ cdr_organizationUnitId cdr

  let !mrp = case rateRole of
               CostRate -> params_costRate env
               IncomeRate -> params_incomeRate env

  (!directPriceCategoryId, !directUnitId)
    <- case info_getPriceCategoryId (params_organizations env) rootUnitId callDate of
         Nothing ->
           throwError $
             createError
               Type_Error
               Domain_RATES
               ("missing price category for unitId - " ++ show rootUnitId)
               ("There is no price-category at date " ++ show callDate ++ ", associated to organization/extension with id " ++ show rootUnitId)
               ("All calls of this organization/extension will not be rated.")
               ("Specify the price-category for the organization or one of its parents.")

         Just r -> return r

  (!normalRateId, _, !normalCalcParams, _) <- fromJust1 "err 7053" <$> selectBestRate env rateRole mrp True cdr Nothing "normal rates" (mainRatePlan_normalRates mainRatePlan)
  let !normalCost = applyRate normalCalcParams
  -- NOTE: the majority of bundle-rates are applied only if there is a cost, so calculate it first
  -- Doing so I can also check if the rate-plan is correct.

  !maybeBundleRate <- selectBestBundleRate env rateRole mrp cdr (directPriceCategoryId, directUnitId)

  !maybeAppliedBundleRate <-
    case maybeBundleRate of
      Nothing -> return Nothing

      Just (!bundleParams, (!bundleRateId, _, !bundleCalcParams, _)) -> do
        case (bundle_onlyForCallsWithACost bundleParams && normalCost == 0) of
          True -> return Nothing
          False ->  do
            let !bundleCost = applyRate bundleCalcParams
            return $ Just (bundleCost, directUnitId, bundleRateId)

  return (Just (normalCost, fromJust1 "err 25" $ cdr_organizationUnitId cdr, normalRateId), maybeAppliedBundleRate)

 where

  applyRate :: CalcParams -> MonetaryValue
  applyRate calcParams = calcParams_calc rateRole calcParams cdr
  {-# INLINE applyRate #-}

-- | Select best bundle-rate, and then the best nested rate, according the matching conditions, but without checking the bundle-limits.
--   Do not apply the rate.
selectBestBundleRate
   :: RatingParams
   -> RateRole
   -> MainRatePlan
   -> CDR
   -> DirectPriceCategory
   -> Either AsterisellError (Maybe (RootBundleRatePlan, SelectedRate))
      -- ^ the best inner-most selected rate

selectBestBundleRate env CostRate mrp cdr _ = Right Nothing
-- there are no bundle-rates for cost rates

selectBestBundleRate env IncomeRate mrp cdr (priceCategoryId, priceCategoryUnitId) = do
  let !rootBundles = mainRatePlan_bundleRates $ params_incomeRate env

  let !validRootBundles
        = if cdr_isServiceCDR cdr
          then [] -- ^ a service CDR is never part of a bundle (in particular it will not decrease bundle limits)
          else L.filter (\b -> L.elem priceCategoryId (bundle_priceCategoryIds b)) rootBundles

  case validRootBundles of
    [] -> Right Nothing

    [b] -> do
      maybeSelectedRate <- selectBestRate env IncomeRate mrp False cdr Nothing (Text.unpack $ bundle_userId b) (bundle_children b)
      case maybeSelectedRate of
        Nothing -> return Nothing
        Just selectedRate -> return $ Just (b, selectedRate)

    _ -> Left $
           createError
               Type_Error
               Domain_RATES
               ("too much bundle rates on price category - " ++ show IncomeRate ++ "-" ++ show priceCategoryId)
               ("The price-category " ++ (Text.unpack $ params_priceCategoryCode env priceCategoryId) ++ " has more than one bundle-rate associated to it.")
               ("All calls of this price category will be not rated.")
               ("Specify only one bundle rate associated to the price-category.")

-- | Select the best nested rate, according matching conditions,
--   in a recursive way, until a leaf is reached.
--   At every recursion pass, select the best rate between all nested rates at the same level.
--   Only leafs can be selected.
--   Manage the `else` part and the nested `use` rates.
--   Do not check bundle-limits.
--   Do not apply the rate.
--
selectBestRate
  :: RatingParams
  -> RateRole
  -> MainRatePlan
  -> Bool       -- ^ True for mandatory rate selection, i.e. an error is signaled if no rate is found
  -> CDR
  -> Maybe RateSystemId
  -> String     -- ^ the parent rate name
  -> [RatePlan] -- ^ the rates to select
  -> Either AsterisellError (Maybe SelectedRate)

selectBestRate env rateRole mrp mandatoryRateSelection cdr maybeRateErrorId parentRateName rates
  = selectBestNestedRate Nothing rates

 where

  rateName :: Maybe RateSystemId -> RateSystemId -> String
  rateName maybeErrorId rid =
    let i = case maybeErrorId of
              Just errorId -> errorId
              Nothing -> rid
    in Text.unpack $ fromJust1 "err 107" $ IMap.lookup i (mainRatePlan_systemIdToUserIdPath mrp)

  rateName2 :: SelectedRate -> String
  rateName2 (i, _, _, _) = rateName Nothing i

  rateNames :: [RateSystemId] -> String
  rateNames is = L.concat $ L.intersperse ", " $ L.map (rateName Nothing) is

  rateNames2 :: [SelectedRate] -> String
  rateNames2 is = rateNames $ L.map (\(p, _, _, _) -> p) is

  mm :: Maybe RateSystemId -> RateSystemId -> RateSystemId
  mm Nothing i = i
  mm (Just i) _ = i
  {-# INLINE mm #-}

  parentRateNameDescr :: Maybe SelectedRate -> String
  parentRateNameDescr Nothing
    = "One of " ++ show rateRole ++ " " ++ parentRateName ++ " rates must match the CDR. "
  parentRateNameDescr (Just (selectedRateId, _, _, _))
    = "The " ++ show rateRole ++ " rate \"" ++ rateName Nothing selectedRateId ++ "\" is matching the CDR, and the CDR must be rated by one of its nested rates. "

  selectBestNestedRate
    :: Maybe SelectedRate -- ^ the parent rate if exists
    -> [RatePlan] -- ^ the rates to select
    -> Either AsterisellError (Maybe SelectedRate)

  selectBestNestedRate maybeParentRate nestedRates = do
    !r <- M.foldM (selectBestMatch maybeRateErrorId) (matchStrenght_initial, []) nestedRates
    case r of
      (_, [(selectedRateId, selectedRate, selectedCalc, selectedMatch)]) -> do
         let !calc2
               = case maybeParentRate of
                        Nothing -> selectedCalc
                        Just (_, _, parentCalc, _) -> calcParams_override parentCalc selectedCalc
         case rate_children selectedRate of
           [] -> return $ Just (selectedRateId, selectedRate, calc2, selectedMatch)
           children -> selectBestNestedRate (Just (selectedRateId, selectedRate, calc2, selectedMatch)) children

      (_, []) ->
        case mandatoryRateSelection of
          False -> return Nothing
          True -> throwError
                    $ createError
                        Type_Error
                        Domain_RATES
                        ("no matching nested rate - " ++ parentRateNameDescr maybeParentRate ++ " - cdr: " ++ cdr_missingRateKey cdr)
                        (parentRateNameDescr maybeParentRate ++ "But none of them can match the CDR.")
                        ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                        ("Correct the rate specification, or contact the assistance.")

      (_, conflictingRates) -> do
        let conflictingRateNames = rateNames2 conflictingRates
        throwError
          $ createError
              Type_Error
              Domain_RATES
              ("too much nested rate - " ++ parentRateNameDescr maybeParentRate ++ " - cdr: " ++ cdr_missingRateKey cdr)
              (parentRateNameDescr maybeParentRate ++ "But Asterisell can not select the best matching nested rate, because there are two or more rates with the same matching strength. The conflicting rates are \"" ++ conflictingRateNames ++ "\".")
              ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
              ("Correct the rate specification, or contact the assistance.")

  -- | Favour the best match.
  selectBestMatch
    :: Maybe RateSystemId
    -> (MatchStrenght, [SelectedRate])  -- ^ the current best match
    -> RatePlan                         -- ^ the rate to evaluate
    -> Either AsterisellError (MatchStrenght, [SelectedRate])  -- ^ the new best match

  selectBestMatch maybeErrorId (match1, results1) nestedRate = do
    r2 <- matchWithUseAndElse maybeErrorId nestedRate
    case r2 of
      Nothing
        -> return (match1, results1)
      Just (rateId2, rate2, calc2, match2)
        -> case match_ord match1 match2 of
             Nothing
              -> let name1 = rateName maybeErrorId rateId2
                     name2 = rateNames2 results1
                 in  throwError
                       $ createError
                           Type_Error
                           Domain_RATES
                           ("incompatible match rate conflict - " ++ show rateRole ++ "-" ++ name1 ++ "-" ++ name2)
                           ("The " ++ show rateRole ++ " rate " ++ name1 ++ ", conflicts with rates " ++ name2 ++ ", because they match the same CDR using two different but incompatibles matching criteria, and Asterisell does not know how to select the best matching rate.")
                           ("Many CDRS will be not rated.")
                           ("Correct the rate specification, or contact the assistance.")
             Just EQ
               -> return (match1, (rateId2, rate2, calc2, match2):results1)
             Just LT
               -> return (match2, [(rateId2, rate2, calc2, match2)])
             Just GT
               -> return (match1, results1)

  -- | If the match fails, try the else part.
  matchWithUseAndElse :: Maybe RateSystemId -> RatePlan -> Either AsterisellError (Maybe SelectedRate)
  matchWithUseAndElse maybeErrorId ratePlan = do
    r <- matchWithUse maybeErrorId ratePlan
    case r of
      Just matched -> return $ Just matched
      Nothing
        -> case rate_elsePart ratePlan of
             [] -> return Nothing
             elseRates -> selectBestRate env rateRole mrp False cdr maybeErrorId (rateName maybeErrorId (rate_systemId ratePlan) ++ " else part") elseRates

  -- | Complete the rate match putting the rate match in logical AND with the rate_use part,
  --   and the rate calculations params overriding the rate_use calculations params.
  matchWithUse :: Maybe RateSystemId -> RatePlan -> Either AsterisellError (Maybe SelectedRate)
  matchWithUse maybeErrorId ratePlan
    = case (rate_matchBeforeUse ratePlan) env cdr of
        Nothing
          -> Right Nothing
        Just (match1, calc1)
          -> case rate_use ratePlan of
               Nothing
                 -> Right $ Just (mm maybeErrorId (rate_systemId ratePlan), ratePlan, calc1, match1)
               Just externalRateReference
                 -> case env_getRate env externalRateReference (cdr_calldate cdr) of
                      Nothing ->
                        Left $ createError
                                    Type_Error
                                    Domain_RATES
                                    ("unknown external rate name - " ++ show rateRole ++ "-" ++ Text.unpack externalRateReference)
                                    ("The " ++ show rateRole ++ " external rate reference \"" ++ Text.unpack externalRateReference ++ "\", does not exists at date " ++ showLocalTime (cdr_calldate cdr))
                                    ("All CDRs with a similar format can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                                    ("Correct the rate main rate specification, or contact the assistance.")

                      Just (useRate, _) ->
                        case L.null (mainRatePlan_bundleRates useRate) of
                          False ->
                            Left $ createError
                                        Type_Error
                                        Domain_RATES
                                        ("external rate name with bundle params - " ++ show rateRole ++ "-" ++ Text.unpack externalRateReference)
                                        ("The " ++ show rateRole ++ " external rate reference \"" ++ Text.unpack externalRateReference ++ "\", exists at date " ++ showLocalTime (cdr_calldate cdr) ++ ", but it has bundle children, and it can not be embeded correctly in a rate plan.")
                                        ("CDRS can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                                        ("Correct the main rate specification, or contact the assistance.")
                          True ->
                            case mainRatePlan_normalRates useRate of
                              [] -> Right Nothing
                              [useRatePlan] ->
                                let actualRateId = mm maybeErrorId (rate_systemId ratePlan)
                                in case matchWithUseAndElse (Just actualRateId) useRatePlan of
                                     Left err -> Left err
                                     Right Nothing -> Right Nothing
                                     Right (Just (_, ratePlan2, calc2, match2)) ->
                                       Right $ Just (actualRateId, ratePlan2, calcParams_override calc1 calc2, match_combineWithChild match1 match2)
                                       -- NOTE: in case of ExternalRates, return the last rate in the RatePlan
                              _ ->
                                Left $ createError
                                         Type_Error
                                         Domain_RATES
                                         ("external rate name with unexpected format - " ++ show rateRole ++ "-" ++ Text.unpack externalRateReference)
                                         ("The " ++ show rateRole ++ " external rate reference \"" ++ Text.unpack externalRateReference ++ "\", exists at date " ++ showLocalTime (cdr_calldate cdr) ++ ", but it has an unexpected internal format, and it can not evaluated.")
                                         ("CDRS can not be rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                                         ("This is an error in the application. Contact the assistance, signaling a problem of type MULTIPLE-RATE-PLANS-FOR-AN-EXTERNAL-RATE-REFERENCE")

-- | Given a Cdr return some of its key values, for reducing the number of errors.
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
          mAppend l = L.foldl' (\r t -> Text.append r t) Text.empty l

      in  Text.unpack $ mAppend ((addI cdr_communicationChannelTypeId):(add4 cdr_externalTelephoneNumberWithAppliedPortability):(Text.pack $ show $ cdr_direction cdr):(addI cdr_priceCategoryId):[])

-- ---------------------------------------
-- Apply explicit rate

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

-- --------------------------------
-- BundleState processing

-- | Fast test to use before calling `bundleState_serviceCdrs`
--   Because the income-rate-plan is constant on the entire rating time-frame,
--   for every closed bundle-rate, there is a corresponding new open bundle-rate.
bundleState_areThereServiceCdrs :: BundleState -> CallDate -> Bool
bundleState_areThereServiceCdrs (Nothing, _) _ = False
bundleState_areThereServiceCdrs (Just minimumCallDate, _) callDate = callDate >= minimumCallDate
{-# INLINE bundleState_areThereServiceCdrs #-}

-- | Create a new BundleRecord for all RootBundleRatePlans.
--   To call for initializing a BundleState.
rootBundleRate_initAllBundleRates
  :: RatingParams
  -> CheckedMainRatePlan
  -> NextScheduledLocalTime
  -- ^ the starting date of the time-frame
  -> (BundleState, Chunk ServiceCDR, ConflictingUnitIds)

rootBundleRate_initAllBundleRates env mainRatePlan fromCallDate
  = case mainRatePlan_bundleRates mainRatePlan of
      [] -> ((Nothing, IMap.empty), V.empty, ISet.empty)
      bs ->
        let (br, services, conflicts) =
              L.foldl'
                  (\(state1, services1, unitIds1) b
                       -> let (state2, services2, unitIds2) = rootBundleRate_initBundleRecord env mainRatePlan b fromCallDate
                          in  (IMap.union state1 state2, services2:services1, ISet.union unitIds1 unitIds2)
                           -- DEV-NOTE: I can make an union because in reality all unitIds are distinct
                  ) (IMap.empty, [], ISet.empty) bs
       in ((bundleRecord_nextScheduledLocalTime br, br), V.fromList $ L.concat services, conflicts)

 where

   mmin :: Maybe CallDate -> Maybe CallDate -> Maybe CallDate
   mmin Nothing d2 = d2
   mmin d1 Nothing = d1
   mmin (Just d1) (Just d2) = Just $ min d1 d2
   {-# INLINE mmin #-}

-- | Check if there are ServiceCDRS to generate,
--   and update the BundleState in case.
--   This function is fast to call, and it can be called for each CDR.
bundleState_maybeUpdateAndGetServiceCDRS
  :: RatingParams
  -> CheckedMainRatePlan
  -> BundleState
  -> CallDate
  -> Maybe ( BundleState        -- ^ the new BundleState
           , [ServiceCDR] -- ^ @ensure ordered by CallDate according R7
           , ConflictingUnitIds
           )
     -- ^ Nothing if the BundleState is still open

bundleState_maybeUpdateAndGetServiceCDRS env mainRatePlan bundleState1 callDate
  = if bundleState_areThereServiceCdrs bundleState1 callDate
    then (Just $ bundleState_updateAndGetServiceCdrs env mainRatePlan bundleState1 callDate)
    else Nothing
{-# INLINE bundleState_maybeUpdateAndGetServiceCDRS #-}

-- | Close the time-frames, and open new-time frames, according the specified call-date.
bundleState_updateAndGetServiceCdrs
  :: RatingParams
  -> CheckedMainRatePlan
  -> BundleState
  -> CallDate
  -> (BundleState, [ServiceCDR], ConflictingUnitIds)
     -- ^ the ServiceCDRs of the  bundle rates activated in next time-frame

bundleState_updateAndGetServiceCdrs env mainRatePlan bundleState1 callDate = fRec bundleState1

 where

  fRec :: BundleState -> (BundleState, [ServiceCDR], ConflictingUnitIds)
  fRec bundleState1@(minimumCallDate1, state1)
    = case bundleState_areThereServiceCdrs (minimumCallDate1, state1) callDate of
        False
          -> (bundleState1, [], ISet.empty)
        True
          -> let
                 (bundleState2, services2, conflicts2) = bundleState_updateToNextTimeFrame env mainRatePlan bundleState1
                 (bundleState3, services3, conflicts3) = fRec bundleState2
                 -- call recursively until the calldate is not reached. In this way if there are holes in the rating, procces,
                 -- the ServiceCDRS are generated for all missing timeframes.
             in  (bundleState3, services2 ++ services3, ISet.union conflicts2 conflicts3)

bundleParams_scale :: BundleParams -> Rational -> BundleParams
bundleParams_scale r p
  = if p == 1
    then r
    else r {
             bundle_leftDuration = scaleI (bundle_leftDuration r)
           , bundle_leftCalls = scaleI (bundle_leftCalls r)
         }

 where

  scaleI :: Maybe Int -> Maybe Int
  scaleI Nothing = Nothing
  scaleI (Just v) = Just (fromInteger $ round $ (toRational v) * p)

-- | Test if the bundle rate respect the bundle limits, and in case decrease them.
--   Consider the entire hierarchy of nested rates.
--
--   @require the CDR is inside the bundle time-frame
--
--   DEV NOTES: this code must be fast because it is called for each CDR.
bundleRecord_applyIfPossible
  :: MainRatePlan
  -> BundleRecord
  -> BundleUnitId
  -> CDR
  -> RateSystemId
  -> Maybe BundleRecord
     -- ^ Nothing if it can not be applied

bundleRecord_applyIfPossible mrp bundleRec1 unitId cdr startRateId = do

  let (!activationDate, !endCallDate, !ratio, !rateIdToBundleLimits) = fromJust1 "rp7474" $  IMap.lookup unitId bundleRec1

  let !callDate = cdr_calldate cdr

  when (callDate < activationDate) (fail "bundle not yet active")
  -- DEV-NOTE: the `fail` indeed return simply Nothing in the Maybe Monad

  rateIdToBundleLimits' <- apply rateIdToBundleLimits startRateId

  return $ IMap.insert unitId (activationDate, endCallDate, ratio, rateIdToBundleLimits') bundleRec1

 where

   apply :: RatePlanIdMap BundleParams -> RateSystemId -> Maybe (RatePlanIdMap BundleParams)
   apply m1 rateId = do
     !m2 <- case IMap.lookup rateId m1 of
              Nothing -> return m1
              Just bundleParams -> do
                bundleParams' <- bundleParams_apply bundleParams cdr
                return $ IMap.insert rateId bundleParams' m1

     let !rate = fromJust1 "mrp 0753" $ IMap.lookup rateId (mainRatePlan_systemIdToRatePlan mrp)

     case rate_parentSystemId rate of
       Nothing -> return m2
       Just parentSystemId -> apply m2 parentSystemId

-- | Apply the call, decreasing the bundle limits.
--   DEV NOTES: this code must be fast because it is called for each CDR.
bundleParams_apply
  :: BundleParams
  -> CDR
  -> Maybe BundleParams
     -- ^ Nothing if the bundle can not be applied because limits are not respected

bundleParams_apply r1 cdr = do
  leftDuration <- removeFromLimit (bundle_leftDuration r1) callDuration
  leftCalls <- removeFromLimit (bundle_leftCalls r1) countOfCalls
  return $ r1 {
                 bundle_leftCalls = leftCalls
               , bundle_leftDuration = leftDuration
               }

 where

  !countOfCalls = cdr_countOfCalls cdr

  !callDuration = fromJust1 "err bs6675" $ cdr_billsec cdr

  removeFromLimit :: Maybe Int -> Int -> Maybe (Maybe Int)
  removeFromLimit Nothing _ = return Nothing
  removeFromLimit (Just v1) v2
    = let v3 = v1 - v2
      in case v3 >= 0 of
           True -> return $ Just v3
           False -> fail "limit not respected"
{-# INLINE bundleParams_apply #-}

-- | Create a new BundleRecord for a RootBundleRatePlan, and its children.
--
--   Take in consideration the activation date of a UnitId inside a bundle-time frame.
--   By design, it does not consider the ending date of a UnitId, but only the first
--   date in which it enters in the bundle time-frame.
--
--   The UnitId are initialized in a way proportional to their presence in the bundle time frame,
--   but only related to the date of when they enter in the bundle,
--   not the date from when they leave the time-frame.
--   This is sound, because a UntId can enter in a bundle, use all its resources,
--   then exit, but he must pay all the bundle because the exit date is not so significant.
--
--   @require there is a unique bundle for each distinct price-category.
--   DEV NOTE: this error is signaled by the rating processing code,
--   while this function will work in a wrong way.
--
--   DEV NOTE: this function is called only at the end of each bundle time frame, so it can be also slow.
rootBundleRate_initBundleRecord
  :: RatingParams
  -> CheckedMainRatePlan
  -> RootBundleRatePlan
  -> NextScheduledLocalTime
  -- ^ the starting date of the time-frame
  -> ( BundleRecord
     , [ServiceCDR]
     , ConflictingUnitIds -- ^ UnitIds with conflicts (i.e. more than one bundle PriceCategory in the Bundle TimeFrame)
     )

rootBundleRate_initBundleRecord env mainRatePlan rootBundleRate fromCallDate
  = let -- The unitIds with a possible direct assignment to the (usually few) BundlePriceCategoryIds
        -- of the bundle-rate.
        -- DEV-NOTE: this is a fast operation to do, and they will be filtered/refined later,
        -- by function `f`.
        candidateUnitIds1 :: ISet.IntSet
        candidateUnitIds1
          = ISet.foldl'
              (\r priceCategoryId
                -> case IMap.lookup priceCategoryId (info_directPriceCategories unitInfo) of
                     Nothing
                       -> r
                     Just unitIds
                       -> addToSet r $ L.map unit_id unitIds
              ) ISet.empty bundlePriceCategoryIds

        (!bundleRecord, !services, !conflictingUnitIds) =
            ISet.foldl' addUnitId (IMap.empty, [], ISet.empty) candidateUnitIds1

    in  (bundleRecord, fromRight1 "ERR 244" (serviceCDRS_rate env services), conflictingUnitIds)

 where

  !precision = params_currencyPrecision env
  !rootName = bundle_userId rootBundleRate
  !bundleCost = bundle_initialCost rootBundleRate
  !bundlePriceCategoryIds = ISet.fromList $ bundle_priceCategoryIds rootBundleRate
  !unitInfo = params_organizations env
  !bundleTimeFrame@(bundleFromCallDate, bundleToCallDate) = timeFrame_fromCallDate rootBundleRate fromCallDate
  !bundleTimeFrameDuration = timeFrame_duration (bundleFromCallDate, bundleToCallDate)
  !proportionalLimits = bundle_limitsAreProportionalsToActivationDate rootBundleRate
  !bundleParams = L.foldl' (\x y -> toBundleParamsMap y x) IMap.empty (bundle_children rootBundleRate)


  addToSet :: ISet.IntSet -> [UnitId] -> ISet.IntSet
  addToSet s unitIds = L.foldl' (\s' unitId -> ISet.insert unitId s') s unitIds
  {-# INLINE addToSet #-}

  -- | A UnitId is selected if it has a direct assignment to the price-category, during the bundle rate timeframe.
  --   DEV NOTE: work efficiently if there are not too much direct info associated to a UnitInfo, as usual it is the case.
  addUnitId :: (BundleRecord, [ServiceCDR], ConflictingUnitIds) -> UnitId -> (BundleRecord, [ServiceCDR], ConflictingUnitIds)
  addUnitId (m1, services1, conflictingUnitIds1) unitId =
    let (!v1, !v2, !v3, !v4) =
          L.foldl'
            (addUnitDataInfo unitId)
            (m1, services1, conflictingUnitIds1, IMap.empty)
            (fromJust1 "rp8" $ IMap.lookup unitId (info_organizations unitInfo))
    in (v1, v2, v3)
  {-# INLINE addUnitId #-}

  addUnitDataInfo
      :: UnitId
      -> (BundleRecord, [ServiceCDR], ConflictingUnitIds, UnitIdMap CallDate)
      -> DataInfo -- ^ in reverse order of declaration
      -> (BundleRecord, [ServiceCDR], ConflictingUnitIds, UnitIdMap CallDate)

  addUnitDataInfo unitId (m1, services1, conflictingUnitIds1, checkUnitIds1) d
    = let !unitFromCallDate = structure_from d
          (!isProperAssignment, !isCheckAssigment, !isThereConflict) =
            case (structure_exists d, structure_rateCategoryId d) of
              (False, _) -> (False, False, False)  -- ^ the data info was removed (does not exists any more)
              (True, Nothing) ->  (False, False, False) -- ^ it is not a direct assignment of PriceCategory
              (True, Just categoryId) ->
                case unitFromCallDate < bundleToCallDate of
                  False -> (False, False, False) -- ^ this assignment is done in the future respect the bundle
                  True ->
                    case IMap.lookup unitId checkUnitIds1 of
                      Just oldActivationDate ->
                        case unitFromCallDate >= bundleFromCallDate && oldActivationDate >= bundleFromCallDate of
                          True -> (False, False, True)   -- ^ there is an overwrite in the same BundleTimeFrame
                          False -> (False, False, False) -- ^ this is an older activation, so use the most recent instead
                      Nothing ->
                        case ISet.member categoryId bundlePriceCategoryIds of
                          False -> (False, True, False) -- ^ it is a direct assignment, but not of the required category
                          True -> (True, True, False)   -- ^ this is the mos recent assignment, so use it

          (proportion2, bundleParams2) =
            case proportionalLimits && unitFromCallDate > bundleFromCallDate of
              False -> (toRational 1, bundleParams)
              True -> let !duration = timeFrame_duration (unitFromCallDate, bundleToCallDate)
                          !proportion = (toRational duration) / (toRational bundleTimeFrameDuration)
                          !scaledBundleParams = IMap.map (\p -> bundleParams_scale p proportion) bundleParams
                      in  (proportion, scaledBundleParams)

          !m2 = case isProperAssignment of
                  False -> m1
                  True -> IMap.insert unitId (unitFromCallDate, bundleToCallDate, proportion2, bundleParams2) m1

          !checkUnitIds2 =
            case isCheckAssigment of
                  False -> checkUnitIds1
                  True -> IMap.insert unitId unitFromCallDate checkUnitIds1

          !conflictingUnitIds2 =
            case isThereConflict of
              False -> conflictingUnitIds1
              True -> ISet.insert unitId conflictingUnitIds1

          bundleCost2 = bundleCost * proportion2

          !services2 =
            case isProperAssignment && bundleCost2 > 0 of
              False -> services1
              True -> (generateServiceCDR unitId bundleTimeFrame bundleCost2):services1
                        -- NOTE: use the fixed bundleTimeFrame and not the activationDate for being sure
                        -- to generate serviceCDRS and CDRS in order of date, for respecting the
                        -- requirements of process_cachedGroupedCDRS (i.e. consecutive CDRS)

      in (m2, services2, conflictingUnitIds2, checkUnitIds2)

  toBundleParamsMap :: RatePlan -> RatePlanIdMap BundleParams -> RatePlanIdMap BundleParams
  toBundleParamsMap p m1
    = let m2 = case rate_bundleParams p of
                 Nothing -> m1
                 Just bp -> IMap.insert (rate_systemId p) bp m1
          m3 = L.foldl' (flip toBundleParamsMap) m2 (rate_children p)
          m4 = L.foldl' (flip toBundleParamsMap) m3 (rate_elsePart p)
      in  m4

  generateServiceCDR :: UnitId -> TimeFrame -> MonetaryValue -> ServiceCDR
  generateServiceCDR unitId (startTimeFrame, endTimeFrame) cost
      = let
            externalTelephoneNumber
              = serviceCdr_defaultExternalTelephoneNumber (bundle_serviceCDRType rootBundleRate) (bundle_serviceCDRDescription rootBundleRate)
              -- NOTE: the rate compilation procedure generate a telephone prefix associated to this telephone number.

            cdr = (cdr_empty startTimeFrame (params_currencyPrecision env)) {
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
                  , cdr_debug_cost_rate = Just $ bundle_userId rootBundleRate
                  , cdr_debug_income_rate = Just $ bundle_userId rootBundleRate
                  }

        in cdr

-- | Close the BundleState with the minimum date, and open new bundles in the same date.
--
--   @require the mainRatePlan is stable (not change) during the change of time-frame,
--   because it assumes that for every bundle closed, there is a bundle to open exactly
--   at the same call-date.
--
--   DEV-NOTES: this function is called only at the end of a time-frame, so it can be also slow.
--   DEV-NOTES: this function is used from `bundleState_updateAndGetServiceCdrs`, and should not called directly.
bundleState_updateToNextTimeFrame
  :: RatingParams
  -> CheckedMainRatePlan
  -- ^ init only the unit-ids of this partition
  -> BundleState
  -- ^ the bundle state to update, in which there can be open time-frames to preserve.
  -> (BundleState, [ServiceCDR], ConflictingUnitIds)

bundleState_updateToNextTimeFrame _ _ (Nothing, bundleState1) = ((Nothing, bundleState1), [], ISet.empty)

bundleState_updateToNextTimeFrame env mainRatePlan (Just bundleEndDate1, bundleState1)
  = let
        -- Remove all closed time BundleState time-frames, from state1,
        -- maintainng not already closed bundle time-frames.
        bundleState2 = IMap.filter (\(_, unitEndDate, _, _) -> unitEndDate > bundleEndDate1) bundleState1

        ((_, bundleState3), services3, conflicts3) =
            L.foldl'
                 (flip processBundleRate)
                 ((Nothing, bundleState2), [], ISet.empty)
                 (L.filter doesBundleRateStartNow $ mainRatePlan_bundleRates mainRatePlan)

        nextScheduled3 = bundleRecord_nextScheduledLocalTime bundleState3
        -- DEV-NOTE: calculating the date here is more robust because we can manage
        -- empty intervals, and overwritten UnitId.

    in  ((nextScheduled3, bundleState3), L.concat services3, conflicts3)

 where

  processBundleRate :: RootBundleRatePlan -> (BundleState, [[ServiceCDR]], ConflictingUnitIds) -> (BundleState, [[ServiceCDR]], ConflictingUnitIds)
  processBundleRate p ((_, s1), ss1, conflicts1)
    = let (s2, ss2, conflicts2) = rootBundleRate_initBundleRecord env mainRatePlan p bundleEndDate1
      in ((Nothing, IMap.union s2 s1), ss2:ss1, ISet.union conflicts1 conflicts2)
      -- DEV-NOTE: I can make an union because in reality all unitIds are distinct
      -- DEV-NOTE: if there are unitIds with open bundleRecords, they will be overwrite by union,
      -- and this is correct, according requirements R1, R2, R3 and R4 (union is left-biased)

  doesBundleRateStartNow :: RootBundleRatePlan -> Bool
  doesBundleRateStartNow p = (fst $ timeFrame_fromCallDate p bundleEndDate1) == bundleEndDate1
  {-# INLINE doesBundleRateStartNow #-}

-- ---------------------------------------------------
-- Service CDRS
-- These are the explicitely assigned services,
-- and not the BundleRate services.

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

          cdrs2OrError = L.foldl' extractError (Right []) $ L.map processServiceInTimeFrame allTimeFrames

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
                                                 _ -> pError "never used"

                          calculatedCdrs = L.concatMap calcService calculatedServices

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

                                                     False -> let maxItems = L.maximum $ items:(L.map calcService_nrOfItems calcs1)
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
                                          maybeDescr = case L.null descr of
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

-- | Export the telephone prefixes to use for ServiceCdrs.
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

-- ---------------------------------------
-- Normalize rate-plan

-- | Load/init complete RatingParams.
--   DEV-NOTE: decide also how to split the rating time-frame, and how to sanitize the rating time-frame.
--   DEV-NOTE: it should work when there are main rate plans with bundle-rates, or when there are no saved bundle states.
--
--   The idea it is to support efficiently the incremental rating of CDRS, saving the previous calculated BundleState,
--   and starting from here.
--   All other rating requests will be start from the begin of the BundleState.
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
      (bundleStartAtCallDate, previousBundleStartAtCallDate, bundleEndAtCallDate)
        <- case env_getRate p (mainRatePlanRef IncomeRate) (params_fromDate p) of
             Nothing
               -> liftIO $ throwIO $ AsterisellException $ "The \"main-income-rate\" is missing at date " ++ (show $ fromLocalTimeToMySQLDateTime $ params_fromDate p)

             Just (ratePlan, _)
               -> let t0 = mainRatePlan_getBundleRateStartCallDate ratePlan (params_fromDate p)
                      t0End = mainRatePlan_getBundleRateEndCallDate ratePlan t0

                      -- subtract 1 second
                      tz = minutesToTimeZone 0
                      t1 = addUTCTime (-1) (localTimeToUTC tz t0)
                      t2 = utcToLocalTime tz t1

                      t3 = mainRatePlan_getBundleRateStartCallDate ratePlan t2

                  in  return (t0, t3, t0End)

      --
      -- Check if the Main Rate Plan is well formed.
      --

      let checkP1 = ratingParams_empty $ p0 { iparams_fromDate = bundleStartAtCallDate }
      checkP2 <- liftIO $ calcStableTimeFrame conn checkP1
      case params_isShorterSafeTimeFrame checkP2 of
        Nothing -> return ()
        Just rateUntilNextRatePlan ->
          when (bundleEndAtCallDate > rateUntilNextRatePlan)
               (throwIO $ AsterisellException
                          ("The CDRs must be rated from date " ++  show (fromLocalTimeToMySQLDateTime bundleStartAtCallDate) ++ " (taking in consideration the start of the bundle time-frame), but in this time-frame there are changes in the main income or cost rate plan (note: not the individual referenced CSV rates that can change, but the main rate plan), and Asterisell can not determine which rate plan to use for calculating open bundles."))

      --
      -- Load main rate plans
      --

      p <- get
      incomeRatePlan <- liftIO $ loadMainRatePlan (mainRatePlanRef IncomeRate) p bundleStartAtCallDate
      costRatePlan <- liftIO $ loadMainRatePlan (mainRatePlanRef CostRate) p bundleStartAtCallDate

      modify' $ \p -> p { params_incomeRate = incomeRatePlan
                        , params_costRate = costRatePlan }

      --
      -- Check BundleState rating params. 
      -- In case there is no saved BundleState, CDRS must be processed again from the beginning.
      --
      -- This code had to work also if there are no bundle-rates.
      --
      -- Check also (and signal the error) that all BundleState starts and ends within the billing-time frame,
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
                         -- so we can reuse the incremetal BundleState.

      let params1 = [toDBLocalTime $ params_fromDate p
                     -- ^ consider `<= ?` because the date is exclusive, so all the new CDRS from this date (inclusive)
                     -- to next dates can be added to the old BundleState. In other word, it contains all CDRS before to_time.
                     -- We are saying that the BundleState can not in the future respect the requested rating date,
                     -- bun only in the past.$
                   , toDBLocalTime bundleStartAtCallDate
                    -- ^ consider `> ?` because otherwise it is better starting with an empty bundle.
                    -- We are saying that the main bundle is initializated at this date,
                    -- and if we have a BundleState too much in the past, we can start with a fresh BundleState.
                   , toDBLocalTime beforeThisCallDateIsAnIllegalRerate
                   ]

      (qsDef, qs) <- liftIO $ DB.query conn query1 params1
      maybeRow <- liftIO $ S.toList qs
      liftIO $ DB.skipToEof qs
      !maybeSavedBundleState <-
        case params_isRateUnbilledCallsEvent p of
          True -> return Nothing -- ^ it is likely that IncomeRate was modified, so a new BundleState must be initializated
          False ->
            case maybeRow of
              [[_, savedBundleCallDate, content]] ->
                case (B64.decode $ fromTextToByteString $ fromDBText content) of
                  Left err -> return Nothing
                  Right content2 ->
                    case (DBS.decode $ LZ.decompress content2) of
                       Left err -> return Nothing
                       Right (b :: BundleState) -> do
                         modify' $ \p ->  p { params_initial = (params_initial p) { iparams_fromDate = fromDBLocalTime savedBundleCallDate } }
                         return $ Just b
              [] -> return Nothing
              unexpected -> throwIO $ AsterisellException ("Error 11775 in application code. Unexpected result: " ++ show unexpected ++ ", with column defs " ++ show qsDef)

      let startWithNewBundleState = isNothing maybeSavedBundleState

      when startWithNewBundleState
           (case bundleStartAtCallDate >= beforeThisCallDateIsAnIllegalRerate of
              True -> modify' (\p -> p { params_initial = (params_initial p) { iparams_fromDate = bundleStartAtCallDate }})
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
            = L.foldl' mainRatePlan_externalRateReferences ratesToImport1 (IMap.elems $ params_rates p)

      p' <- liftIO $ ratePlan_loadRates conn p allRatesToImport
      put $ pAssert "ERR 012" (isRight $ ratingParams_respectCodeContracts p') p'

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

      --
      -- Load BundleState only now, because all params are correctly initializated
      --

      case maybeSavedBundleState of
        Just b -> do
          let (_, _, !conflictingUnitIds) = rootBundleRate_initAllBundleRates p (params_incomeRate p) bundleStartAtCallDate
          modify' $ \p ->  p { params_lastSavedBundleState = b
                             , params_conflictingUnitIds = conflictingUnitIds
                             }
        Nothing -> do
          p <- get
          let (bs, services, conflictingUnitIds) =
                rootBundleRate_initAllBundleRates
                  p
                  (params_incomeRate p)
                  bundleStartAtCallDate


          modify' $ \p -> p { params_lastSavedBundleState = bs
                            , params_conflictingUnitIds = conflictingUnitIds
                            , params_initialBundleRateServices = services
                            }

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
                   -- NOTE: assume that the same name is assigned to the same sub-rate, so already saved BundleState remain consistent
                   -- In case of redefinition of a main-rate-plan:
                   -- * a complete rating event is generated on the unbilled time-frame
                   -- * BundleStates start from the beginning in empty state
                   -- * this is correct only if BundleState is initializated at the beginning of the rating time-frame

-- ----------------------
-- Unit tests

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
        bundle_userId = "bundle"
      , bundle_serviceCDRType = Text.pack ""
      , bundle_serviceCDRDescription = Text.pack ""
      , bundle_timeFrame = f
      , bundle_priceCategoryIds = []
      , bundle_limitsAreProportionalsToActivationDate = False
      , bundle_onlyForCallsWithACost = False
      , bundle_initialCost = 0
      , bundle_children
          = [RatePlan {
                        rate_systemId  = 1
                      , rate_parentSystemId = Nothing
                      , rate_userId = ""
                      , rate_bundleParams = Nothing
                      , rate_matchBeforeUse = matchFun_initial
                      , rate_children = []
                      , rate_elsePart = []
                      , rate_use = Nothing
                      }
            ]
      }

  cp1 = calcParams_defaultValues

  cp2 = calcParams_empty {
          calcParams_costOnCall = Just $ RateCost_cost 2
        }

  cp3 = calcParams_empty {
          calcParams_costForMinute = Just 1
        }

  cpO1 = calcParams_override (calcParams_override cp1 cp2) cp3

  cpO2 = calcParams_override cp1 cp2

  cpO3 = calcParams_override cp2 cp3

  cpO4 = cp2

  cdrO1 = (cdr_empty date1 4) {
            cdr_direction = CDR_outgoing
          , cdr_billsec = Just 60
          }

  cdrO1V = toRational 3

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
  serviceMap ss = IMap.fromList $ L.map (\s -> (service_id s, s)) ss

  priceListMap :: [ServicePrice] -> ServiceIdMap [ServicePrice]
  priceListMap ss
    = let sameService s1 s2 = (servicePrice_serviceId s1) == (servicePrice_serviceId s2)
          l1 = L.groupBy sameService ss
          l2 = L.map (\(e:r) -> ((servicePrice_serviceId e), (e:r))) l1
          l3 = L.map (\(i, l) -> (i, L.reverse $ L.sortBy (\x y -> compare (servicePrice_fromDate x) (servicePrice_fromDate y)) l)) l2
      in  IMap.fromList l3

  assignmentMap :: [AssignedService] -> ServiceIdMap (UnitIdMap [AssignedService])
  assignmentMap ss
    = let ss1 = L.foldl' serviceParams_insertAssignment IMap.empty ss
          ss2 = IMap.map (\m -> IMap.map (\s -> L.reverse $ L.sortBy (\x y -> compare (assignedService_fromDate x) (assignedService_fromDate y)) s) m) ss1
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

          c2 = L.sort $ L.map extract c1
          e2 = L.sort e1

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
      , params_servicePrices = priceListMap $ (L.concatMap generateCommonSp1 $ L.map service_id $ [s3_1, s3_2, s3_3, s3_4, s3_5])
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
      , params_servicePrices = priceListMap $ (L.concatMap generateCommonSp1 $ L.map service_id $ [s3_1, s3_2, s3_3, s3_4, s3_5])
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
