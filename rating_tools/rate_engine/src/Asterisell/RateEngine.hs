{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Rate CDRs.
--   This is mainly imperative code:
--   * it starts with the content of DB tables
--   * it updates them in an incremental way
--   * the code had to preserve the invariants of the tables
--   * in an ideal world, one had to prove that the code preserve them
--   * in practice I will list them, and I will review and check the code accordingly
--
--   The features the code had to have:
--   * F1) efficient incremental rating of new normal calls
--   * F2) efficient incremental rating of BundleRates
--
--   The aspects the code had to take in account:
--   * A1: CDR rating
--   * A2: BundleState
--   * A3: save partial BundleState for incremental rerating
--   * A4: decoration of errors
--   * A5: avoid repeated errors
--   * A6: count/group stats about CDRS and errors
--   * A7: BundleService
--   * A8: ExpandedExtension
--
--  The data-structures properties that the code had to respect are:
--  * D1) in ar_cdr there are normal calls (CDRS) corresponding the rating of ar_source_cdr
--  * D2) in ar_cdr there are service CDRS associated to bundle-rates defined in the main-income-rate-plan
--  * D3) in ar_cdr there are pure service CDRS (services) defined in the ar_service table
--  * S1) bundle-services and pure services of not yet closed time-frame had to be inserted in advance,
--    so the customer can see them on the call-report
--  * S2) in case of changes of bundle-services and pure-services specifications, the unbilled time-frame must be updated accordingly
--  * S3) services must be inserted only the correct number of times
--  * B1) the bundle-state table contains the current situation of bundle-rates of customers
--  * B2) service CDRS (also imported from provider) are not used to update bundle-state, because they are not calls made from customers
--  * B3) the bundle-rates starts at the beginning of the billing time-frame, or inside it, and ends before the end of the billing time-frame
--  * B4) inside a billing time-frame, there can be only one active version of main-income-rate-plan, and main-cost-rate-plan
--  * B5) inside a billing time-frame there cane be changes of prices in other CSV rates
--  * B6) every change of MainIncomeRate is not lost, and a complete rerating is scheduled again (despite current rerating), and a new BundleState is calculated
--  * C1) all new CDRS must be rated and inserted in the DB
--  * C2) CDRS to ignore can be not inserted
--  * C3) already billed CDRS can not be rated again
--  * C4) CDRS to ignore are sent to the processing chain, and not filtered, because they can contain info about CallDate and ServiceCDRS to generate
--  * C3) if a type of CDR (call, bundle-rate, pure-service) is deleted before rerating, it must be generated in the same rating-phase, and viceversa
--  * G1) ar_cached_grouped_cdrs and ar_cached_errors must contains the grouped and cached sums of all values
--  * T1) tables must be compact and compressed
--  * D1) there can be more than one instance running at the same time, on the same host, so use unique file names
--  * D2) there can be only one rating process running at the same time for each instance
--  * M1) data structures shared by multiple threads had to be protected by MVar or similar things
--  * M2) there can be only one thread owning a DB Connection
--  * R1) a BundleRecord for each unitId is opened only for UnitId with direct assignment to the PriceCategory of the BundleRate
--  * R2) a UnitId can have only one open BundleRecord in a BundleTimeFrame
--  * R5) BundleRateServices will be generated for new activated BundleRecord but not for closed BundleRecords
--  * R6) AsterisellError should be light to generate, because they will be enriched with CDR details from dedicated process, and only if the AsterisellErrorKey is new. Otherwise in case of many CDRS with erorrs, the rating process can be too much slow.
--  * R7) CDRS and ServiceCDRS must be generated more or less in CallDate order, in particular within the same days, so CachedGroupedCDRS can cache them, and the BundleState can be updated correctly/fair.
--
--  The code respect these rules in this way:
--  * in case of change of rating params (also services and bundle-rates specifications)
--    the entire unbilled time-frame is rerated (S2, C3)
--  * a service is inserted at the beginning of its time-frame, and not when it is closed (S1, S3)
--  * every inserted CDR is in the rating time-frame, and the cached values are updated according this rate time-frame, so C1 is respected
--  * during each rerating past CDRS are deleted, and new rated CDRS inserted,
--    but the tables have no IDS, but they use calldate as primary (clustered) key,
--    and so there is no waste of space (T1)
--  * there is a consecutive ID used for avoiding clash of CDRS with the same date (T1)
--  * bundle-state is saved few minutes before the last call, so if there are pending calls not yet
--    inserted, they do not invalidate it. (F2)
--  * when new ar_source_cdr are inserted, only these new calls are rated, starting from the first calldate (F1)
--    This assumes that calls are received in order of calldate.
--    In case this is not true, the entire unbilled time-frame will be rated again.
--  * tables are saved in TokuDB with snappy compression (T1)
--  * ar_cached_... tables are updated with the new rated CDRS (G1).
--    NOTE: a service is inserted only if we are rating its opening date, so it is inside
--    the update interval
--  * the rating time-frame is split in parts where there is a unique main-income-rate-plan,
--    in this way bundle-rates time-frame are predictable, and every time a bundle-rate
--    is closed, a bundle-rate of the same type is open. This simplify the code.
--  * the B3 condition permits to start a new billing time-frame with an empty bundle-rates state,
--    and this simplifie a lot the code, because it needs to be incremental only within a billing time-frame
--
module Asterisell.RateEngine (
  rateEngine_rate,
  RunLevel(..),
  RunLevelT(..),
  runLevel_default,
  rateEngine_updateCachedGroupedCDRS,
  rateEngine_openDBAndUpdateCachedGroupedCDRS,
  tt_rateSpecificationsTests,
  tt_mainRateCalcTets
) where

import Asterisell.DB
import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.ParseRatePlan
import Asterisell.ApplyRatePlan
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.CustomerSpecificImporters

import qualified Data.Trie.BigEndianPatricia.Base as Trie (size)
import GHC.Generics (Generic)
import Data.List as L
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Control.Monad.IO.Class       (liftIO)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Time.LocalTime
import qualified Data.Text as Text
import qualified Test.HUnit as HUnit
import qualified Data.Csv as CSV
import qualified Data.Csv.Streaming as S
import System.IO as IO
import Debug.Trace
import Data.Maybe
import Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet
import Data.IORef
import qualified Data.Serialize as DBS
import qualified Data.ByteString.Base64 as B64
import Data.Foldable as F
import qualified Codec.Compression.QuickLZ as LZ
import Control.Monad.Except as M
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.HashTable.IO as IMH
import System.IO.Streams as S
import Data.Either

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Database.MySQL.Base as DB
import Text.Heredoc
import Control.DeepSeq as DeepSeq

-- ---------------------------------
-- Params

rateGarbageKey :: RatingParams -> BS.ByteString
rateGarbageKey env = "rating-process-cdrs"
{-# INLINE rateGarbageKey #-}

-- | Number of CDRs to process in each phase: parse, rate, write to DB.
--   A bigger chunk reduces the number of DB round-trips, because more CDRs are sent to the DB,
--   and maximize the CPU cache usage.
--   A bigger chunk optimizes also the CPU cache usage, because the same work is done on a chunk of CDRS,
--   instead of changing for each CDR the working context. So in each phase the results are forced to be
--   under DeepSeq, and fully evaluated.
param_chunkSize :: Int
param_chunkSize = 512
{-# INLINE param_chunkSize #-}

-- | The number of source CDRs to fetch from the DB, before splitting the rating event.
--   Up to date the Haskell MySQL driver does not support the forward-only cursor mode,
--   and it stores all the CDRS of the rating query in RAM.
--   So the query is splitted (efficiently) in smaller queries, and they are rated separately.
--   NOTE: 30K CDRs requires approximately 100M - 200M of RAM (according the number of rates to load)
--   NOTE: the cost of splitting the query is rather neglible.
param_chunkSize2 :: Int
param_chunkSize2 = 1024 * 30
{-# INLINE param_chunkSize2 #-}

data RunLevel = RunLevel {
                  runLevel_type :: RunLevelT
                , runLevel_cores :: Int -- ^ 0 for using all the cores
                , runLevel_fastGroupedCDRS :: Bool -- ^ True for fast calculation of grouped CDRS (experimental)
                         }
 deriving (Eq, Ord, Show, Generic, NFData)

runLevel_default :: RunLevel
runLevel_default = RunLevel { runLevel_type = RunLevelFull, runLevel_cores = 0, runLevel_fastGroupedCDRS = False }

-- | Used for benchmarking the application, bypassing some part of the execution.
data RunLevelT
  = RunLevelFull
  -- ^ complete/normal rating process
  | RunLevelSkipUpdateCachedGroupedCDRS
  -- ^ complete rating, but skip the expensive calculation of grouped CDRS at the end
  | RunLevelSkipDBWrite
  -- ^ write to a file instead to the DB,
  --   and RunLevelSkipUpdateCachedGroupedCDRS
  --   and RunLevelFakePortedTelephoneNunmbers,
  --   so test the raw speed of CDR rating, without DBMS roundtrips
  | RunLevelFakePortedTelephoneNunmbers
  -- ^ complete rate, but without calculating ported telephone numbers,
  -- and RunLevelSkipUpdateCachedGroupedCDRS
 deriving (Eq, Ord, Show, Generic, NFData)

-- -------------------------------------------------------------
-- Rating of CDRs

-- | Rate the CDRs on the source data file.
--   This is the entry point of the rating process, and it coordinates all the rest:
--   * signal the changed days for other maintanance jobs
--   * recalculate cached sum of days
--
--   The rating-time frame event is reset on the PHP side.
--
--   The main paradigm is this:
--   * CDRs with errors are reported as CDR with the error field set, so the online call report can report them
--   * CDRs with errors generates an error message/description
--   * errors of type Type_Error are associated to the single CDR, and rating process continue
--   * errors of type Type_Critical interrupt the rating process, signaling as unrated all next CDRS.
--   * the rating process must be conservative: in doubt an error message is raised, and rating is blocked.
--
--   DBMS guidelines:
--   * a DB connection is opened when there is yet ready data to process, and not before, for avoiding lock-timeout problems
--   * DB connections are released when there is no more activity, for avoiding lock-timeouts problems
--
--   See also:
--   * comments on Cmd_deleteCDRS about the logic of (re)rating of calls
--   * notes in the module heder
--
-- NOTE: in case of succesfull rating, the pending rate-event is deleted from the `ManageRateEvent.php` caller
--
rateEngine_rate
  :: RunLevel
  -> InitialRatingParams
  -> IO Int
  -- ^ the number of processed CDRs.
  --   Normal errors are added directly to the error table, and associated to rated CDRs.
  --   Critical errors are signaled using an exception, because they are critical and they block the entire rating process.

rateEngine_rate runLevelR initialParams = do
  nrOfRatingJobs <- process_initCores (runLevel_cores runLevelR)
  rateAllRatingTimeFrames initialParams nrOfRatingJobs 0

 where

  runLevel = runLevel_type runLevelR

  rateAllRatingTimeFrames currParams nrOfRatingJobs countCDRS1 = do
    env <- ratePlan_loadRatingParams currParams
    let maybeRateToCallDate
          = case (params_isShorterSafeTimeFrame env) of
              Nothing -> Nothing
              Just toCallDate -> Just toCallDate

    countCDRS2 <- rateEngine_rateProcess runLevelR env nrOfRatingJobs maybeRateToCallDate
    let totCDRS = countCDRS1 + countCDRS2
    case maybeRateToCallDate of
      Nothing
        -> do return totCDRS
      Just toCallDate
        -> do let newParams = currParams { iparams_fromDate = toCallDate }
              rateAllRatingTimeFrames newParams nrOfRatingJobs totCDRS

-- | Called from `rateEngine_rate`: rate a stable time-frame in which there are no differences of main rate-plan.
rateEngine_rateProcess
  :: RunLevel
  -> RatingParams
  -> Int
  -- ^ number of rating jobs
  -> Maybe CallDate
  -- ^ Nothing for rating all the calls, or a limit to use, where the main rate-plan is stable
  -> IO Int
  -- ^ the number of processed CDRs.
  --   Normal errors are added directly to the error table, and associated to rated CDRs.
  --   Critical errors are signaled using an exception, because they are critical and they block the entire rating process.

rateEngine_rateProcess runLevelR env nrOfJobs maybeToCallDate = do
  let runLevel = runLevel_type runLevelR
  let fastGrouping = runLevel_fastGroupedCDRS runLevelR

  let pipeName = "/var/tmp/import_rated_cdrs_pipe__" ++ params_dbName env

  let !isDebugMode = params_isDebugMode env
  let !precisionDigits = params_currencyPrecision env
  let !dbConf = params_dbConf env
  let !isOfficialRatingEvent = params_isRateUnbilledCallsEvent env

  let listOfRatingJobs = V.fromList [0 .. (nrOfJobs - 1)]

  withResource'
    (db_init dbConf (params_debugFileName env) (params_currencyPrecision env))
    (\dbState -> do
       parsedCDRSChan :: OrderedStream ParsedCDR <- newEmptyMVar
       portedCDRSChan :: OrderedStream ParsedCDR1 <- newEmptyMVar
       toRateChan :: OrderedStream (ParsedCDR1, AppliedCostAndIncomeRate, [ExpandedExtension]) <- newEmptyMVar
       ratedChan  :: OrderedStream ParsedCDR1 <- newEmptyMVar
       extensionsToExpandChan :: OrderedStream ExpandedExtension <- newEmptyMVar
       cdrsWithErrorsChan :: OrderedStream ParsedCDR1 <- newEmptyMVar


       -- NOTE: before opening other DB connections, execute these long operation
       -- for avoiding deadlocks.
       db_deleteCDRS dbState (params_fromDate env)

       withResource''
         (do c1 <- db_openConnection (params_dbConf env) False
             c2 <- db_openConnection (params_dbConf env) False
             return (c1, c2))
         (\(rawCDRSConn, portedTelephoneNumbersConn) -> do

              groupedCDRSSuggestedSize <- groupedCDRS_suggestedSize rawCDRSConn env

              db_garbagePastErrorsR dbState (rateGarbageKey env) (Just $ params_fromDate env) maybeToCallDate

              -- NOTE: execute for first, for being sure to acquire DB connection ownership
              (loadToDBChan, writeToDBJobs) <-
                db_loadDataFromNamedPipe dbState (fromStringToByteString pipeName) "ar_cdr" cdr_mysqlCSVLoadCmd cdr_toMySQLCSV

              parsingJob <-
                process_parseSourceCDRS rawCDRSConn env maybeToCallDate parsedCDRSChan

              portedTelephoneNumberJob <-
                case (runLevel == RunLevelFakePortedTelephoneNunmbers || runLevel == RunLevelSkipDBWrite) of
                        True -> process_fakePortedTelephoneNumberCompletition portedTelephoneNumbersConn env parsedCDRSChan portedCDRSChan
                        False -> process_cdrsWithPortedTelephoneNumbers portedTelephoneNumbersConn env parsedCDRSChan portedCDRSChan

              -- This is the most expensive phase, so execute using many parallel jobs.
              ratingJobs <-
                process_orderedChunksUsingFun
                              "RatingCDRS"
                              portedCDRSChan
                              nrOfJobs
                              (process_calcRates env)
                              toRateChan
                              True  -- propagate close signal

              -- This must be done by a single process, because there is a shared BundleState depending from the order of CDRS.
              applyRatesJob <-
                process_applyRates env (params_lastSavedBundleState env, Nothing, Nothing, params_fromDate env, ISet.empty) toRateChan ratedChan extensionsToExpandChan

              expandExtensionsJob <-
                process_expandExtensions dbState env extensionsToExpandChan

              errorsJob <-
                process_errors env dbState ratedChan cdrsWithErrorsChan

              groupingJob <-
                case (runLevel == RunLevelSkipUpdateCachedGroupedCDRS || (not fastGrouping)) of
                  True ->
                    process_fakeCachedGroupedCDRS cdrsWithErrorsChan loadToDBChan
                  False ->
                    process_cachedGroupedCDRS env dbState groupedCDRSSuggestedSize (groupedErrors_suggestedSize) cdrsWithErrorsChan loadToDBChan

              _ <- wait parsingJob
              _ <- wait portedTelephoneNumberJob
              (_, maybeBundleStateToSave, maybeDailyBundleStateToSave, maxCallDate, conflictingUnitIds2) <- wait applyRatesJob
              V.mapM_ wait ratingJobs
              -- wait termination of all rates jobs, excluded these jobs needing to write the final result to the DB

              DB.close rawCDRSConn
              DB.close portedTelephoneNumbersConn
              -- close now the read-only db connection, because they are not needed anymore,
              -- and we avoid lock timeout in case other saving are taking too much time

              V.mapM_ wait writeToDBJobs
              _ <- wait expandExtensionsJob
              _ <- wait errorsJob
              totCDRS2 <- wait groupingJob
              db_saveBundleState dbState env (params_saveBundleStateImmediatelyAfter env) maybeBundleStateToSave maybeDailyBundleStateToSave

              -- Generate pure services, taking note of the day in which they are generated.

              (loadToDBChan', writeToDBJobs') <-
                db_loadDataFromNamedPipe dbState (fromStringToByteString pipeName) "ar_cdr" cdr_mysqlCSVLoadCmd cdr_toMySQLCSV

              (totCDRS1, daysToRegroup) <- do
                case service_generate env (params_fromDate env) maxCallDate of
                  Left errMsg
                    -> throwIO $
                         asterisellError_toException $
                           createError
                           Type_Critical
                           Domain_RATES
                           errMsg
                           ("Error in specification of Services. " ++ errMsg)
                           ("Corresponding ServiceCDRs will be not produced, and inserted in call report. The call report does not contain stats about these missing service CDRs.")
                           ("Correct the error in Service specification, and rerate them.")

                  Right pureServices
                    -> do !pureServices' <- V.fromList <$> serviceCDRS_rateIO env pureServices
                          orderedStream_put loadToDBChan' $ Just pureServices'
                          return $ ( V.length pureServices'
                                   , Set.fromList $ L.map (\c -> today $ cdr_calldate c) $ V.toList pureServices')
              orderedStream_put loadToDBChan' Nothing
              V.mapM_ wait writeToDBJobs'

              -- Update days where groupingJob was not complete
              when (runLevel /= RunLevelSkipUpdateCachedGroupedCDRS) $ do
                case fastGrouping of
                  True -> do
                    M.mapM_
                      (\d -> rateEngine_updateCachedGroupedCDRS dbState True d (Just $ tomorrow d))
                      (Set.insert (today $ params_fromDate env) daysToRegroup)
                    -- We added new CDRS to `ar_cdr` table, and these days had to be grouped again.
                    -- NOTE: usually they are few days, and they are generated only during complete rerating of the unbilled time-frame
                  False -> do
                    rateEngine_updateCachedGroupedCDRS dbState False (today $ params_fromDate env) Nothing

              rateEngine_signalConflictingUnitIds
                dbState
                env
                maxCallDate
                (ISet.union (params_conflictingUnitIds env) conflictingUnitIds2)

              db_closeRatingEvent env dbState maxCallDate

              return $ totCDRS1 + totCDRS2)

         (\(c1, c2) -> do DB.close c1; DB.close c2)
         -- DEV-NOTE: this is called only in case of exceptions,
         -- otherwise they are closed inside the code
         -- They are read-only connections.

         (id))

    (\isOk dbState -> do
       when (not isOk) $ do
         tryPutMVar (dbps_semaphore dbState) ()
         return ()
         -- NOTE: in case of interrupted processing, there can be no ownership in the DB,
         -- so force it
       db_releaseResourceR isOk dbState
    )
    (id)

-- --------------------------
-- Save data on DB

-- | Save bundle-state, deleting all other bundle-states.
db_saveBundleState
  :: DBState
  -> RatingParams
  -> LocalTime   -- ^ bundle-state date
  -> Maybe BundleState -- ^ bundle-state to save
  -> Maybe BundleState -- ^ daily bundle-state
  -> IO ()

db_saveBundleState dbState env toCallDate maybeBundleState maybeDailyBundleState = do
  _ <- takeMVar $ dbps_semaphore dbState
  case dbps_conn dbState of
    Left _ -> return ()
    Right conn -> do
      let qb = "DELETE FROM ar_bundle_state WHERE 1"
      _ <- DB.execute_ conn qb

      let q = "INSERT INTO ar_bundle_state (to_time, data_file) VALUES(?, ?)"

      case maybeDailyBundleState of
        Nothing -> return ()
        Just bs -> do
          _ <- DB.execute conn q [toDBLocalTime $ toBeginOfTheDay toCallDate, serializeBundleState bs]
          return ()

      case maybeBundleState of
        Nothing -> return ()
        Just bs -> do
          _ <- DB.execute conn q [toDBLocalTime toCallDate, serializeBundleState bs]
          return ()

      return ()
  putMVar (dbps_semaphore dbState) ()

 where

   serializeBundleState b = toDBText $ fromByteStringToText $ B64.encode $ LZ.compress $ DBS.encode b

-- | Close the rating process, writing the rating status to the DB.
db_closeRatingEvent :: RatingParams -> DBState -> CallDate -> IO ()
db_closeRatingEvent env state toCallDate = do
  _ <- takeMVar $ dbps_semaphore state
  let fromCallDate = params_fromDate env
  case dbps_conn state of
    Left _ -> return ()
    Right conn -> do
      !stmtId <- DB.prepareStmt conn "CALL add_daily_status_change_event(?)"
      let (allDays, _) = timeFrame_getYearsAndMonth (fromCallDate, toCallDate)
      let toCallDate' d = toDBLocalTime $ LocalTime { localDay = d, localTimeOfDay = midnight}
      M.mapM_ (\d -> DB.executeStmt conn stmtId [toCallDate' d]) (Set.toList allDays)

  putMVar (dbps_semaphore state) ()

-- ----------------------------------------
-- Expanded extensions process

process_expandExtensions
  :: DBState
  -> RatingParams
  -> OrderedStream ExpandedExtension
  -- ^ input chan
  -- @require when Nothing is received, this process became the owner of the database connection
  -> IO (Async ())

process_expandExtensions dbState env inChan = do
  let ees = HS.empty
  -- DEV-NOTE: there are not so much extensions, and so they can be stored in RAM, like other customer-related data

  async $ mainJob ees

 where

  mainJob :: HS.HashSet ExpandedExtension -> IO ()
  mainJob ees1 = do
    mes <- takeMVar inChan
    case mes of
      Just es -> do
         let !ees2 = V.foldl' (flip HS.insert) ees1 es
         mainJob ees2

      Nothing -> do
         _ <- takeMVar $ dbps_semaphore dbState
         case dbps_conn dbState of
           Left _ -> return ()
           Right conn -> do
             let q = [str|INSERT INTO ar_expanded_extensions
                         |SET ar_organization_unit_id = ?
                         |,   extension_code = ?
                         |]

             stmtId <- DB.prepareStmt conn q
             M.mapM_ (\ee -> do
                           _ <- DB.executeStmt conn stmtId [toDBInt64 $ ee_organizationId ee, toDBByteString $ ee_specificExtensionCode ee]
                           return ()
                     )  $ (HS.toList ees1)
             return ()
         putMVar (dbps_semaphore dbState) ()

-- ----------------------------------------
-- Error processing

-- | Transform a CDR to an error CDR.
cdr_toError :: CDR -> AsterisellError -> CDR
cdr_toError cdr0 err
  = let cdr1 = cdr0 { cdr_problemDuplicationKey = Just $ asterisellError_dupKey err }
        errorDirection
          = case cdr_direction cdr1 of
              CDR_error -> cdr_errorDirection cdr1
                           -- NOTE: if the direction is already an error, then the error direction is already set

              CDR_none -> CDR_outgoing
                          -- NOTE: signal the worst damage.
                          -- Usually it is CDR_none in case of CDRS with importing/parsing problems

              other -> other

    in cdr1 { cdr_errorDirection = errorDirection
            , cdr_direction = CDR_error
            , cdr_duration = case cdr_duration cdr1 of
                               Nothing -> Just 0
                               Just r -> Just r
            , cdr_billsec = case cdr_billsec cdr1 of
                              Nothing -> Just 0
                              Just r -> Just r
            }

-- | Delete all errors with the specified garbage collection key, so only new errors are inserted.
--   To call before inserting new errors.
db_garbagePastErrorsR
  :: DBState
  -> BS.ByteString
  -- ^ the garbage key
  -> Maybe LocalTime
  -- ^ delete errors from this date
  -> Maybe LocalTime
  -- ^ delete errors to this date
  -> IO ()

db_garbagePastErrorsR dbState garbageKey fromDate toDate = do
  case dbps_conn dbState of
    Left _ -> return ()
    Right conn ->  do
      _ <- initErrors conn garbageKey fromDate toDate
      return ()
 where

  initErrors :: MySQLConn -> BS.ByteString -> Maybe LocalTime -> Maybe LocalTime -> IO ()
  initErrors db garbageKey mfromDate mtoDate
    = do case (mfromDate, mtoDate) of
                (Just fromDate, Just toDate)
                  -> do _ <- DB.execute
                               db
                               "DELETE FROM ar_new_problem WHERE garbage_collection_key = ? AND garbage_collection_from >= ? AND garbage_collection_to <= ? ;"
                               [toDBByteString garbageKey, toDBLocalTime fromDate, toDBLocalTime toDate]
                        return ()
                (Just fromDate, Nothing)
                  -> do _ <- DB.execute
                               db
                               "DELETE FROM ar_new_problem WHERE garbage_collection_key = ? AND garbage_collection_from >= ?"
                               [toDBByteString garbageKey, toDBLocalTime fromDate]
                        return ()
                (Nothing, Just toDate)
                  -> do _ <- DB.execute
                               db
                               "DELETE FROM ar_new_problem WHERE garbage_collection_key = ? AND garbage_collection_to <= ? ;"
                               [toDBByteString garbageKey, toDBLocalTime toDate]
                        return ()
                (Nothing, Nothing)
                  -> do _ <- DB.execute
                               db
                               "DELETE FROM ar_new_problem WHERE garbage_collection_key = ?"
                               [toDBByteString garbageKey]
                        return ()

-- | Generate an error message for each UnitId with conflicting BundleRate assignations.
rateEngine_signalConflictingUnitIds :: DBState -> RatingParams -> CallDate -> ConflictingUnitIds -> IO ()
rateEngine_signalConflictingUnitIds dbState env toCallDate conflictingUnitIds = do
  _ <- takeMVar $ dbps_semaphore dbState

  case dbps_conn dbState of
    Left _ -> return ()
    Right conn -> do
      let dbName = dbps_name dbState
      stmtId <- dbErrors_prepareInsertStmt conn

      let !garbageTimeFrame = Just (params_fromDate env, toCallDate)

      M.mapM_
        (\unitId -> do
          let !organizationsInfo = params_organizations env
          let organizationName =
                case info_getDataInfoForUnitId organizationsInfo unitId (params_fromDate env) False of
                  Nothing -> "UNKNOWN NAME"
                  Just unitInfo ->
                    let dataInfoParents =
                          info_getDataInfoParentHierarchy organizationsInfo unitInfo (params_fromDate env) False

                    in  info_getFullName dataInfoParents 0 False False False

          let !err =
                createError
                  Type_Critical
                  Domain_VOIP_ACCOUNTS
                  ("bundle-rate conflict for unitId " ++ show unitId)
                  ("Organization unit with id " ++ show unitId ++ ", and name " ++ organizationName ++ " is assigned to more than one bundle-rate inside the same bundle-rate time-frame.")
                  ("Calls of this organization will not rated correctly.")
                  ("Assign the organization to the new price-category/bundle-rate only at beginning of next bundle-rate time-frame.")

          dbErrors_insert conn (Just stmtId) err garbageTimeFrame

        ) (ISet.toList conflictingUnitIds)

  putMVar (dbps_semaphore dbState) ()
  return ()

-- | Scan all CDRS and complete with errors information and stats if needed.
process_errors
  :: RatingParams
  -> DBState
  -> OrderedStream ParsedCDR1
  -- ^ input chan
  -- @require when Nothing is received, this process became the owner of the database connection
  -- @require if CDR_error then it is already called cdr_toError on it with all errors fields completed
  -> OrderedStream ParsedCDR1
  -- ^ out chan
  -- @ensure propagate EOF
  -> IO (Async ())

process_errors env dbState inChan outChan = do
  h <- createDBFile (db_errorFileName $ dbps_name dbState)
  hSetBuffering h (BlockBuffering Nothing)
  errorsRef <- newIORef asterisellErrorsDictionary_empty

  async $ mainJob h errorsRef
  -- NOTE: doing so it is sure to initialize the DB with the lock

 where

   mainJob :: Handle -> IORef AsterisellErrorsDictionary -> IO ()

   mainJob outH errorsR = do
     cdrs1 <- takeMVar inChan
     case cdrs1 of
       Just cdrs2 -> do
         cdrs3 <- M.mapM (processCDR outH errorsR) cdrs2
         orderedStream_put outChan $ Just cdrs3

         mainJob outH errorsR

       Nothing -> do

         hClose outH

         orderedStream_put outChan Nothing

         _ <- takeMVar $ dbps_semaphore dbState

         case dbps_conn dbState of
           Left _ -> return ()
           Right conn -> do
             let dbName = dbps_name dbState
             stmtId <- dbErrors_prepareInsertStmt conn

             errors <- readIORef errorsR
             c <- LBS.readFile (db_errorFileName dbName)
             for_ (S.decode CSV.NoHeader c) (\err -> do
               let garbageKey = asterisellError_garbageKey err
               let dupKey = asterisellError_dupKey err
               let garbageTimeFrame = fromJust1 "ERR 72225" $ Map.lookup dupKey errors
               dbErrors_insert conn (Just stmtId) err (Just garbageTimeFrame))

         putMVar (dbps_semaphore dbState) ()

         return ()

   processCDR :: Handle -> IORef AsterisellErrorsDictionary -> ParsedCDR1 -> IO ParsedCDR1

   processCDR outH errorsR (ParsedCDR1 (sourceCDR, Nothing, cdr)) = do
     case cdr_direction cdr == CDR_error of
       True -> do
         let err = createError
                     Type_Error
                     Domain_APPLICATION
                     "undefined CDR error 705"
                     ("A CDR can not be rated, but the corresponding error description was not generated.")
                     ("There are CDRS not rated, without a descripton of the reason.")
                     ("This is an error in the application code. Contact the assistance signaling the error code 12756.")
         return $ ParsedCDR1 (sourceCDR, Just err, cdr)

       False -> do
         return $ ParsedCDR1 (sourceCDR, Nothing, cdr)

   processCDR outH errorsR (ParsedCDR1(sourceCDR1, Just err1, cdr1)) = do

     -- Enrich the CDR with error info
     let !cdr2 = case cdr_direction cdr1 == CDR_error of
                   True -> cdr1
                   False -> cdr_toError cdr1 err1

     errs1 <- readIORef errorsR

     let !dupKey = DeepSeq.force $ asterisellError_dupKey err1
     let !garbageFrom = cdr_calldate cdr2
     let !garbageTo
               = case cdr_toCalldate cdr2 of
                   Nothing -> garbageFrom
                   Just r -> r

     let (!isNew, !needUpdate)
            = case Map.lookup dupKey errs1 of
                Nothing -> (True, False)
                Just (oldGarbageFrom, oldGarbageTo) -> (False, (garbageFrom /= oldGarbageFrom) || (garbageTo /= oldGarbageTo))

     let !err2
           = DeepSeq.force $
               case isNew of
                 False -> err1
                 True -> let
                   (providerName, callDate, formatId, sourceCDR) = sourceCDR1
                   cdrDetails1 = rate_showDebug env cdr2
                   cdrDetails2
                       = describeSourceCDRForErrorReporting
                           (params_fastLookupCDRImporters env)
                           (providerName, formatId, sourceCDR)

                   errDescription
                       = asterisellError_description err1 ++ cdrDetails2 ++ "\n\nCDR content:\n" ++ cdrDetails1

                   in err1 { asterisellError_description = errDescription }

     when (isNew || needUpdate) $ do
       modifyIORef' errorsR (\m -> Map.insert dupKey (garbageFrom, garbageTo) m)

     when isNew $ do
           LBS.hPut outH $ CSV.encode [err2]
           return ()

     return $ ParsedCDR1 (sourceCDR1, Just err1, cdr2)

-- --------------------------------------
-- CDR Parsing

-- | Read rawSourceCDRS from the DB, and parse them without rating.
process_parseSourceCDRS
  :: DB.MySQLConn
  -> RatingParams
  -> Maybe CallDate
  -> OrderedStream ParsedCDR
  -- ^ where sending the result
  --   @ensure propagate EOF signal
  -> IO (Async ())

process_parseSourceCDRS inConn env maybeToCallDate' outCDRS
  = do
        -- Prepare the query for extracting all rawCDRS
        -- DEV-NOTE: use a filter on calldate and id instead of LIMIT and OFFSET because
        -- in case of big data it is a lot faster starting from the correct calldate,
        -- respect applying an OFFSET.
        let inQ1 = [str| SELECT s.calldate, s.id, p.internal_name, s.ar_physical_format_id, s.content
                       | FROM ar_source_cdr AS s
                       | INNER JOIN ar_cdr_provider AS p
                       | ON s.ar_cdr_provider_id = p.id
                       | WHERE ((calldate = ? AND s.id > ?) OR calldate > ?)
                       |]

        let maybeToCallDate
              = case (maybeToCallDate', params_testToDate env) of
                  (Nothing, Nothing) -> Nothing
                  (Just d1, Nothing) -> Just d1
                  (Just d1, Just d2) -> Just $ min d1 d2
                  (Nothing, Just d2) -> Just d2

        let (inQ2, cond2) = case maybeToCallDate of
                              Nothing -> ("", [])
                              Just toCallDate -> (" AND calldate < ? ", [toDBLocalTime toCallDate])

        let inQ = LBS.concat [inQ1, inQ2, " ORDER BY s.calldate, s.id LIMIT ?"]
        stmtId <- DB.prepareStmt inConn (DB.Query inQ)

        async $ generateAllSplits stmtId cond2 (params_fromDate env, -1)

 where

   !precisionDigits = params_currencyPrecision env
   !fastLookupCDRImporters = params_fastLookupCDRImporters env

   -- | There is no streamed access to DB query results, but an entire result-set
   --   is materializated each time from MySQL driver. So I simulate stream-access
   --   retrieving a big chunk of CDRS every time.
   generateAllSplits
     :: StmtID
     -- ^ the statement to use for retrieving the CDRs
     -> [DB.MySQLValue]
     -- ^ condition on maybeToCallDate
     -> (CallDate -- ^ the current calldate to process
        , Int -- ^ the -- ^ the last `ar_source_cdr.id` processed
              --   -1 for the first call to this function
        )
     -> IO ()

   generateAllSplits stmtId maybeCond (currentCallDate, currentSourceId) = do
        when (currentSourceId == -1) $ do
          -- proper init of all jobs
          sendCDRSToIgnore currentCallDate

        (_, inS1) <- DB.queryStmt
                       inConn
                       stmtId
                       ([toDBLocalTime currentCallDate
                        , toDBInt64 currentSourceId
                        , toDBLocalTime currentCallDate
                        ] ++ maybeCond ++ [toDBInt64 param_chunkSize2])

        inS2 <- S.chunkVector param_chunkSize inS1

        let lastChunk' = param_chunkSize2 `div` param_chunkSize
        let lastChunk = if (lastChunk' * param_chunkSize) < param_chunkSize2
                        then (lastChunk' + 1)
                        else lastChunk'

        (lastCallDate, lastSourceId, maybeContinue) <- processSplit inS2 (lastChunk - 1) (currentCallDate, currentSourceId)
        case maybeContinue of
          False -> do
            sendCDRSToIgnore lastCallDate
            orderedStream_put outCDRS Nothing
            return ()
          True -> generateAllSplits stmtId maybeCond (lastCallDate, lastSourceId)

   processSplit :: S.InputStream (Chunk [DB.MySQLValue]) -> Int -> (CallDate, Int) -> IO (CallDate, Int, Bool)
   processSplit dbInChan lastChunk (lastCallDate, lastSourceId) = do
     mv <- S.read dbInChan
     case mv of
       Nothing
         -> do
               return (lastCallDate, lastSourceId, False)
               -- NOTE: do not continue because the end of the stream was reached before the end of the maximum expected
               -- size of result, so there are no any more results to process
       Just v
         -> do
               let v' = V.map processRecord v
               orderedStream_put outCDRS $ Just v'

               let (lastCallDate'', id'')
                     = if V.null v
                       then (lastCallDate, lastSourceId)
                       else let [lastCallDate', id', _, _, _] = V.last v
                            in  (fromDBLocalTime lastCallDate', fromDBInt id')

               case lastChunk == 0 of
                 True -> do
                   eofSignal <- S.read dbInChan
                   return $ pAssert "ERR 002" (isNothing eofSignal) (lastCallDate'', id'', True)
                 False -> processSplit dbInChan (lastChunk - 1) (lastCallDate'', id'')

   {-# INLINE processRecord #-}
   processRecord [!callDate', !id', !providerName', !formatId', !content']
    = let providerName = fromDBText providerName'
          callDate = fromDBLocalTime callDate'
          formatId = fromDBInt formatId'
          rawCDR = fromDBByteString content'
          maybeCdrs
            = case (parseRawSourceCDR precisionDigits fastLookupCDRImporters providerName formatId rawCDR) of
                           Left !err
                             -> Left err
                           Right !Nothing
                             -> Right []
                           Right (Just (_, Left !err))
                             -> Left err
                           Right (Just (_, Right !cdrs2))
                             -> Right $ pAssert "ERR 003" (L.all (\c -> cdr_calldate c == callDate) cdrs2) cdrs2

      in ParsedCDR ((providerName, callDate, formatId, rawCDR), maybeCdrs)

   -- | Send a CDR to ignore, so BundleState services can be fully generated until the specified date.
   sendCDRSToIgnore :: CallDate -> IO ()
   sendCDRSToIgnore callDate = do
        let cdr
              = (cdr_empty callDate precisionDigits) {
                   cdr_direction = CDR_ignored
                 , cdr_billsec = Just 0
                 , cdr_duration = Just 0 }
        orderedStream_put outCDRS $ Just $ V.singleton $ ParsedCDR (("", callDate, 0, ""), Right [cdr])

-- | Complete the info about ported telephone numbers.
process_cdrsWithPortedTelephoneNumbers
    :: DB.MySQLConn
    -> RatingParams
    -> OrderedStream ParsedCDR
    -- ^ input parsed CDRS
    -> OrderedStream ParsedCDR1
    -- ^ output CDRS with applied number-portability
    -- @ensure propagate EOF
    -> IO (Async ())

process_cdrsWithPortedTelephoneNumbers conn env inCDRS outCDRS
 = do
       -- Init the working tables

       _ <- DB.execute_ conn "DROP TABLE IF EXISTS ar_tmp_request_exported_tn"
       _ <- DB.execute_ conn [str|CREATE TEMPORARY TABLE ar_tmp_request_exported_tn(
                                 |  id INTEGER NOT NULL,
                                 |  telephone_number VARCHAR(255) NULL,
                                 |  from_date DATETIME NOT NULL,
                                 |  PRIMARY KEY (`id`)
                                 |) ENGINE=memory
                                 |]

       stmtTruncate <- DB.prepareStmt conn "TRUNCATE ar_tmp_request_exported_tn"
       let stmtInsertQ = "INSERT INTO ar_tmp_request_exported_tn(id, telephone_number, from_date) VALUES(?, ?,?)"
       stmtGetPortedTelephoneNumbers
         <- DB.prepareStmt
                       conn
                       [str| SELECT
                           |   t.id
                           | , SUBSTRING_INDEX(
                           |     GROUP_CONCAT(
                           |       p.ported_telephone_number
                           |       ORDER BY p.from_date DESC
                           |       SEPARATOR ',~~~'), ',~~~', 1)
                           | FROM ar_tmp_request_exported_tn AS t
                           | JOIN ar_number_portability AS p
                           | ON t.telephone_number = p.telephone_number
                           | WHERE p.from_date <= t.from_date
                           | AND t.telephone_number IS NOT NULL
                           | GROUP BY t.id, t.telephone_number
                           | ORDER BY t.id
                           |]

       async $ processChunks stmtTruncate stmtInsertQ stmtGetPortedTelephoneNumbers

 where


   processChunks stmtTruncate stmtInsertQ stmtGetPortedTelephoneNumbers = do
     maybeChunk <- takeMVar inCDRS
     case maybeChunk of
       Nothing
         -> do
                orderedStream_put outCDRS Nothing
       Just multiCDRS -> do
         let cdrs = toParsedCDR1 multiCDRS
         let valuesToPort
                = V.imap
                      (\i (ParsedCDR1 ((_, localTime, _, _), maybeErr, cdr))
                             -> case maybeErr of
                                 Just err
                                   -> Nothing
                                 Nothing
                                   -> case cdr_externalTelephoneNumberWithAppliedPortability cdr of
                                        Just _
                                          -> Nothing
                                             -- NOTE: nothing to port because the portability was already applied by the importer
                                        Nothing
                                          -> Just [toDBInt64 i, toDBText $ cdr_externalTelephoneNumber cdr, toDBLocalTime localTime]
                      ) cdrs

         _ <- DB.executeStmt conn stmtTruncate []
         let !valuesToPort' = V.toList $ V.map fromJust $ V.filter isJust valuesToPort
         case (L.null valuesToPort') of
           True -> return ()
           False -> do _ <- DB.executeMany conn stmtInsertQ valuesToPort'
                       return ()

         (_, portedRS) <- DB.queryStmt conn stmtGetPortedTelephoneNumbers []

         -- assign ported telephone numers
         portedNumbers1 :: V.Vector ParsedCDR1
           <- S.fold (\cdrs1 [dbId, dbN] ->
                              let i = fromDBInt dbId
                                  pn = fromDBText dbN
                                  ParsedCDR1 (sourceCDR, maybeErr, cdr1) = ((V.!) cdrs i)
                                  cdr2 = cdr1 { cdr_externalTelephoneNumberWithAppliedPortability = Just pn }
                              in (V.//) cdrs1 [(i, ParsedCDR1 (sourceCDR, maybeErr, cdr2))]
                     ) cdrs portedRS


         -- assign default telephone numbers for numbers that are not ported
         let !portedNumbers2
               = V.map (\unchanged@(ParsedCDR1 (sourceCDR, maybeErr, cdr))
                            -> case cdr_externalTelephoneNumberWithAppliedPortability cdr of
                                 Just e
                                   -> unchanged
                                 Nothing
                                   -> let cdr2 = cdr { cdr_externalTelephoneNumberWithAppliedPortability = Just $ cdr_externalTelephoneNumber cdr }
                                      in ParsedCDR1 (sourceCDR, maybeErr, cdr2)
                       ) portedNumbers1


         -- NOTE: previous result was forced deep-seq, so after closing the transaction, there should be no resource-leak
         orderedStream_put outCDRS $ (Just portedNumbers2)

         processChunks stmtTruncate stmtInsertQ stmtGetPortedTelephoneNumbers

-- | Apply the minimal amount of work for completing the external telephone number.
process_fakePortedTelephoneNumberCompletition
    :: DB.MySQLConn
    -> RatingParams
    -> OrderedStream ParsedCDR
    -- ^ input parsed CDRS
    -> OrderedStream ParsedCDR1
    -- ^ output CDRS with applied number-portability
    -> IO (Async ())

process_fakePortedTelephoneNumberCompletition conn env inCDRS outCDRS

  = async $ processChunks

 where

   processChunks = do
     maybeChunk <- takeMVar inCDRS
     case maybeChunk of
       Nothing
         -> do orderedStream_put outCDRS Nothing
       Just cdrs
         -> do let cdrs' = Just $ V.map noPorted $ toParsedCDR1 cdrs
               orderedStream_put outCDRS cdrs'

               processChunks

   noPorted :: ParsedCDR1 -> ParsedCDR1
   noPorted (ParsedCDR1 (sourceCDR, maybeErr, cdr))
     = let cdr2 = cdr { cdr_externalTelephoneNumberWithAppliedPortability = Just $ cdr_externalTelephoneNumber cdr}
       in  (ParsedCDR1 (sourceCDR, maybeErr, cdr2))

-- --------------------------------------------------------------
-- Efficient support for calculation of `ar_cached_grouped_cdrs`,
-- and `ar_cached_errors`

-- | Key values of `ar_cached_grouped_cdrs`, in order of appareance on the table.
type GroupedCDRSKey
  = ( BS.ByteString -- cached_parent_id_hierarchy
    , Int -- billable_ar_organization_unit_id
    , CallDate
    , Int -- destination_type
    , Int -- ar_communication_channel_type_id
    , Text.Text -- operator_type
    , Int -- ar_vendor_id
    , Text.Text -- geographic_location
    )

instance Hashable GroupedCDRSKey where
  hashWithSalt s (x1, x2, x3, x4, x5, x6, x7, x8) = hashWithSalt s ((x1, x2, x3, x4), (x5, x6, x7, x8))

-- | A special hierarchy value, grouping all the calls of all the customers.
groupedCDRSKey_toMatchAllHirearchy :: GroupedCDRSKey -> GroupedCDRSKey
groupedCDRSKey_toMatchAllHirearchy (_, _, x1, x2, x3, x4, x5, x6) = (BS.empty, 0, x1, x2, x3, x4, x5, x6)
{-# INLINE groupedCDRSKey_toMatchAllHirearchy #-}

-- | The value fields of `ar_cached_grouped_cdrs` in the order they appear in the table:
--   * count_of_calls
--   * billsec
--   * income
--   * cost_saving
--   * cost
--   * count_of_records
type GroupedCDRSValue = UV.Vector Int

-- | An IO Mutable HashMap.
--   DEV-NOTE: it has optimal performances for lookup, but it is slow if it had to be grown,
--   so it is better starting with a good size.
type GroupedCDRS = IMH.BasicHashTable GroupedCDRSKey GroupedCDRSValue

{-# INLINE groupedCDRS_add #-}
groupedCDRS_add :: GroupedCDRS -> GroupedCDRSKey -> GroupedCDRSValue -> IO ()
groupedCDRS_add groupedCDRS key producerValues = do
  _ <- IMH.mutate
         groupedCDRS
         key
         (\maybeV -> case maybeV of
                       Nothing -> (Just producerValues, ())
                       Just lastValues -> (Just $ UV.map (\(x,y) -> x + y) $ UV.zip producerValues lastValues, ()))
  return ()

-- | The suggested initial size of the groupedCDRS hash table.
groupedCDRS_suggestedSize :: DB.MySQLConn -> RatingParams -> IO Int
groupedCDRS_suggestedSize conn env = do
  let q1 = [str| SELECT COUNT(*)
               | FROM ar_cached_grouped_cdr
               | WHERE calldate >= ?
               | AND   calldate < ?
               |]

  -- a date where presumably there are similar stats, and that are correct
  let d = yesterday $ params_fromDate env
  (_, inS1) <- DB.query conn q1 [toDBLocalTime $ yesterday d, toDBLocalTime d]
  rs <- S.toList inS1
  case rs of
    []    -> return guessResult
    [[c]] -> case fromDBInt c of
               0 -> return guessResult
               c2 -> return c2

 where

   guessResult = (Trie.size $ info_extensions $ params_organizations env) * 3 * 10

groupedCDRS_mysqlCSVLoadCmd :: [BS.ByteString]
groupedCDRS_mysqlCSVLoadCmd
  =  [ "cached_parent_id_hierarchy",
       "billable_ar_organization_unit_id",
       "calldate",
       "destination_type",
       "ar_communication_channel_type_id",
       "operator_type",
       "ar_vendor_id",
       "geographic_location",
       "count_of_calls",
       "billsec",
       "income",
       "cost_saving",
       "cost",
       "count_of_records"]

groupedCDRS_toMySQLCSV :: (GroupedCDRSKey, GroupedCDRSValue) -> B.Builder
groupedCDRS_toMySQLCSV ((k1h, k1b, k2, k3, k4, k5, k6, k7), values) =
        toMySQLCSV_byteString k1h <> B.charUtf8 '\t' <>
        toMySQLCSV_int k1b <> B.charUtf8 '\t' <>
        toMySQLCSV_localTime k2 <> B.charUtf8 '\t' <>
        toMySQLCSV_int k3 <> B.charUtf8 '\t' <>
        toMySQLCSV_int k4 <> B.charUtf8 '\t' <>
        toMySQLCSV_text k5 <> B.charUtf8 '\t' <>
        toMySQLCSV_int k6 <> B.charUtf8 '\t' <>
        toMySQLCSV_text k7 <> B.charUtf8 '\t' <>
        toMySQLCSV_int (values UV.! 0) <> B.charUtf8 '\t' <>
        toMySQLCSV_int (values UV.! 1) <> B.charUtf8 '\t' <>
        toMySQLCSV_int (values UV.! 2) <> B.charUtf8 '\t' <>
        toMySQLCSV_int (values UV.! 3) <> B.charUtf8 '\t' <>
        toMySQLCSV_int (values UV.! 4) <> B.charUtf8 '\t' <>
        toMySQLCSV_int (values UV.! 5)

-- | Key values of `ar_cached_errors`, in order of appareance on the table.
type GroupedErrorsKey
  = ( CallDate
    , Int -- destination_type
    , Int -- error_destination_type
    )

-- | The `count_of_calls` field of `ar_cached_errors`.
type GroupedErrorsValue = Int

-- | An IO Mutable HashMap.
type GroupedErrors = IMH.BasicHashTable GroupedErrorsKey GroupedErrorsValue

-- | The suggested initial size of the groupedErrors hash table.
groupedErrors_suggestedSize :: Int
groupedErrors_suggestedSize = 32

{-# INLINE groupedErrors_add #-}
groupedErrors_add :: GroupedErrors -> GroupedErrorsKey -> GroupedErrorsValue -> IO ()
groupedErrors_add groupedErrors key producerValue = do
  _ <- IMH.mutate
         groupedErrors
         key
         (\maybeV -> case maybeV of
                       Nothing -> (Just producerValue, ())
                       Just lastValue -> (Just $ producerValue + lastValue, ()))
  return ()

groupedErrors_mysqlCSVLoadCmd :: [BS.ByteString]
groupedErrors_mysqlCSVLoadCmd
  =  [ "calldate"
     , "destination_type"
     , "error_destination_type"
     , "count_of_calls"
     ]

groupedErrors_toMySQLCSV :: (GroupedErrorsKey, GroupedErrorsValue) -> B.Builder
groupedErrors_toMySQLCSV ((k1, k2, k3), value) =
      toMySQLCSV_localTime k1 <> B.charUtf8 '\t' <>
        toMySQLCSV_int k2 <> B.charUtf8 '\t' <>
        toMySQLCSV_int k3 <> B.charUtf8 '\t' <>
        toMySQLCSV_int value

-- | Fake process.
process_fakeCachedGroupedCDRS
  :: OrderedStream ParsedCDR1
  -- ^ the input channel
  -> OrderedStream CDR
  -- ^ the output channel
  -- @ensure propagate EOF
  -> IO (Async CountOfCDRS) -- ^ the job for which waiting the termination

process_fakeCachedGroupedCDRS inChan outChan = async $ mainJob 0

 where

   mainJob countCDRS1 = do
     mv <- takeMVar inChan
     case mv of
       Nothing -> do
         orderedStream_put outChan Nothing
         return countCDRS1
       Just cdrs -> do
         orderedStream_put outChan $ (Just $ V.map parsedCDR1_toCDR cdrs)
         let !countCDRS2 = countCDRS1 + V.length cdrs

         mainJob countCDRS2

-- | Calculate daily sums of CDRS.
--   DEV-NOTE: `rateEngine_updadetCachedGroupedCDRS` is very slow in case of many CDRS, because
--   it scan the entire `ar_cdr` table. So the sums of CDRS are calculated during rating, when
--   they are already available, saving near 30% of rating-time.
--   @ensure collect daily sums for each organization
--   @ensure collect daily sums, summing all organizations toghether
--   @ensure collect daily sums of CDRS with errors
--   @ensure the calculated values except first and last day of rating time frame are the same of `rateEngine_updadetCachedGroupedCDRS`
process_cachedGroupedCDRS
  :: RatingParams
  -> DBState
  -> Int -- ^ CDRS suggested size. NOTE: calculated apart because it needs a lock on the DB
  -> Int -- ^ errors suggested size
  -> OrderedStream ParsedCDR1
  -- ^ the input channel
  -> OrderedStream CDR
  -- ^ the output channel
  -- @ensure propagate EOF
   -> IO (Async CountOfCDRS)
  -- ^ return the number of received CDRS


process_cachedGroupedCDRS env dbState cdrsSuggestedSize errorsSuggestedSize inChan outChan = do
  let dbName = dbps_name dbState
  cdrsOutH <- createDBFile (db_groupedCDRSFileName dbName)
  errorsOutH <- createDBFile (db_groupedErrorsFileName dbName)

  let writtenCallDate = yesterday (params_fromDate env)

  lastGroupedCDRS <- IMH.newSized cdrsSuggestedSize
  lastGroupedErrors <- IMH.newSized errorsSuggestedSize

  async $ mainJob dbName cdrsOutH errorsOutH writtenCallDate lastGroupedCDRS lastGroupedErrors 0

 where

   !precisionDigits = params_currencyPrecision env

   mainJob
     :: String -- ^ dbName
     -> Handle -- ^ where write grouped CDRS
     -> Handle -- ^ where write grouped errors
     -> CallDate -- ^ the last calldate written on the file
     -> GroupedCDRS -- ^ not already sent grouped CDRS
     -> GroupedErrors -- ^ not already sent grouped errors
     -> Int           -- ^ count processed CDRS
     -> IO CountOfCDRS

   mainJob dbName outCDRSH outErrorsH writtenCallDate lastGroupedCDRS lastGroupedErrors countCDRS = do
     mv <- takeMVar inChan
     case mv of
       Nothing -> do
         -- Save all data because this is the end of the work: all CDRS are generated
         addToGroupedFile dbName outCDRSH outErrorsH lastGroupedCDRS lastGroupedErrors
         hClose outCDRSH
         hClose outErrorsH
         orderedStream_put outChan Nothing

         _ <- takeMVar $ dbps_semaphore dbState
         case dbps_conn dbState of
           Left _ -> return ()
           Right writeConn -> do

             -- insert calculated data
             let q1 = "DELETE FROM ar_cached_grouped_cdr WHERE calldate >= ?"
             _ <- DB.execute writeConn q1 [toDBLocalTime $ toBeginOfTheDay $ params_fromDate env]

             let q2 = B.byteString "LOAD DATA INFILE ? INTO TABLE ar_cached_grouped_cdr CHARACTER SET utf8mb4 ("
                           <> mconcat (L.intersperse (B.byteString ",") $ L.map B.byteString groupedCDRS_mysqlCSVLoadCmd)
                           <> B.byteString ")"
             _ <- DB.execute writeConn (DB.Query $ B.toLazyByteString q2) [toDBByteString $ fromStringToByteString $ db_groupedCDRSFileName dbName]

             let q3 = "DELETE FROM ar_cached_errors WHERE calldate >= ?"
             _ <- DB.execute writeConn q3 [toDBLocalTime $ toBeginOfTheDay $ params_fromDate env]

             let q4 = B.byteString "LOAD DATA INFILE ? INTO TABLE ar_cached_errors CHARACTER SET utf8mb4 ("
                           <> mconcat (L.intersperse (B.byteString ",") $ L.map B.byteString groupedErrors_mysqlCSVLoadCmd)
                           <> B.byteString ")"
             _ <- DB.execute writeConn (DB.Query $ B.toLazyByteString q4) [toDBByteString $ fromStringToByteString $ db_groupedErrorsFileName dbName]
             return ()

         putMVar (dbps_semaphore dbState) ()

         return countCDRS

       Just cdrs -> do
         orderedStream_put outChan (Just $ V.map parsedCDR1_toCDR cdrs)

         (newWrittenCallDate, newGroupedCDRS, newGroupedErrors)
           <- M.foldM (processCDR dbName outCDRSH outErrorsH) (writtenCallDate, lastGroupedCDRS, lastGroupedErrors) cdrs

         let !countCDRS2 = countCDRS + V.length cdrs

         -- continue processing other cdrs
         mainJob dbName outCDRSH outErrorsH newWrittenCallDate newGroupedCDRS newGroupedErrors countCDRS2

   processCDR
     :: String -- ^ dbName
     -> Handle -- ^ where write grouped CDRS
     -> Handle -- ^ where write grouped errors
     -> (CallDate, GroupedCDRS, GroupedErrors)
     -> ParsedCDR1
     -> IO (CallDate, GroupedCDRS, GroupedErrors)

   processCDR dbName outCDRSH outErrorsH (writtenCallDate, lastGroupedCDRS, lastGroupedErrors) (ParsedCDR1 (parsedCDR, maybeError, ratedCDR)) = do
        let !callDate = cdr_calldate ratedCDR

        (!newWrittenCallDate, !newGroupedCDRS, !newGroupedErrors)
            <- case pAssert ("ERR 005: callDate " ++ show callDate ++ " < writtenCallDate " ++ show writtenCallDate) (callDate >= writtenCallDate) (toBeginOfTheDay callDate == toBeginOfTheDay writtenCallDate) of
                 True -> return (writtenCallDate, lastGroupedCDRS, lastGroupedErrors)
                 False -> do
                   addToGroupedFile dbName outCDRSH outErrorsH lastGroupedCDRS lastGroupedErrors

                   r1 <- IMH.newSized cdrsSuggestedSize
                   r2 <- IMH.newSized errorsSuggestedSize
                   return (toBeginOfTheDay callDate, r1, r2)

        when (cdr_direction ratedCDR /= CDR_ignored && cdr_direction ratedCDR /= CDR_error) $ do
          let tp = fromJust1 ("ERR 122: " ++ show ratedCDR) $
                     IMap.lookup
                       (fromJust1 ("ERR 121: " ++ show ratedCDR) $ cdr_telephonePrefixId ratedCDR)
                       (params_idToTelephonePrefix env)

          let key = ( cachedParentIdHierarchy_toText $ fromJust1 "ERR 6565" $ cdr_cachedParentIdHierarchy ratedCDR
                    , fromJust1 "ERR 63" $ cdr_billableOrganizationUnitId ratedCDR
                    , toBeginOfTheDay $ cdr_calldate ratedCDR
                    , pAssert "ERR 006" (cdr_direction ratedCDR /= CDR_error
                              && cdr_direction ratedCDR /= CDR_none
                              && cdr_errorDirection ratedCDR == CDR_none)
                             (cdrDirection_asterisellCode $ cdr_direction ratedCDR)
                    , fromJust1 "ERR 61" $ cdr_communicationChannelTypeId ratedCDR
                    , tp_operatorType tp
                    , fromJust1 "ERR 62" $ cdr_vendorId ratedCDR
                    , tp_geographicLocation tp
                    )

          let values = UV.fromList $ [cdr_countOfCalls ratedCDR
                                     ,fromJust1 "ERR 60" $ cdr_billsec ratedCDR
                                     ,toMonetaryValueWithFixedPrecisionInt precisionDigits $  cdr_income ratedCDR
                                     ,toMonetaryValueWithFixedPrecisionInt precisionDigits $  cdr_costSaving ratedCDR
                                     ,toMonetaryValueWithFixedPrecisionInt precisionDigits $  cdr_cost ratedCDR
                                     ,1
                                   ]
          groupedCDRS_add newGroupedCDRS key values
          groupedCDRS_add newGroupedCDRS (groupedCDRSKey_toMatchAllHirearchy key) values

        when (cdr_direction ratedCDR /= CDR_ignored) $ do
          groupedErrors_add
            newGroupedErrors
            ( toBeginOfTheDay $ cdr_calldate ratedCDR
            , cdrDirection_asterisellCode $ cdr_direction ratedCDR
            , cdrDirection_asterisellCode $ cdr_errorDirection ratedCDR)
            (cdr_countOfCalls ratedCDR)

        return (newWrittenCallDate, newGroupedCDRS, newGroupedErrors)

   addToGroupedFile
     :: String -- ^ dbName
     -> Handle -- ^ where write grouped CDRS
     -> Handle -- ^ where write grouped errors
     -> GroupedCDRS -- ^ not already sent grouped CDRS
     -> GroupedErrors -- ^ not already sent grouped errors
     -> IO ()

   addToGroupedFile dbName outCDRSH outErrorsH lastGroupedCDRS lastGroupedErrors = do
     IMH.mapM_ (\(k, v) -> B.hPutBuilder outCDRSH (groupedCDRS_toMySQLCSV (k, v) <> B.charUtf8 '\n')) lastGroupedCDRS
     IMH.mapM_ (\(k, v) -> B.hPutBuilder outErrorsH (groupedErrors_toMySQLCSV (k, v) <> B.charUtf8 '\n')) lastGroupedErrors

-- | Precalculate sums, in order to reduce the need to scan the entire table of CDRS for common queries.
--   The reduction in size is ~ 40x and so the more recent values of this table can be easily cached.
rateEngine_updateCachedGroupedCDRS :: DBState -> Bool -> CallDate -> Maybe CallDate -> IO ()
rateEngine_updateCachedGroupedCDRS dbState isReplace fromDate toDateM =
  let fromDate1 = DB.MySQLDate $ localDay fromDate
      toDate1 = case toDateM of
                  Nothing -> Nothing
                  Just toDate -> Just $ DB.MySQLDate $ localDay toDate

      fromDate2 = DB.MySQLDateTime $ toBeginOfTheDay fromDate
      toDate2 = case toDateM of
                  Nothing -> Nothing
                  Just toDate -> Just $ DB.MySQLDateTime $ toBeginOfTheDay toDate

      qm = case toDateM of
             Nothing -> ""
             Just _ -> " AND calldate < ? "

      replaceCmd = case isReplace of
                     True  -> "REPLACE INTO "
                     False -> "INSERT INTO "
  in do
      _ <- takeMVar $ dbps_semaphore dbState
      case dbps_conn dbState of
        Left _ -> return ()
        Right writeConn -> do
          let q1 = LBS.pack $ "DELETE FROM ar_cached_grouped_cdr WHERE calldate >= ? " ++ qm

          when (not isReplace) $ do
            _ <- DB.execute writeConn (DB.Query q1) ([fromDate1] ++ maybeToList toDate1)
            return ()

          -- NOTE: produce the records in the same order of the primaryKey schema
          let q2 isMatchAll
                 = replaceCmd
                     ++ [str| ar_cached_grouped_cdr(
                            |    cached_parent_id_hierarchy
                            |  , billable_ar_organization_unit_id
                            |  , `calldate`
                            |  , `destination_type`
                            |  , ar_communication_channel_type_id
                            |  , operator_type
                            |  , ar_vendor_id
                            |  , geographic_location
                            |  ,`count_of_calls`
                            |  ,`billsec`
                            |  ,`income`
                            |  ,`cost_saving`
                            |  ,`cost`
                            | , `count_of_records`)
                            | SELECT
                            |] ++ (if isMatchAll then " '', 0 " else " cached_parent_id_hierarchy, billable_ar_organization_unit_id ")
                     ++ [str| , ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)))
                            | , destination_type
                            | , ar_communication_channel_type_id
                            | , p.operator_type
                            | , ar_vendor_id
                            | , p.geographic_location
                            | , SUM(count_of_calls)
                            | , SUM(billsec)
                            | , SUM(income)
                            | , SUM(cost_saving)
                            | , SUM(cost)
                            | , COUNT(ar_cdr.id)
                            | FROM ar_cdr
                            | INNER JOIN ar_telephone_prefix AS p
                            | ON ar_cdr.ar_telephone_prefix_id = p.id
                            | WHERE calldate >= ?
                            |]
                     ++ qm
                     ++ [str| AND destination_type <> ?
                            | AND destination_type <> ?
                            | GROUP BY
                            |]
                     ++ "YEAR(calldate), DAYOFYEAR(calldate), "
                     ++ (if isMatchAll then " " else " cached_parent_id_hierarchy, billable_ar_organization_unit_id, ")
                     ++ [str|   destination_type
                            | , ar_communication_channel_type_id
                            | , p.operator_type
                            | , ar_vendor_id
                            | , p.geographic_location
                            | ;
                            |]
                     -- DEV-NOTE: the GROUP BY on calldate before cached_parent_id_hierarchy takes advantage
                     -- of the ar_cdr index on calldate.

          let q2p = [fromDate2] ++ maybeToList toDate2 ++ [toDBInt64 $ cdrDirection_asterisellCode CDR_ignored, toDBInt64 $ cdrDirection_asterisellCode CDR_error]

          _ <- DB.execute writeConn (DB.Query $ LBS.pack $ q2 False) q2p
          _ <- DB.execute writeConn (DB.Query $ LBS.pack $ q2 True) q2p

          let q3 = LBS.pack $ "DELETE FROM ar_cached_errors WHERE calldate >= ? " ++ qm

          when (not isReplace) $ do
            _ <- DB.execute writeConn (DB.Query q3) ([fromDate1] ++ maybeToList toDate1)
            return ()

          let q4 = replaceCmd
                     ++ [str| ar_cached_errors(
                            |    `calldate`
                            |  , `destination_type`
                            |  , `error_destination_type`
                            |  ,`count_of_calls`
                            |  )
                            | SELECT
                            |   ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)))
                            | , destination_type
                            | , error_destination_type
                            | , SUM(count_of_calls)
                            | FROM ar_cdr
                            | WHERE calldate >= ?
                            |]
                        ++ qm
                        ++ [str| AND destination_type <> ?
                               | GROUP BY YEAR(calldate), DAYOFYEAR(calldate)
                               | , destination_type
                               | , error_destination_type
                               | ;
                               |]

          _ <- DB.execute writeConn (DB.Query $ LBS.pack q4) ([fromDate2] ++ maybeToList toDate2 ++ [toDBInt64 $ cdrDirection_asterisellCode CDR_ignored])

          return ()

      putMVar (dbps_semaphore dbState) ()

-- | Update all `ar_cached_grouped_cdr` in the entire database.
rateEngine_openDBAndUpdateCachedGroupedCDRS :: DBConf -> IO ()
rateEngine_openDBAndUpdateCachedGroupedCDRS dbConf
  = do
       dbState <- db_init dbConf Nothing 4
       let writeConn = fromRight (error "ERR 522666") (dbps_conn dbState)
       db_openTransaction writeConn
       let q1 = "SELECT MIN(calldate), MAX(calldate) FROM ar_cdr"
       (_, inS1) <- DB.query_ writeConn  q1
       dates <- S.toList inS1
       case dates of
         [] -> do DB.execute_  writeConn "DELETE FROM ar_cached_grouped_cdr WHERE TRUE"
                  return ()
         [[d1,d2]] -> rateEngine_updateCachedGroupedCDRS dbState False (today $ fromDBLocalTime d1) Nothing
       db_commitTransactionR dbState
       DB.close writeConn
       return ()
-- ---------------------------------------------------
-- Unit Tests

tt_mainRateCalcTets
  = [ HUnit.TestCase $ HUnit.assertEqual "convert date" True (isJust $ fromMySQLDateTimeToLocalTime "2014-01-01 00:00:00")
    , HUnit.TestCase $ HUnit.assertEqual "convert number" True (isJust $ fromTextToRational "110")
    , testCost "no cost" cdr1 params1 "0"
    , testCost "simple 1" cdr1 (params2 { calcParams_costOnCall = Just $ RateCost_cost 0 }) "0.5"
    , testCost "simple 2" cdr1 (params2 { calcParams_costForMinute = Just 0 }) "100"
    , testCost "simple 3" cdr1 params2 "100.5"
    , testCost "max cost" cdr1 (params2 { calcParams_maxCostOfCall = Just $ fromTextToRational "50" }) "50"
    , testCost "max cost" cdr1 (params2 { calcParams_maxCostOfCall = Just $ fromTextToRational "50" }) "50"
    , testCost "max cost" cdr1 (params2 { calcParams_maxCostOfCall = Just $ fromTextToRational "50", calcParams_minCostOfCall = Just $ fromTextToRational "55" }) "55"
    , testCost "free seconds" cdr1 (params3 { calcParams_freeSecondsAfterCostOnCall = Just 15 }) "101.5"
    , testCost "discrete 1" cdr1 (params3 { calcParams_durationDiscreteIncrements = Just $ Just 4 }) "103.2"
    , testCost "discrete 2" cdr1 (params3 { calcParams_durationDiscreteIncrements = Just $ Just 0 }) "103"
    , HUnit.TestCase $ HUnit.assertEqual "mathRound" 1 (mathRound 0.5)
    , HUnit.TestCase $ HUnit.assertEqual "mathRound" 2 (mathRound 1.5)
    , HUnit.TestCase $ HUnit.assertEqual "mathRound" 1 (mathRound 0.6)
    , HUnit.TestCase $ HUnit.assertEqual "mathRound" 0 (mathRound 0.4)
    , testCost "round 1" cdr1 (params4 { calcParams_roundToDecimalDigits = Just $ Just 2 }) "100.13"
    , testCost "round 2" cdr1 (params4 { calcParams_roundToDecimalDigits = Just $ Just 1 }) "100.1"
    , testCost "round 3" cdr1 (params4 { calcParams_roundToDecimalDigits = Just $ Just 2 }) "100.13"
    , testCost "round 4" cdr1 (params4 { calcParams_roundToDecimalDigits = Just $ Just 0 }) "100"
    , testCost "round 5" cdr1 (params4 { calcParams_roundToDecimalDigits = Just $ Just 3 }) "100.125"
    ]

 where

  precisionDigits = 4

  cdr1 = (cdr_empty (fromJust1 "100" $ fromMySQLDateTimeToLocalTime "2014-01-01 00:00:00") precisionDigits) {
           cdr_duration = Just 30
         , cdr_billsec = Just 30
         }

  params1 = calcParams_defaultValues

  params2 = calcParams_defaultValues {
              calcParams_costForMinute = Just 1
            , calcParams_costOnCall = Just $ RateCost_cost 100
            }

  params3 = calcParams_defaultValues {
              calcParams_costForMinute = Just 6
            , calcParams_costOnCall = Just $ RateCost_cost 100
            }

  params4 = calcParams_defaultValues {
              calcParams_costForMinute = fromTextToRational "0.25"
            , calcParams_costOnCall = Just $ RateCost_cost 100
            }

  testCost testName cdr params expectedCost
    = let cost = calcParams_calc CostRate params cdr
      in  HUnit.TestCase $ HUnit.assertEqual testName (fromJust1 "1001" $ fromTextToRational expectedCost) cost
