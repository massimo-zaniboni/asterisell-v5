{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Rate CDRs.
--   This is mainly imperative code:
--   * it starts with the content of DB tables
--   * it updates them in an incremental way
--   * the code had to preserve the invariants of the tables
--   * in an ideal world, one had to prove that the code preserve them
--   * in practice I will list them, and I will review and check the code accordingly
--
--  NOTE: the difficulty it is that the properties to respect are simple, but the algo
--  works in an incremental way, so it had to preserve them in case of arbitrary workflows.
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
--  * C1) all new CDRS must be rated and inserted in the DB
--  * C2) CDRS to ignore can be not inserted
--  * C3) already billed CDRS can not be rated again
--  * C3) if a type of CDR (call, bundle-rate, pure-service) is deleted before rerating, it must be genarated in the same rating-phase, and viceversa
--  * G1) ar_cached_grouped_cdrs and ar_cached_errors must contains the grouped and cached sums of all values
--  * T1) tables must be compact and compressed
--  * F1) there can be more than one instance running at the same time, on the same host, so use unique file names
--  * F2) there can be only one rating process running at the same time for each instance
--  * M1) data structures shared by multiple threads had to be protected by MVar or similar things
--  * M2) there can be only one thread owning a DB Connection
--  * TODO continue...
--
--  The features the code had to have:
--  * F1) efficiennt incremental rating of new calls
--  * F2) efficient incremental rating of calls under bundle-rates (there are counters of left calls/minutes to maintain)
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
  RatingMonad,
  forceRight,
  rateEngine_updateCachedGroupedCDRS,
  rateEngine_openDBAndUpdateCachedGroupedCDRS,
  tt_rateSpecificationsTests,
  tt_mainRateCalcTets
) where

import Asterisell.DB
import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.MainRatePlan
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
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Int
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char (ord, chr)
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Test.HUnit as HUnit
import qualified Data.Csv as CSV
import qualified Data.Csv.Streaming as S
import System.IO as IO
import System.Directory as IO
import Debug.Trace
import Data.Maybe
import Data.Set as Set
import qualified Data.Map as Map
import Data.IntMap as IMap
import Data.IORef
import qualified Data.Serialize as DBS
import qualified Data.Serialize.Text as DBS
import qualified Data.ByteString.Base64 as B64
import Data.String
import Data.Traversable (mapAccumR)
import Data.Foldable as F

import qualified Codec.Compression.QuickLZ as LZ
import Control.Monad.Except as M
import Data.Text.Encoding
import qualified Data.Serialize as Serialize
import Data.Monoid
import qualified Data.Text.Read as T
import Data.Text.Encoding
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import Data.Hashable

import qualified Data.HashTable.IO as IMH

import System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S
import qualified Data.Vector as V
import Control.DeepSeq as DeepSeq

import GHC.Conc (getNumProcessors)
import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Control.Exception.Assert.Sugar
import Control.DeepSeq.Generics
import Control.Parallel
import Control.Parallel.Strategies

import qualified Control.Concurrent.Chan as UnboundedChan

import Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc
import qualified System.Posix.Files.ByteString as Posix
import qualified System.Posix.IO.ByteString as Posix
import qualified System.Posix.Process.ByteString as Posix
import qualified System.Posix.Types as Posix
import qualified System.Process as Process
import qualified System.Posix.User as Posix
import Foreign.StablePtr

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

-- -------------------------------------------------------------------
-- DB Actions

data DBState
       = DBState {
           dbps_conn :: DB.MySQLConn
           -- ^ @require: only one thread at a time is the owner of this connection
         , dbps_name :: String
         , dbps_expandedExtensions :: MVar (HS.HashSet ExpandedExtension)
         -- ^ the found expanded extensions that are not matched by an explicit expanded extension,
         --   but that are matched from a generic extension.
         , dbps_debugFile :: Maybe String
         -- ^ Nothing for writing to the DB, a debug file otherwise
         , dbps_currencyPrecision :: CurrencyPrecisionDigits
         }

dbps_writeData :: DBState -> Bool
dbps_writeData s = isNothing $ dbps_debugFile s

db_init :: DBConf -> Maybe String -> CurrencyPrecisionDigits -> IO (IORef DBState)
db_init dbConf debugFile currencyPrecision = do
  conn <- db_openConnection dbConf False
  ees <- newMVar (HS.empty)
  r <- newIORef (DBState {
                      dbps_conn = conn
                    , dbps_name = fromByteStringToString $ dbConf_dbName dbConf
                    , dbps_expandedExtensions = ees
                    , dbps_debugFile = debugFile
                    , dbps_currencyPrecision = currencyPrecision
                         })
  when (isNothing debugFile) $ db_openTransaction conn
  return r

-- | Save data and normal error messages.
--   Errors are saved only if the transaction is not aborted.
--   So critical errors (aborting the transaction), must be returned using `throwIO`
--   and normal errors will be discarded.
db_commitTransactionR :: IORef DBState -> IO ()
db_commitTransactionR stateR = do
  state <- readIORef stateR
  let conn = dbps_conn state
  let writeData = dbps_writeData state
  case writeData of
    False -> do return ()
    True -> do
               db_saveExpandedExtensionsIntoDB stateR
               db_commitTransaction conn

db_rollBackTransactionR :: IORef DBState -> IO ()
db_rollBackTransactionR stateR = do
  state <- readIORef stateR
  let conn = dbps_conn state
  let writeData = dbps_writeData state
  case writeData of
    False -> do return ()
    True -> db_rollBackTransaction conn

db_releaseResourceR :: Bool -> IORef DBState -> IO ()
db_releaseResourceR isOk dbStateR
 = do
      case isOk of
        True -> db_commitTransactionR dbStateR
        False -> db_rollBackTransactionR dbStateR

      state <- readIORef dbStateR
      DB.close (dbps_conn state)


-- | Delete old calculated CDRs before calculating new ones.
--   Delete also `ar_cached_grouped_cdrs` and `ar_cached_errors`, because they will be updated
--   from installed DB triggers.
db_deleteCDRS :: IORef DBState -> LocalTime -> IO ()

db_deleteCDRS stateR fromCallDate = do
  state <- readIORef stateR
  let conn = dbps_conn state
  let writeData = dbps_writeData state
  case writeData of
    False
      -> do return ()
    True
      -> do
            let d1 = toDBLocalTime fromCallDate

            let q1 = "DELETE FROM ar_cdr WHERE calldate >= ?"

            DB.execute conn (DB.Query q1) [d1]
            return ()

-- | Save bundle-state, deleting all other bundle-states.
db_saveBundleState
  :: IORef DBState
  -> RatingParams
  -> LocalTime   -- ^ bundle-state date
  -> BundleState -- ^ bundle-state to save
  -> BundleState -- ^ daily bundle-state
  -> IO ()

db_saveBundleState dbStateR env toCallDate bundleState dailyBundleState = do
  dbState <- readIORef dbStateR
  case dbps_writeData dbState of
    False
      -> do return ()
    True
      -> do
            let conn = dbps_conn dbState

            let qb = "DELETE FROM ar_bundle_state WHERE 1"
            _ <- DB.execute_ conn qb

            let q = "INSERT INTO ar_bundle_state (to_time, data_file) VALUES(?, ?)"
            _ <- DB.execute conn q [toDBLocalTime $ toBeginOfTheDay toCallDate, serializeBundleState dailyBundleState]
            _ <- DB.execute conn q [toDBLocalTime toCallDate, serializeBundleState bundleState]

            return ()

 where

   serializeBundleState b = toDBText $ fromByteStringToText $ B64.encode $ LZ.compress $ DBS.encode b


db_errorFileName :: String -> String
db_errorFileName dbName = "/var/tmp/errors__" ++ dbName ++ ".csv"

db_bundleFileName :: String -> String
db_bundleFileName dbName = "/var/tmp/bundles__" ++ dbName ++ ".csv"

db_groupedCDRSFilName :: String -> String
db_groupedCDRSFilName dbName = "/var/tmp/grouped_cdrs__" ++ dbName ++ ".csv"

db_groupedErrorsFilName :: String -> String
db_groupedErrorsFilName dbName = "/var/tmp/grouped_errors__" ++ dbName ++ ".csv"

-- | Create a DB file, accessible only from MySQL.
createDBFile :: String -> IO Handle
createDBFile fileName = do
  e <- IO.doesFileExist fileName
  when e (removeFile fileName)
  let mysqlCanRead = Posix.unionFileModes Posix.ownerModes Posix.groupReadMode
  fd <- Posix.createFile (fromStringToByteString fileName) mysqlCanRead
  mysqlUser <- Posix.groupID <$> Posix.getGroupEntryForName "mysql"
  Posix.setOwnerAndGroup (fromStringToByteString fileName) (0 - 1) mysqlUser
  -- NOTE: the file must be accessible from MySQL

  outH <- Posix.fdToHandle fd
  hSetBuffering outH (BlockBuffering Nothing)

  return outH

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

db_insertExpandedExtension :: IORef DBState -> ExpandedExtension -> IO ()
db_insertExpandedExtension dbStateR ee = do
  dbState <- readIORef dbStateR
  let mees = dbps_expandedExtensions dbState
  ees <- takeMVar mees
  putMVar mees $ HS.insert ee ees
  return ()

-- | Save expandend extensions.
-- NOTE: internal extensions are few, and so they can be store all in RAM, and processed as a list.
db_saveExpandedExtensionsIntoDB :: IORef DBState -> IO ()
db_saveExpandedExtensionsIntoDB dbStateR = do
  dbState <- readIORef dbStateR
  let conn = dbps_conn dbState
  extensions <- takeMVar (dbps_expandedExtensions dbState)
  case dbps_writeData dbState of
    False
      -> return ()
    True
      -> do stmtId <- prepareInsertStmt conn
            M.mapM_ (\ee -> do
                        _ <- DB.executeStmt conn stmtId [toDBInt64 $ ee_organizationId ee, toDBByteString $ ee_specificExtensionCode ee]
                        return ()
                    )  $ (HS.toList extensions)
  putMVar (dbps_expandedExtensions dbState) (HS.empty)

 where

  prepareInsertStmt conn
    = let q = [str|INSERT INTO ar_expanded_extensions
                 |SET ar_organization_unit_id = ?
                 |,   extension_code = ?
                 |]
    in DB.prepareStmt conn q

-- | Close the rating process, writing the rating status to the DB.
db_closeRatingEvent :: RatingParams -> IORef DBState -> CallDate -> IO ()
db_closeRatingEvent env stateR toCallDate = do
  let fromCallDate = params_fromDate env
  state <- readIORef stateR
  let conn = dbps_conn state
  let writeData = dbps_writeData state
  case writeData of
    False
      -> do return ()
    True
      -> do
            !stmtId <- DB.prepareStmt conn "CALL add_daily_status_change_event(?)"
            let (allDays, _) = timeFrame_getYearsAndMonth (fromCallDate, toCallDate)
            let toCallDate' d = toDBLocalTime $ LocalTime { localDay = d, localTimeOfDay = midnight}
            M.mapM_ (\d -> DB.executeStmt conn stmtId [toCallDate' d]) (Set.toList allDays)

-- ----------------------------------------
-- Error process

-- | Delete all errors with the specified garbage collection key, so only new errors are inserted.
--   To call before inserting new errors.
db_garbagePastErrorsR
  :: IORef DBState
  -> BS.ByteString
  -- ^ the garbage key
  -> Maybe LocalTime
  -- ^ delete errors from this date
  -> Maybe LocalTime
  -- ^ delete errors to this date
  -> IO ()

db_garbagePastErrorsR dbStateR garbageKey fromDate toDate = do
  dbState <- readIORef dbStateR
  case dbps_writeData dbState of
    False -> do return ()
    True ->  do _ <- initErrors (dbps_conn dbState) garbageKey fromDate toDate
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

process_errors
  :: RatingParams
  -> IORef DBState
  -> MVar (Maybe (AsterisellError, SourceCDR, CDR))
  -- ^ Nothing when the process had to terminate
  -- @require when Nothing is received, this process became the owner of the database connection
  -- DEV-NOTE: I use an MVar instead of a Chan, for being sure that there is an upper bound on the usage of RAM.
  -- Maybe this can slowdown performances if there are spikes of CDRs with errors, because there can be only
  -- one CDR queued, but usually this process is very fast in processing CDRS.
  -> IO ()

process_errors env dbStateR inChan = do
  dbState <-readIORef dbStateR
  h <- case dbps_writeData dbState of
         False
           -> return Nothing
         True
           -> do h' <- createDBFile (db_errorFileName $ dbps_name dbState)
                 hSetBuffering h' (BlockBuffering Nothing)
                 return $ Just h'

  mainJob h asterisellErrorsDictionary_empty

 where

   mainJob
     :: Maybe Handle -- ^ where writing errors
     -> AsterisellErrorsDictionary
     -> IO ()

   mainJob Nothing errs1 = do
     mv <- takeMVar inChan
     case mv of
       Just _ -> mainJob Nothing errs1
                 -- do nothing
       Nothing -> return ()

   mainJob (Just outErrorsH) errs1 = do
     mv <- takeMVar inChan
     case mv of
       Just (err, (providerName, callDate, formatId, sourceCDR), cdr) -> do
         -- Process data

         let garbageFrom = callDate
         let garbageTo
               = case cdr_toCalldate cdr of
                   Nothing -> callDate
                   Just r -> r

         let !dupKey = DeepSeq.force $ asterisellError_dupKey err
         let !isNew = not $ Map.member dupKey errs1
         let !errs2 = DeepSeq.force $
                        Map.insertWith
                           (\(from1, to1) (from2, to2) -> (min from1 from2, max to1 to2))
                           dupKey
                           (garbageFrom, garbageTo)
                           errs1

         when isNew (LBS.hPut outErrorsH $ CSV.encode [err])

         mainJob (Just outErrorsH) errs2

       Nothing -> do
         -- Save all data because this is the end of the work

         hClose outErrorsH

         dbState <-readIORef dbStateR
         let conn = dbps_conn dbState
         let dbName = dbps_name dbState
         stmtId <- dbErrors_prepareInsertStmt conn

         let errors = errs1

         c <- LBS.readFile (db_errorFileName dbName)
         for_ (S.decode CSV.NoHeader c) (\err -> do
           let garbageKey = asterisellError_garbageKey err
           let dupKey = asterisellError_dupKey err
           let garbageTimeFrame = fromJust1 "ERR 72225" $ Map.lookup dupKey errors
           dbErrors_insert conn (Just stmtId) err (Just garbageTimeFrame))

         return ()

-- -------------------------------------------------------------
-- Rating of CDRs
--

-- | A monad returning an error, or returning some result, inside IO.
--   The returned type is an implicit parameter of the type.
--   The error must always of type (CDR, AsterisellError).
type RatingMonad = ExceptT (CDR, AsterisellError) IO

-- | Generate a normal excetpion in case of error.
forceRight :: RatingMonad a -> IO a
forceRight m = do
  r <- runExceptT m
  case r of
    Left (_, err) -> throwIO $ asterisellError_toException err
    Right a -> return a
{-# INLINE forceRight #-}

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
--   See also comments on Cmd_deleteCDRS about the logic of (re)rating of calls.
--
-- NOTE: in case of succesfull rating, the pending rate-event is deleted from the `ManageRateEvent.php` caller
--   @require bundleStateFromCallDate <= fromCallDate
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
    (\dbStateR -> do
       parsedCDRS :: OrderedStream ParsedCDR <- newEmptyMVar
       portedCDRS :: OrderedStream ParsedCDR1 <- newEmptyMVar
       initializatedCDRS :: OrderedStream ParsedCDR1 <- newEmptyMVar
       inRating :: V.Vector (OrderedStream ParsedCDR1) <- V.replicateM nrOfJobs newEmptyMVar
       outRating :: V.Vector (OrderedStream CDR) <- V.replicateM nrOfJobs newEmptyMVar
       groupedCDRSChan :: UnboundedChan.Chan (Maybe (Int, CallDate, GroupedCDRS, GroupedErrors)) <- UnboundedChan.newChan
       errCDRS <- newEmptyMVar

       -- NOTE: before opening other DB connections, execute these long operati
       -- for avoiding deadlocks.
       db_deleteCDRS dbStateR (params_fromDate env)

       withResource''
         (do c1 <- db_openConnection (params_dbConf env) False
             c2 <- db_openConnection (params_dbConf env) False
             return (c1, c2))
         (\(rawCDRSConn, portedTelephoneNumbersConn) -> do

              db_garbagePastErrorsR dbStateR (rateGarbageKey env) (Just $ params_fromDate env) maybeToCallDate

              groupedCDRSSuggestedSize <- groupedCDRS_suggestedSize rawCDRSConn env

              maybeToDB <- do
                dbState <- readIORef dbStateR
                case dbps_debugFile dbState of
                  Nothing -> return $ Right $ dbps_conn dbState
                  Just n -> return $ Left n

              (ratedCDRS, writeToDBProcess) <- db_loadDataFromNamedPipe maybeToDB (fromStringToByteString pipeName) "ar_cdr" cdr_mysqlCSVLoadCmd cdr_toMySQLCSV

              -- Now start the normal CDR rating
              -- NOTE: initial bundle-rate services are generated because `ratePlan_loadRatingParams` use a `bundleState_fake`
              -- in `params_lastSavedBundleState` with 1 second before the generation of the real/new bundle-rate state.

              job1 <- process_parsedSourceCDRS nrOfJobs rawCDRSConn env maybeToCallDate parsedCDRS
              job2 <- case (runLevel == RunLevelFakePortedTelephoneNunmbers || runLevel == RunLevelSkipDBWrite) of
                        True -> process_fakePortedTelephoneNumberCompletition portedTelephoneNumbersConn env parsedCDRS portedCDRS
                        False -> process_cdrsWithPortedTelephoneNumbers portedTelephoneNumbersConn env parsedCDRS portedCDRS

              -- Associate CDRS to a customer and a vendor, without rating them.
              -- NOTE: this is a moderately expensive operation (some lookup), but
              -- * I can not execute it on previous phase, involving DB connection
              -- * I can not execute it in next rating phase, because I need the init info for deciding how to partition the CDRS
              jobs3 <- process_orderedChunksUsingFun "init CDRS ordered chunks" portedCDRS nrOfJobs (execute_initialClassification dbStateR env) initializatedCDRS True

              -- I rate CDRS in parallel, but sharding on root unid-id, so I'm sure to not put in conflict
              -- the distinct bundle-state calculations, because every bundle-state has a unique chain of billable organizations.

              statsR :: V.Vector (IORef (CountOfCDRS, CallDate, Set.Set CallDate, BundleState, Maybe BundleState, Maybe BundleState))
                <- V.mapM
                            (\(i, bundleStateSplit) -> newIORef (0, params_fromDate env, Set.empty, bundleStateSplit, Nothing, Nothing))
                            (V.zip listOfRatingJobs (V.fromList $ bundleState_partition env nrOfJobs (params_lastSavedBundleState env)))

              jobErrs <- async $ process_errors env dbStateR errCDRS

              jobs4 :: V.Vector (Async ())
                <- V.mapM
                     (\(i, statR) -> do
                         async $ process_rateCDRS
                                   dbStateR
                                   env
                                   (nrOfJobs, i)
                                   fastGrouping
                                   statR
                                   groupedCDRSSuggestedSize
                                   ((V.!) inRating i)
                                   ((V.!) outRating i)
                                   groupedCDRSChan
                                   errCDRS)
                     (V.zip listOfRatingJobs statsR)

              jobs5 <- process_orderedChunks "rate CDRS ordered chunks" initializatedCDRS inRating outRating partitionByRootId ratedCDRS False

              job6 <- case fastGrouping && runLevel /= RunLevelSkipUpdateCachedGroupedCDRS of
                        True -> process_cachedGroupedCDRS env dbStateR nrOfJobs groupedCDRSChan
                        False -> process_nullCachedGroupedCDRS groupedCDRSChan

              _ <- waitAll (V.toList $ V.concat [V.singleton job1, V.singleton job2, jobs3, jobs4, jobs5]) (job6:(V.toList writeToDBProcess))

              DB.close rawCDRSConn
              DB.close portedTelephoneNumbersConn
              -- DEV-NOTE: close now the db connection, because they are not any more needed,
              -- and we avoid lock timeout

              -- Join partial results of rating jobs, completing the partial results that have not received all data, due
              -- to mismatches in the partitions.

              (totCDRS1, maxCallDate, serviceDays, bundleStatesToSave, dailyBundleStatesToSave)
                <- M.foldM (\(totCDRS1, maxCallDate1, serviceDays1, bundleStatesToSave, dailyBundleStatesToSave) (statR, thisSplit)
                               -> do

                                     (totCDRS2, lastCallDate2, serviceDays2, bundleStateToClose2, bundleStateToSave2, dailyBundleStateToSave2) <- readIORef statR

                                     bundleStateToSave3
                                       <- case bundleStateToSave2 of
                                            Just h -> return h
                                            Nothing -> bundleState_updateToSomeCallDate
                                                         env
                                                         (params_incomeRate env)
                                                         (nrOfJobs, thisSplit)
                                                         bundleStateToClose2
                                                         (params_saveBundleStateImmediatelyAfter env)
                                                         -- DEV-NOTE: align the last state to the requested date

                                     dailyBundleStateToSave3
                                       <- case dailyBundleStateToSave2 of
                                            Just h -> return h
                                            Nothing -> bundleState_updateToSomeCallDate
                                                         env
                                                         (params_incomeRate env)
                                                         (nrOfJobs, thisSplit)
                                                         bundleStateToClose2
                                                         (toBeginOfTheDay $ params_saveBundleStateImmediatelyAfter env)
                                                       -- DEV-NOTE: align the last state to the requested date

                                     return ( totCDRS2 + totCDRS1
                                            , max maxCallDate1 lastCallDate2
                                            , Set.union serviceDays1 serviceDays2
                                            , bundleStateToSave3:bundleStatesToSave
                                            , dailyBundleStateToSave3:dailyBundleStatesToSave)

                               ) (0, params_fromDate env, Set.empty, [], []) (V.zip statsR listOfRatingJobs)

              let bundleStateToSave = bundleState_join bundleStatesToSave
              let dailyBundleStateToSave = bundleState_join dailyBundleStatesToSave

              -- Generate pure services, taking note of the day in which they are generated.
              (totCDRS2, daysToRegroup) <- do
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
                           -> do !pureServices' <- forceRight $ rateServiceCDRS dbStateR env (V.fromList pureServices)
                                 putMVar ratedCDRS $ DeepSeq.force $ Just pureServices'
                                 return $ (totCDRS1 + V.length pureServices'
                                          , Set.fromList $ L.map (\c -> today $ cdr_calldate c) $ V.toList pureServices')

              -- Send the EOF signal writing all the `ar_cdrs` pending in the `LOAD INFILE` connection
              putMVar ratedCDRS Nothing
              waitAll (V.toList writeToDBProcess) [job6, jobErrs]

              -- Save bundle state
              db_saveBundleState dbStateR env (params_saveBundleStateImmediatelyAfter env) bundleStateToSave dailyBundleStateToSave

              -- Send the EOF signal, writing `ar_cached_grouped_cdrs`
              UnboundedChan.writeChan groupedCDRSChan Nothing
              waitAll [job6] [jobErrs]

              dbState <- readIORef dbStateR
              let writeConn = dbps_conn dbState
              when (runLevel /= RunLevelSkipUpdateCachedGroupedCDRS) $ do
                case fastGrouping of
                  True -> do
                    M.mapM_ (\d -> rateEngine_updateCachedGroupedCDRS writeConn True d (Just $ tomorrow d)) (Set.union daysToRegroup serviceDays)
                    -- We added new CDRS to `ar_cdr` table, and these days had to be grouped again.
                    -- NOTE: usually they are few days, and they are generated only during complete rerating of the unbilled time-frame
                  False -> do
                     rateEngine_updateCachedGroupedCDRS writeConn False (today $ params_fromDate env) Nothing

              -- Save errors
              -- NOTE: save after closing of other jobs, because it has to be the owner of the DB connection
              putMVar errCDRS Nothing
              waitAll [jobErrs] []

              db_closeRatingEvent env dbStateR maxCallDate

              return totCDRS2)

         (\(c1, c2) -> do DB.close c1; DB.close c2)
         -- DEV-NOTE: this is called only in case of exceptions,
         -- otherwise they are closed inside the code

         (id))

    (db_releaseResourceR)
    (id)

 where

   partitionByRootId thisSplit (ParsedCDR1 (_, _, cdr))
     = case cdr_cachedParentIdHierarchy cdr of
         Nothing -> thisSplit == 0
                    -- NOTE: cdrs with parsing errors are managed only from the first job
         Just [] -> thisSplit == 0
         Just (r:_) -> bundlePartition_contains (nrOfJobs, thisSplit) r

-- | Read rawSourceCDRS from the DB, and parse them without rating.
--   @ensure send CDRS-to-ignore to all jobs at the beginning and at the end of the stream,
--   so all jobs will receive the first and last calldate to rate.
process_parsedSourceCDRS
  :: Int
  -> DB.MySQLConn
  -> RatingParams
  -> Maybe CallDate
  -> OrderedStream ParsedCDR
  -- ^ where seding the result
  -> IO (Async ())
process_parsedSourceCDRS nrOfJobs inConn env maybeToCallDate' outCDRS
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
        -- DEV-NOTE: CDR parsing is a lot more faster than other operations,
        -- so use only one thread for executing it.

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
          sendCDRSToIgnoreToAllJobs nrOfJobs currentCallDate

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
            sendCDRSToIgnoreToAllJobs nrOfJobs lastCallDate
            putMVar outCDRS Nothing
            return ()
          True -> generateAllSplits stmtId maybeCond (lastCallDate, lastSourceId)

   processSplit :: S.InputStream (Chunk [DB.MySQLValue]) -> Int -> (CallDate, Int) -> IO (CallDate, Int, Bool)
   processSplit dbInChan lastChunk (lastCallDate, lastSourceId) = do
     mv <- S.read dbInChan
     case mv of
       Nothing
         -> return (lastCallDate, lastSourceId, False)
            -- NOTE: do not continue because the end of the stream was reached before the end of the maximum expected
            -- size of result, so there are no any more results to process
       Just v
         -> do
               let v' = V.map processRecord v
               putMVar outCDRS $ Just v'

               let (lastCallDate'', id'')
                     = if V.null v
                       then (lastCallDate, lastSourceId)
                       else let [lastCallDate', id', _, _, _] = V.last v
                            in  (fromDBLocalTime lastCallDate', fromDBInt id')

               case lastChunk == 0 of
                 True -> do
                   eofSignal <- S.read dbInChan
                   return $ assert (isNothing eofSignal) (lastCallDate'', id'', True)
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
                             -> Right $ assert (L.all (\c -> cdr_calldate c == callDate) cdrs2) $ cdrs2

      in ParsedCDR ((providerName, callDate, formatId, rawCDR), maybeCdrs)

   -- | Send to all jobs a CDR to ignore, so they can process bundle-state and services properly.
   sendCDRSToIgnoreToAllJobs :: Int -> CallDate -> IO ()
   sendCDRSToIgnoreToAllJobs 0 _ = return ()
   sendCDRSToIgnoreToAllJobs leftJobs callDate = do
        let cdr
              = (cdr_empty callDate precisionDigits) {
                   cdr_direction = CDR_ignored
                 , cdr_billsec = Just 0
                 , cdr_duration = Just 0 }
        putMVar outCDRS $ Just $ V.singleton $ ParsedCDR (("", callDate, 0, ""), Right [cdr])
        sendCDRSToIgnoreToAllJobs (leftJobs - 1) callDate

-- | Complete the info about ported telephone numbers.
process_cdrsWithPortedTelephoneNumbers
    :: DB.MySQLConn
    -> RatingParams
    -> OrderedStream ParsedCDR
    -- ^ input parsed CDRS
    -> OrderedStream ParsedCDR1
    -- ^ output CDRS with applied number-portability
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
                putMVar outCDRS Nothing
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
         case (L.null valuesToPort) of
           True -> return ()
           False -> do _ <- DB.executeMany conn stmtInsertQ (V.toList $ V.map fromJust $ V.filter isJust valuesToPort)
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
               = DeepSeq.force $
                   V.map (\unchanged@(ParsedCDR1 (sourceCDR, maybeErr, cdr))
                            -> case cdr_externalTelephoneNumberWithAppliedPortability cdr of
                                 Just _
                                   -> unchanged
                                 Nothing
                                   -> let cdr2 = cdr { cdr_externalTelephoneNumberWithAppliedPortability = Just $ cdr_externalTelephoneNumber cdr }
                                      in ParsedCDR1 (sourceCDR, maybeErr, cdr2)
                         ) portedNumbers1


         -- NOTE: previous result was forced deep-seq, so after closing the transaction, there should be no resource-leak
         putMVar outCDRS $ DeepSeq.force $ Just $ portedNumbers2

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

process_fakePortedTelephoneNumberCompletition conn env inCDRS outCDRS = async processChunks

 where

   processChunks = do
     maybeChunk <- takeMVar inCDRS
     case maybeChunk of
       Nothing
         -> do putMVar outCDRS Nothing
       Just cdrs
         -> do let cdrs' = DeepSeq.force $ Just $ V.map noPorted $ toParsedCDR1 cdrs
               putMVar outCDRS cdrs'
               processChunks

   noPorted :: ParsedCDR1 -> ParsedCDR1
   noPorted (ParsedCDR1 (sourceCDR, maybeErr, cdr))
     = let cdr2 = cdr { cdr_externalTelephoneNumberWithAppliedPortability = Just $ cdr_externalTelephoneNumber cdr}
       in  (ParsedCDR1 (sourceCDR, maybeErr, cdr2))

-- | Rate CDRS.
--   Raise exceptions only in case of critical errors compromizing the entire rating process.
--   Normal rating errors are signaled generating a CDR with an error.
--   @require it will receive also as a CDR-to-ignore, the last calldate to process, so all bundle-rate services are generated.
--   Otherwise if some customer has no calls at the end of the time-frame, some service CDRS can be not generated.
--   @ensure for each inpuct Chunk (except Nothing), only one out/result Chunk is written, in accordance with OrderedStream API.
--   @ensure for Nothing input Chunk only two out/result Chunks are generated: final data, and Nothing
process_rateCDRS
    :: IORef DBState
    -- ^ to use for sending error messages
    -> RatingParams
    -> BundlePartition
    -> Bool -- ^ True for calculating fast grouping of CDRS
    -> IORef ( CountOfCDRS
             , CallDate
             -- ^ the last/previous rating call-date
             , Set.Set CallDate
             -- ^ days to recalculate, because there are bundle/service CDRS.
             -- Usually there are very few days of this type inside a rating-time frame,
             -- so they can be counted apart simplifying the code.
             , BundleState
             -- ^ the current bundle-state
             , Maybe BundleState
             -- ^ the bundle-state to save at the end of the rating process
             -- NOTE: it is saved a bundle-state not immediately at the end of rating process,
             -- but few minutes before, so it can be reused at next rating process
             , Maybe BundleState
             -- ^ the bundle-state to save at the beginnig of the last day of rating process.
             --   It is like previous bundle-state, but instead of 30m before the ending, it is at begin of the day.
             --   Having this bundle-state make more efficient incremental rerating of resellers (they receive daily status update from providers),
             --   or from arbitrarly time-frames.
             )
    -> Int
    -- ^ suggested initial size of the GroupedCRSValue, because Hash resize is expensive
    -> OrderedStream ParsedCDR1
    -- ^ the CDR (already initializated) to rate.
    -> OrderedStream CDR
    -- ^ where sending the rated CDRS
    -> UnboundedChan.Chan (Maybe (Int          -- ^ the producer id
                               , CallDate      -- ^ daily data
                               , GroupedCDRS   -- ^ rated CDRS
                               , GroupedErrors -- ^ CDRS with errrors
                               )
                          )
       -- ^ where sending GroupedCDRS
    -> MVar (Maybe (AsterisellError, SourceCDR, CDR))
       -- ^ where sending errors
    -> IO ()

process_rateCDRS dbStateR env (totSplits, thisSplit) fastGrouping statsR groupedCDRSInitialSize inCDRS outCDRS outGroupedCDRS errCDRS = do

  groupedCDRS <- IMH.newSized groupedCDRSInitialSize
  groupedErrors <- IMH.newSized groupedErrors_suggestedSize

  mainProcess groupedCDRS groupedErrors

 where

  !fromCallDate = params_fromDate env
  !cdrImporters = params_fastLookupCDRImporters env
  !isOfficialRatingEvent = params_isRateUnbilledCallsEvent env
  !precisionDigits = params_currencyPrecision env

  mainProcess groupedCDRS groupedErrors = do

    let processName = "process_rateCDRS"
    maybeCdrs1 <- takeMVar inCDRS
    case maybeCdrs1 of
      Nothing -> do
        (_, lastCallDate1, _, _, _, _) <- readIORef statsR
        when (fastGrouping) $ do
          UnboundedChan.writeChan outGroupedCDRS $ Just (thisSplit, toBeginOfTheDay lastCallDate1, groupedCDRS, groupedErrors)

        putMVar outCDRS Nothing

      Just cdrs1 -> do
        (_, lastCallDate1, _, _, _, _) <- readIORef statsR

        -- MAYBE use more efficient data structures
        ratedCDRSAndCDRSToIgnore :: V.Vector (Maybe CDR, Chunk ServiceCDR) <- V.mapM (rateCDR groupedCDRS groupedErrors) cdrs1
        let ratedCDRS :: Chunk CDR = V.map fromJust $ V.filter isJust $ V.map fst ratedCDRSAndCDRSToIgnore
        let serviceCDRS :: Chunk ServiceCDR = V.concat $ V.toList $ V.map snd ratedCDRSAndCDRSToIgnore
        let !allCDRS = (V.++) ratedCDRS serviceCDRS
        putMVar outCDRS (DeepSeq.force $ Just allCDRS)

        (_, lastCallDate2, _, _, _, _) <- readIORef statsR
        (!newGroupedCDRS, !newGroupedErrors)
            <- case ((not fastGrouping) || toBeginOfTheDay lastCallDate1 == toBeginOfTheDay lastCallDate2) of
                 True -> return (groupedCDRS, groupedErrors)
                 False -> do
                   UnboundedChan.writeChan outGroupedCDRS $ Just (thisSplit, toBeginOfTheDay lastCallDate1, groupedCDRS, groupedErrors)
                   -- NOTE: it is ok sending also values of lastCallDate2, they will be correctly summed
                   r1 <- IMH.newSized groupedCDRSInitialSize
                   r2 <- IMH.newSized groupedErrors_suggestedSize
                   return (r1, r2)

        mainProcess newGroupedCDRS newGroupedErrors

  -- | Rate CDRS.
  --   Raise exceptions only in case of critical errors compromising the entire rating process.
  --   Normal rating errors are signaled generating a CDR with an error.
  --   Return Nothing if the CDR can be safely ignored.
  --   @ensure update GroupedCDRS with correctly rated CDRS
  --   @ensure update GroupedErrors with CDRS with rating errors
  --   @ensure in GroupedCDRS group only by organization and call-date,
  --   while the total grouping of all organizations will be done from the `process_cachedGroupedCDRS`
  rateCDR :: GroupedCDRS -> GroupedErrors -> ParsedCDR1 -> IO (Maybe CDR, Chunk ServiceCDR)
  rateCDR groupedCDRS groupedErrors (ParsedCDR1(sourceCDR@(providerName, callDate, formatId, rawCDR), maybeErr, cdr1)) = do
    -- send groupedCDRS
    let (!cdr2, !isNormalCDR)
          = if cdr_isServiceCDR cdr1
            then (cdr1 { cdr_direction = CDR_system}, False)
                 -- NOTE: by definition a service CDR imported from a ar_source_cdr is a imported service CDR,
                 -- and it has a direction CDR_system by default.
             else (cdr1, True)

    serviceCDRS <- updateBundleStateAndStats callDate isNormalCDR

    let isCDRToIgnore = (cdr_direction cdr2 == CDR_ignored)

    case maybeErr of
      Just err
        -> do

              errCDR' <- processError groupedErrors sourceCDR (Just cdr1) err
              return (Just errCDR', serviceCDRS)
      Nothing
        -> case isCDRToIgnore of
             True
               -> do
                      return (Nothing, serviceCDRS)
             False
               -> do
                     mr <- runExceptT (mainRate_apply env statsR cdr2)
                     case mr of
                       Right ratedCDR
                         -> do
                               case (cdr_direction ratedCDR /= CDR_ignored) of
                                 True -> do
                                   -- update groupedCDRS
                                   let tp = fromJust1 "ERR 122" $ IMap.lookup (fromJust1 "ERR 121" $ cdr_telephonePrefixId ratedCDR) (params_idToTelephonePrefix env)
                                   let key = ( cachedParentIdHierarchy_toText $ fromJust1 "ERR 6565" $ cdr_cachedParentIdHierarchy ratedCDR
                                             , fromJust1 "ERR 63" $ cdr_billableOrganizationUnitId ratedCDR
                                             , toBeginOfTheDay $ cdr_calldate ratedCDR
                                             , cdrDirection_asterisellCode $ cdr_direction ratedCDR
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
                                                              ]
                                   when fastGrouping $ do
                                     groupedCDRS_add groupedCDRS key values

                                     groupedErrors_add
                                       groupedErrors
                                       (toBeginOfTheDay $ cdr_calldate ratedCDR
                                       , cdrDirection_asterisellCode $ cdr_direction ratedCDR
                                       , cdrDirection_asterisellCode $ cdr_errorDirection ratedCDR)
                                       (cdr_countOfCalls ratedCDR)

                                   return (Just ratedCDR, serviceCDRS)
                                 False -> return (Nothing, serviceCDRS)
                       Left (errCDR, err)
                         -> do
                               errCDR' <- processError groupedErrors sourceCDR (Just errCDR) err
                               return (Just errCDR', serviceCDRS)

  -- | Update the rating status, and rate the corresponding bundle rates service CDRS.
  --   ThrowIO a critical error if the service CDR can not be initializated correctly.
  updateBundleStateAndStats :: CallDate -> Bool -> IO (Chunk ServiceCDR)
  updateBundleStateAndStats callDate isNormalCDR = do
    (countCDRS1, lastCallDate1, _, bundleState1, savedBundleState1, savedDailyBundleState1) <- readIORef statsR

    let lastCallDate2 = assert (callDate >= lastCallDate1) callDate

    serviceCDRS
      <- case bundleState_areThereServiceCdrs bundleState1 callDate of
           False -> return V.empty
           True -> do let ratePlan = params_incomeRate env
                      r <- bundleState_updateAndGetServiceCdrs env ratePlan (totSplits, thisSplit) statsR callDate
                      forceRight $ rateServiceCDRS dbStateR env r

    maybeSavedBundleState2
      <- case savedBundleState1 of
           Just s -> return $ Just s
           Nothing -> case callDate >= (params_saveBundleStateImmediatelyAfter env) of
                        True -> Just <$> bundleState_updateToSomeCallDate env (params_incomeRate env) (totSplits, thisSplit) bundleState1 (params_saveBundleStateImmediatelyAfter env)
                                -- DEV-NOTE: align the previous state, to the state exctaly at `params_saveBundleStateImmediatelyAfter`

                        False -> return Nothing

    maybeSavedDailyBundleState2
      <- case savedDailyBundleState1 of
           Just s -> return $ Just s
           Nothing -> case callDate >= (toBeginOfTheDay $ params_saveBundleStateImmediatelyAfter env) of
                        True -> Just <$> bundleState_updateToSomeCallDate env (params_incomeRate env) (totSplits, thisSplit) bundleState1 (toBeginOfTheDay $ params_saveBundleStateImmediatelyAfter env)
                                -- DEV-NOTE: align the previous state, to the state exctaly at desidered time-frame

                        False -> return Nothing

    modifyIORef' statsR (\(countCDRS2, _, serviceDays, newBundleState, _, _) -> ( countCDRS2 + 1, lastCallDate2, serviceDays, newBundleState, maybeSavedBundleState2, maybeSavedDailyBundleState2))

    return serviceCDRS

  -- | Complete error information and send to the database.
  processError :: GroupedErrors -> SourceCDR -> Maybe CDR -> AsterisellError -> IO CDR
  processError groupedErrors (providerName, callDate, formatId, sourceCDR) maybeCDR err = do

    let cdrDetails
          = describeSourceCDRForErrorReporting
              (params_fastLookupCDRImporters env)
              (providerName, formatId, sourceCDR)

    let errDescription
          = asterisellError_description err ++ cdrDetails

    let err2
          = err { asterisellError_garbageKey = rateGarbageKey env
                , asterisellError_description = errDescription
                , asterisellError_effect = "This CDR will be not rated. " ++ asterisellError_effect err
                , asterisellError_proposedSolution = asterisellError_proposedSolution err ++ ". If there are many errors of this type, there can be a problem in the input format specification or in the application code. In case contact the assistance. "
                }

    let toCallDate
          = case maybeCDR of
              Nothing -> callDate
              Just cdr -> case cdr_toCalldate cdr of
                            Nothing -> callDate
                            Just r -> r

    let !errorCDR
          = case maybeCDR of
              Nothing  -> cdr_toError ((cdr_empty callDate (params_currencyPrecision env)) { cdr_direction = CDR_outgoing}) err2
                          -- I use CDR_outgoing for being pessimistic, and generating the worst possible error.
              Just cdr -> cdr_toError cdr err2

    when fastGrouping $ do
      groupedErrors_add
        groupedErrors
        (toBeginOfTheDay $ cdr_calldate errorCDR
        , cdrDirection_asterisellCode $ cdr_direction errorCDR
        , cdrDirection_asterisellCode $ cdr_errorDirection errorCDR)
        (cdr_countOfCalls errorCDR)

    putMVar errCDRS $ DeepSeq.force (Just (err2, (providerName, callDate, formatId, sourceCDR), errorCDR))
    -- send the error to the process

    return errorCDR

-- | Send the specified service-cdrs, but before apply a rating normalization process.
--   The services CDRS are proportional to the number of customers, so they can be stored and processed in RAM.
rateServiceCDRS
  :: IORef DBState
  -> RatingParams
  -> Chunk ServiceCDR
  -> RatingMonad (Chunk ServiceCDR)

rateServiceCDRS dbStateR env services0 = do
  V.mapM (cdr_initialClassification dbStateR env Nothing) services0
{-# INLINE rateServiceCDRS #-}

-- --------------------------------------------------------------
-- Efficient support for calculation of `ar_cached_grouped_cdrs`, and `ar_cached_errors`

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

  -- a date where presumebly there are similar stats, and that are correct
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
       "cost"]

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
        toMySQLCSV_int (values UV.! 4)

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

-- | Do nothing, simply read the channel.
process_nullCachedGroupedCDRS
  :: UnboundedChan.Chan (Maybe (Int -- ^ the producer id
                               , CallDate
                               , GroupedCDRS -- ^ the producer daily data
                               , GroupedErrors -- ^ the producer daily data
                               )
                        ) -- ^ Nothing when there are no any more data
  -> IO (Async ()) -- ^ the job for which waiting the termination

process_nullCachedGroupedCDRS inChan = async mainJob
 where

  mainJob = do
    mv <- UnboundedChan.readChan inChan
    case mv of
      Nothing -> return ()
      Just _ -> mainJob

-- | Receive partitioned sums of ar_cdrs from rating jobs, and update `ar_cached_grouped_cdrs`
--   with the grand-totals.
--   DEV-NOTE: `rateEngine_updadetCachedGroupedCDRS` is very slow in case of many CDRS, because
--   it scan the entire `ar_cdr` table. So the sums of CDRS are calculated during rating, when
--   they are already available, saving near 30% of rating-time.
--   @ensure collect daily sums for each organization
--   @ensure collect daily sums, summing all organizations toghether
--   @ensure collect daily sums of CDRS with errors
--   @ensure the calculated values except first and last day of rating time frame are the same of `rateEngine_updadetCachedGroupedCDRS`
process_cachedGroupedCDRS
  :: RatingParams
  -> IORef DBState
  -> Int -- ^ number of producer jobs
  -> UnboundedChan.Chan (Maybe (Int -- ^ the producer id
                               , CallDate
                               , GroupedCDRS -- ^ the producer daily data
                               , GroupedErrors -- ^ the producer daily data
                               )
                        ) -- ^ Nothing when there are no any more data
     -- ^ can use an unbounded chan (without back-pressure)
     --   because the traffic is very limited (daily sums), and the consumer is a lot faster
  -> IO (Async ()) -- ^ the job for which waiting the termination

process_cachedGroupedCDRS env dbStateR nrOfJobs inChan = do
  dbState <- readIORef dbStateR
  let dbName = dbps_name dbState
  cdrsOutH <- createDBFile (db_groupedCDRSFilName dbName)
  errorsOutH <- createDBFile (db_groupedErrorsFilName dbName)

  let writtenCallDate = yesterday (params_fromDate env)
  producersCallDate <- MV.replicate nrOfJobs writtenCallDate
  lastGroupedCDRS <- IMH.new
  lastGroupedErrors <- IMH.newSized groupedErrors_suggestedSize

  async $ mainJob dbName cdrsOutH errorsOutH producersCallDate writtenCallDate lastGroupedCDRS lastGroupedErrors

 where

   -- | Group toghether the daily GroupedCDRS.
   --
   --   DEV-NOTE: this is called for each day, for each producer, so very few,
   --   but the datastructure had to be efficient, because they are used also on the producer side
   --   and they are updated for every CDR.
   mainJob
     :: String -- ^ dbName
     -> Handle -- ^ where write grouped CDRS
     -> Handle -- ^ where write grouped errors
     -> MV.IOVector CallDate -- ^ for each producer, the greater received calldate
     -> CallDate -- ^ the last calldate written on the file
     -> GroupedCDRS -- ^ not already sent grouped CDRS
     -> GroupedErrors -- ^ not already sent grouped errors
     -> IO ()

   mainJob dbName outCDRSH outErrorsH producersCallDate writtenCallDate lastGroupedCDRS lastGroupedErrors = do
     mv <- UnboundedChan.readChan inChan
     case mv of
       Nothing -> do

         --
         -- Save all data because this is the end of the work: all CDRS are generated
         --

         -- save pending sums
         IMH.mapM_ (\(k, v) -> B.hPutBuilder outCDRSH (groupedCDRS_toMySQLCSV (k, v) <> B.charUtf8 '\n')) lastGroupedCDRS
         hClose outCDRSH

         IMH.mapM_ (\(k, v) -> B.hPutBuilder outErrorsH (groupedErrors_toMySQLCSV (k, v) <> B.charUtf8 '\n')) lastGroupedErrors
         hClose outErrorsH

         dbState <- readIORef dbStateR
         let writeConn = dbps_conn dbState

         -- insert calculated data
         let q1 = "DELETE FROM ar_cached_grouped_cdr WHERE calldate >= ?"
         _ <- DB.execute writeConn q1 [toDBLocalTime $ toBeginOfTheDay $ params_fromDate env]

         let q2 = B.byteString "LOAD DATA INFILE ? INTO TABLE ar_cached_grouped_cdr CHARACTER SET utf8mb4 ("
                           <> mconcat (L.intersperse (B.byteString ",") $ L.map B.byteString groupedCDRS_mysqlCSVLoadCmd)
                           <> B.byteString ")"
         _ <- DB.execute writeConn (DB.Query $ B.toLazyByteString q2) [toDBByteString $ fromStringToByteString $ db_groupedCDRSFilName dbName]

         let q3 = "DELETE FROM ar_cached_errors WHERE calldate >= ?"
         _ <- DB.execute writeConn q3 [toDBLocalTime $ toBeginOfTheDay $ params_fromDate env]

         let q4 = B.byteString "LOAD DATA INFILE ? INTO TABLE ar_cached_errors CHARACTER SET utf8mb4 ("
                           <> mconcat (L.intersperse (B.byteString ",") $ L.map B.byteString groupedErrors_mysqlCSVLoadCmd)
                           <> B.byteString ")"
         _ <- DB.execute writeConn (DB.Query $ B.toLazyByteString q4) [toDBByteString $ fromStringToByteString $ db_groupedErrorsFilName dbName]

         -- calculate in a precise way the starting date, because the online sums can not start from the beginning of the day
         (when $ let d = params_fromDate env in d /= today d)
           (rateEngine_updateCachedGroupedCDRS writeConn True (today $ params_fromDate env) (Just $ tomorrow $ params_fromDate env))

         return ()

       Just (producerId, producerCallDate, producerCDRS, producerErrors) -> do

         --
         -- Process a received group of totals
         --

         MV.write producersCallDate producerId (assert (producerCallDate > writtenCallDate) producerCallDate)

         -- update global stats, with producer stats
         IMH.mapM_
             (\(producerKey, producerValues) -> do
                 groupedCDRS_add lastGroupedCDRS producerKey producerValues
                 groupedCDRS_add lastGroupedCDRS (groupedCDRSKey_toMatchAllHirearchy producerKey) producerValues
             ) producerCDRS

         IMH.mapM_
             (\(producerKey, producerValues) -> groupedErrors_add lastGroupedErrors producerKey producerValues)
             producerErrors

         let s = MV.length producersCallDate
         minProducerCallDate <- M.foldM (\d1 i2 -> do
                                              d2 <- MV.read producersCallDate i2
                                              return $ min d1 d2
                                          ) producerCallDate [0 .. (s - 1)]
         newWrittenCallDate
           <- case minProducerCallDate > writtenCallDate of
                 False -> return writtenCallDate
                 True -> do
                    -- Write lastGroupedCDRS to file
                    dataToWrite1 <- IMH.foldM (\r (k@(_, _, callDate, _, _, _, _, _), v)
                                                   -> case callDate <= minProducerCallDate of
                                                        True -> return $ (k, v):r
                                                        False -> return r
                                              ) [] lastGroupedCDRS

                    M.mapM_ (\(k, v) -> B.hPutBuilder outCDRSH (groupedCDRS_toMySQLCSV (k, v) <> B.charUtf8 '\n')) dataToWrite1

                    M.mapM_ (\(k, v) -> IMH.delete lastGroupedCDRS k) dataToWrite1

                    -- Write lastGroupedErrors to file
                    dataToWrite2 <- IMH.foldM (\r (k@(callDate, _, _), v)
                                                   -> case callDate <= minProducerCallDate of
                                                        True -> return $ (k, v):r
                                                        False -> return r
                                              ) [] lastGroupedErrors

                    M.mapM_ (\(k, v) -> B.hPutBuilder outErrorsH (groupedErrors_toMySQLCSV (k, v) <> B.charUtf8 '\n')) dataToWrite2

                    M.mapM_ (\(k, v) -> IMH.delete lastGroupedErrors k) dataToWrite2

                    return minProducerCallDate

         mainJob dbName outCDRSH outErrorsH producersCallDate newWrittenCallDate lastGroupedCDRS lastGroupedErrors

-- | Precalculate sums, in order to reduce the need to scan the entire table of CDRS for common queries.
--   The reduction in size is ~ 40x and so the more recent values of this table can be easily cached.
rateEngine_updateCachedGroupedCDRS :: DB.MySQLConn -> Bool -> CallDate -> Maybe CallDate -> IO ()
rateEngine_updateCachedGroupedCDRS writeConn isReplace fromDate toDateM
  = let fromDate1 = DB.MySQLDate $ localDay fromDate
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
                            |  ,`cost`)
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

-- | Update all `ar_cached_grouped_cdr` in the entire database.
rateEngine_openDBAndUpdateCachedGroupedCDRS :: DBConf -> IO ()
rateEngine_openDBAndUpdateCachedGroupedCDRS dbConf
  = do
       writeConn <- db_openConnection dbConf False
       db_openTransaction writeConn
       let q1 = "SELECT MIN(calldate), MAX(calldate) FROM ar_cdr"
       (_, inS1) <- DB.query_ writeConn  q1
       dates <- S.toList inS1
       case dates of
         [] -> do DB.execute_  writeConn "DELETE FROM ar_cached_grouped_cdr WHERE TRUE"
                  return ()
         [[d1,d2]] -> rateEngine_updateCachedGroupedCDRS writeConn False (today $ fromDBLocalTime d1) Nothing
       db_commitTransaction writeConn
       DB.close writeConn
       return ()

-- | Init the CDRS and the errors.
--   The errors are not sent to the DBMS, but only returned.
execute_initialClassification :: IORef DBState -> RatingParams -> ParsedCDR1 -> IO ParsedCDR1
execute_initialClassification dbStateR env (ParsedCDR1 (sourceCDR@(_, callDateToVerify, _, _), maybeError, cdr1)) = do
  case maybeError of
        Just err
          -> return $ ParsedCDR1 (sourceCDR, maybeError, cdr1)
        Nothing
          -> do maybeInitCDR <- runExceptT (cdr_initialClassification dbStateR env (Just callDateToVerify) cdr1)
                case maybeInitCDR of
                  Left (errorCDR, err)
                    -> return $ ParsedCDR1 (sourceCDR, Just err, errorCDR)
                  Right cdr2
                    -> return $ ParsedCDR1 (sourceCDR, Nothing, cdr2)

-- | Apply initial classification to the CDR, completing the fields that are not completed from
--   the initial custom CDR importer, or complete fields of ServiceCDR.
cdr_initialClassification :: IORef DBState -> RatingParams -> Maybe CallDate -> CDR -> RatingMonad CDR
cdr_initialClassification dbStateR env maybeCallDateToVerify cdr1
  =
      case (cdr_direction cdr1 == CDR_error || cdr_direction cdr1 == CDR_ignored) of
        True
          -> return cdr1
        False
          -> case runStateT pass1 cdr1 of
               Left err
                 -> throwError err
               Right (isExtensionalMatch, cdr2)
                 -> case cdr_direction cdr2 == CDR_ignored of
                      True -> return cdr2
                      False ->
                       case execStateT pass2 cdr2 of
                         Left err -> throwError err
                         Right cdr4
                           -> do when (cdr_direction cdr1 == CDR_error)
                                      (throwError
                                      ( cdr4
                                      , createError
                                          Type_Critical
                                          Domain_APPLICATION
                                          "unexpected error code 1075375"
                                          "Unexpected error in the code. Contact the assistance signaling ERR CODE 17253"
                                          "An erorr CDR was generated as normal CDR, instead of being associated to an error message."
                                          "Application contains an error in the code, and for some CDRS can not inform the user about the reason they can not be rated."))

                                 when (not (cdr_calldate cdr1 == callDateToVerify))
                                      (let err = (createError
                                                     Type_Error
                                                     Domain_RATES
                                                     ("changed rating classification code")
                                                     ("This CDR was first imported, using the call date " ++ fromLocalTimeToMySQLDateTime callDateToVerify ++ ", but now during rating phase, the parsed calldate is " ++ fromLocalTimeToMySQLDateTime (cdr_calldate cdr1) ++ ". Probably the code interpreting the CDR format was changed in the past, and now during rerating there is some mismatch.")
                                                     ("All CDRs of this type will be not rated, because the database content is potentially corrupted.")
                                                     ("Extract calls at the specified calldate (the first one of the list), and of the same provider/type/version, and resubmit them. They will be deleted from the database, and resubmitted with the correct call date (the second one of the list)."))

                                        in throwError (cdr4 { cdr_calldate = min callDateToVerify (cdr_calldate cdr4)}, err))

                                 when isExtensionalMatch
                                      (let ee = ExpandedExtension {
                                                   ee_organizationId = fromJust1 "p557" $ cdr_organizationUnitId cdr4
                                                 , ee_specificExtensionCode = fromTextToMySQLResult $ cdr_internalTelephoneNumber cdr4
                                                                   }
                                       in liftIO $ db_insertExpandedExtension dbStateR ee)

                                 return cdr4

 where

  callDateToVerify
    = case maybeCallDateToVerify of
        Nothing -> cdr_calldate cdr1
        Just c -> c

  !info = params_organizations env

  pass1
    = do  let channelDomains = params_channelDomains env
          let organizationsInfo = params_organizations env

          let cdrTime = cdr_calldate cdr1

          modify (\cdr -> cdr { cdr_precision = (params_currencyPrecision env) })

          -- complete billsec and duration, if only one of the two fields is specified.
          cdr1 <- get
          when (isNothing $ cdr_billsec cdr1)
               (put $ cdr1 { cdr_billsec = cdr_duration cdr1 })

          when (isNothing $ cdr_duration cdr1)
               (put $ cdr1 { cdr_duration = cdr_billsec cdr1 })

          cdr1 <- get
          when (isNothing $ cdr_billsec cdr1)
               (throwError $ createRateError cdr1
                                             ("CDR not consistent - " ++ cdr_uniqueErrorKey cdr1)
                                             ("This CDR has no duration or billsec field specified.")
                                             ("The CDR will not be rated.")
                                             ("If there are many CDRs with this problem, contact the assistance.")
               )

          -- Use a standard method for retrieving the default Vendor

          (maybeVendorId, maybeChannelTypeId)
                <- case (cdr_channel cdr1) of
                    Nothing
                      -> return (Nothing, Nothing)
                    Just channelName
                      -> case channelDomains_match channelDomains (fromTextToByteString channelName) cdrTime of
                           [(r1, r2)] -> return (Just r1, Just r2)
                           [] -> throwError $ createRateError cdr1 ("unknown channel name - " ++ Text.unpack channelName)
                                                              ("Channel \"" ++ Text.unpack channelName ++ "\" is not defined in the Channel Domain table.")
                                                              ("All CDRs using the same channel domain will be not rated.")
                                                              ("Configure better the Channel Domain Table. Check also the validity time frame for the domains.")

                           _ ->  throwError $ createRateError cdr1 ("too much channels - " ++ Text.unpack channelName)
                                                              ("There is more than one valid entry in the Channel Domain Table, for the Channel Domain \"" ++ Text.unpack channelName ++ "\".")
                                                              ("All CDRs using the same channel domain will be not rated.")
                                                              ("Configure better the Channel Domain Table. Check also the validity time frame for the domains.")

          when (isNothing $ cdr_vendorId cdr1)
               (modify $ \c -> c { cdr_vendorId = maybeVendorId })

          when (isNothing $ cdr_communicationChannelTypeId cdr1)
               (modify $ \c -> c { cdr_communicationChannelTypeId = maybeChannelTypeId })

          cdr1 <- get
          when   (isNothing $ cdr_vendorId cdr1)
                 (throwError $ createRateError cdr1 ("CDRs without vendors ")
                                                    ("This CDR has no vendor.")
                                                    ("CDRs of the same type will be not rated.")
                                                    ("This is probably an error in the code. Contact the assistance.")
                 )

          when (isNothing $ cdr_communicationChannelTypeId cdr1)
               (throwError $ createRateError cdr1 ("CDRs without channel type")
                                                   ("This CDR has no communication channel type.")
                                                   ("CDRs of the same type will be not rated.")
                                                   ("This is probably an error in the code. Contact the assistance.")
               )

          -- Complete info about the internal extensions/organization associated to the call.

          (unitId, unitInfo, isExtensionalMatch)
                <- case cdr_organizationUnitId cdr1 of
                     Just i
                       -> return $ (i, fromJust1 ("error 1: there is no info for unitId " ++ show i ++ ", at date " ++ showLocalTime cdrTime) $ info_getDataInfoForUnitId organizationsInfo i cdrTime False, True)

                     Nothing
                       -> do let accountCode = cdr_internalTelephoneNumber cdr1
                                 accountCodeS = Text.unpack $ accountCode

                             when (Text.length accountCode == 0)
                                  (throwError $ createRateError cdr1 ("unknown ArAsteriskAccount NULL")
                                                                     ("Asterisk VoIP code can not be empty.")
                                                                     ("All these CDRs with empty VoIP code will not be rated.")
                                                                     ("This is an error in the CDR format. Contact the assistance.")
                                  )

                             (unitInfo, isExtensionalMatch)
                               <- case info_getDataInfoForExtensionCode organizationsInfo (fromTextToByteString accountCode) cdrTime of
                                    Right Nothing
                                      -> throwError $ createRateError
                                                        cdr1
                                                        ("unknown accountcode - " ++ accountCodeS)
                                                        ("\"" ++ accountCodeS ++ "\" VoIP account code is not associated to an extension at call-date " ++ showLocalTime cdrTime)
                                                        ("All CDRs with this VoIP code will not rated.")
                                                        ("Complete the VoIP account codes.")
                                    Left err
                                      -> throwError (cdr1, err)
                                    Right (Just r)
                                      -> return r

                             return $ (unit_id unitInfo, unitInfo, isExtensionalMatch)

          let dataInfoParents = info_getDataInfoParentHierarchy organizationsInfo unitInfo cdrTime False
          let parentIds = info_getParentIdHierarchy dataInfoParents
          let parentIdsText = info_getParentHiearchyIds dataInfoParents
          let maybeBillableDataInfo = info_getBillableDataInfo dataInfoParents
          let organizationName = info_getFullName dataInfoParents 0 False False False

          when (isNothing maybeBillableDataInfo)
               (throwError $ createRateError cdr1 ("unknown billable organization for " ++ Text.unpack parentIdsText)
                                                  ("Organization unit with id " ++ show unitId ++ ", and name " ++ organizationName ++ " has not a billable parent.")
                                                  ("These CDRs can not be assigned to a billable organization, and they can not be billed correctly.")
                                                  ("Define a billable organization in the organization hierarchy.")
               )

          case info_isOrganizationToIgnore info parentIds of
            True -> do modify  (\c -> c { cdr_direction = CDR_ignored})
                       return True
            False -> do

              let maybeRateCategoryId = info_getRateCategoryId dataInfoParents

              when (isNothing maybeRateCategoryId)
                   (throwError $ createRateError cdr1 ("unknown rate category for " ++ Text.unpack parentIdsText)
                                                      ("Organization unit with id " ++ show unitId ++ ", and name " ++ organizationName ++ " has not a price category.")
                                                      ("These CDRs can not be billed.")
                                                      ("Define a rate/price category in some part of the organization hierarchy.")
                   )

              let billableDataInfo = fromJust1 "2" maybeBillableDataInfo

              modify $ (\c -> c { cdr_organizationUnitId =  Just unitId  })
              modify $ (\c -> c { cdr_cachedParentIdHierarchy = Just parentIds })
              modify $ (\c -> c { cdr_billableOrganizationUnitId =  Just $ unit_id billableDataInfo })
              modify $ (\c -> c { cdr_priceCategoryId =  maybeRateCategoryId })

              --
              -- Complete info about telephone numbers
              --

              cdr1 <- get
              let externalTelephoneNumber = cdr_externalTelephoneNumber cdr1

              when (Text.length externalTelephoneNumber == 0)
                   (throwError $ createRateError cdr1 ("CDR without external telephone number" ++ cdr_uniqueErrorKey cdr1 )
                                                      ("This CDR has no external telephone number.")
                                                      ("This CDR has no external telephone number.")
                                                      ("The problem can be in the CDR record or in the application configuration. Contact the assistance.")
                   )

              -- NOTE: the final transformed cdr is in the state
              return isExtensionalMatch

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
         when (isNothing $ cdr_displayedMaskedExternalTelephoneNumber cdr1)
              (do let external0 = fromJust1 "4" $ cdr_displayedExternalTelephoneNumber cdr1

                  let result
                        = case cdr_direction cdr1 of
                            CDR_internal
                              -> external0
                            _ -> let external1 = Text.unpack external0

                                     -- remove the default prefix, if it is part of the telephone number
                                     maybeDefaultPrefix
                                       = case params_defaultTelephonePrefixToNotDisplay env of
                                           Just r -> Just $ Text.unpack r
                                           Nothing -> Nothing

                                     external2
                                           = case maybeDefaultPrefix of
                                               Nothing
                                                 -> external1
                                               Just defaultPrefix
                                                 -> case stripPrefix defaultPrefix external1 of
                                                      Nothing -> external1
                                                      Just r -> r
                                     external3
                                       = case numberOfDigitsToMask > 0 of
                                           False
                                             -> external2
                                           True
                                             -> case (L.length external2) <= numberOfDigitsToMask of
                                                  True
                                                    -> external2
                                                       -- do not mask very short telephone numbers
                                                  False
                                                    -> let toRemove = min (L.length external2) numberOfDigitsToMask
                                                           toTake = (L.length external2) - toRemove
                                                       in  L.take toTake external2 ++ L.replicate numberOfDigitsToMask 'X'

                                 in fromString external3

                  modify (\c -> c {  cdr_displayedMaskedExternalTelephoneNumber = Just result })
                  return ()
              )

         -- NOTE: the real result is the CDR saved/modified in the state
         return ()

  createRateError cdr key descr effect solution
    = ( cdr
      , createError Type_Error Domain_RATES key descr effect solution)

-- | Use the main rate plan for rating a CDR: cost and income.
--   Update also the BundleState, and generate the related service-cdrs.
--   @require cdrs are sent in order of call-date
mainRate_apply
   :: RatingParams
   -> IORef (CountOfCDRS, CallDate, Set.Set CallDate, BundleState, Maybe BundleState, Maybe BundleState)
   -> CDR
   -> RatingMonad CDR
mainRate_apply env statsR cdr1
  = case isVoipReseller of
      True -> do cdr2 <- searchRateAndApply CostRate cdr1
                 cdr3 <- searchRateAndApply IncomeRate cdr2
                 return cdr3
      False -> do cdr2 <- searchRateAndApply IncomeRate cdr1
                  -- in case of call reporting the cost and income are the same
                  let cdr3 = cdr2 { cdr_cost = cdr_income cdr2
                                   , cdr_debug_cost_rate = cdr_debug_income_rate cdr2
                                   }
                  return cdr3

 where

  isDebugMode = params_isDebugMode env
  isVoipReseller = params_isVoipReseller env

  dbg :: (NFData a) => a -> Maybe a
  dbg a = DeepSeq.force $ if isDebugMode then (Just a) else Nothing
  {-# INLINE dbg #-}

  dbg' :: (NFData a) => Maybe a -> Maybe a
  dbg' a = DeepSeq.force $ if isDebugMode then a else Nothing
  {-# INLINE dbg' #-}

  dbgT :: Text.Text -> Text.Text
  dbgT a = DeepSeq.force $ if isDebugMode then a else Text.empty
  {-# INLINE dbgT #-}

  bundleDebugInfo
     :: BundleState
     -> Maybe BundleRateSystemId
     -> Maybe BundleRateUnitId
     -> ( Maybe Int
          -- ^ left calls
        , Maybe Int
          -- ^ left duration
        , Maybe MonetaryValue
          -- ^ left cost
        )

  bundleDebugInfo _ Nothing _ = (Nothing, Nothing, Nothing)

  bundleDebugInfo _ _ Nothing = (Nothing, Nothing, Nothing)

  bundleDebugInfo (_, state1) (Just rateId) (Just unitId)
     = case IMap.lookup rateId state1 of
         Nothing
           -> (Nothing, Nothing, Nothing)
         Just (_, _, map2)
           -> case IMap.lookup unitId map2 of
                Nothing
                  -> (Nothing, Nothing, Nothing)
                Just (params, _)
                  -> (  bundle_leftCalls params
                      , bundle_leftDuration params
                      , bundle_leftCost params
                     )

  -- | Rate the CDR calculating cost or income.
  --   @require RatingStatus is correctly initializated.
  --   @require RatingStatus has no critical error.
  searchRateAndApply
    :: RateRole
    -> CDR
    -> RatingMonad CDR
  searchRateAndApply rateRole cdr
    = do (_, _, _, bundleState1, _, _) <- liftIO $ readIORef statsR
         let ratePlan :: CheckedMainRatePlan
               = case rateRole of
                   IncomeRate
                     -> params_incomeRate env
                   CostRate
                     -> params_costRate env
         -- liftIO $ putStrLn $ "mainRate_apply: here 0 "

         (!value, !bundleRateUnitId, !bundleState2, debugInfo)
            <- case mainRatePlan_calcCost isDebugMode rateRole env bundleState1 ratePlan cdr of
                 Right r
                   -> do -- liftIO $ putStrLn $ "mainRate_apply: here 1.1 correctly rated "
                         return r
                 Left err
                   -> do -- liftIO $ putStrLn $ "mainRate_apply: here 1.2 error " ++ asterisellError_userShow err
                         throwError (cdr, err)

         -- liftIO $ putStrLn $ "mainRate_apply: here 2 " ++ show value

         let debugRatingDetails1
               = let s = fromJust1 "ERR 56" $ info_ratingDetails $ fromJust1 "ERR 55" debugInfo
                     rateRoleName
                       = case rateRole of
                           IncomeRate -> "\nDetails during calculation of Income:\n"
                           CostRate -> "\nDetails during calculation of Cost:\n"
                 in Text.append rateRoleName s

         -- liftIO $ putStrLn $ "mainRate_apply: here 3 " ++ show debugRatingDetails1

         -- liftIO $ putStrLn $ "mainRate_apply: here 4 " ++ show debugRatingDetails2

         let appliedRateName
               = fromJust1 "ERR 12555" $ IMap.lookup (info_appliedRate $ fromJust1 "ERR 2274" debugInfo) (mainRatePlan_systemIdToUserIdPath ratePlan)

         -- liftIO $ putStrLn $ "mainRate_apply: here 5 " ++ show appliedRateName

         let residualAppliedRate
               = case (info_residualAppliedRate $ fromJust1 "ERR 0247" debugInfo) of
                   Nothing
                     -> Text.empty
                   Just i
                     -> fromJust1 "ERR 753777" $ IMap.lookup i (mainRatePlan_systemIdToUserIdPath ratePlan)

         -- liftIO $ putStrLn $ "mainRate_apply: here 6 " ++ show residualAppliedRate

         case rateRole of
           IncomeRate
             -> let (leftCalls, leftDuration, leftCost)
                      = bundleDebugInfo bundleState2 (info_bundleRateSystemId $ fromJust1 "ERR 775344" debugInfo) bundleRateUnitId

                in do liftIO $ modifyIORef' statsR (\(c1, c2, c3, _, c4, c5) -> (c1, c2, c3, bundleState2, c4, c5))
                      return $ cdr {   cdr_income = value
                                     , cdr_bundleOrganizationUnitId = bundleRateUnitId
                                     , cdr_debug_income_rate = dbg $ appliedRateName
                                     , cdr_debug_residual_income_rate = dbg $ residualAppliedRate
                                     , cdr_debug_residual_call_duration = dbg' $ info_residualCallDuration $ fromJust1 "ERR 772" debugInfo
                                     , cdr_debug_bundle_left_calls = dbg' $ leftCalls
                                     , cdr_debug_bundle_left_duration = dbg' $ leftDuration
                                     , cdr_debug_bundle_left_cost = dbg' $ leftCost
                                     , cdr_debug_rating_details = dbg $ debugRatingDetails1
                                     }

           CostRate
             -> return $ cdr { cdr_cost = value
                             , cdr_debug_cost_rate = dbg $ appliedRateName
                             , cdr_debug_rating_details = dbg $ debugRatingDetails1
                             }

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
