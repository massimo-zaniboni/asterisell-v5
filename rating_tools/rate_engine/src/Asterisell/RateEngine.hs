{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

{- $LICENZE 2013, 2014, 2015, 2016, 2017
 * Copyright (C) 2013-2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 ofstributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
-}

-- | Rate CDRs.
--
module Asterisell.RateEngine (
  rateEngine_rate,
  SourceDataSummaryInfo,
  DBConf(..),
  RunLevel(..),
  rateEngine_importDataFile,
  rateEngine_exportDataFile,
  tt_rateSpecificationsTests,
  tt_mainRateCalcTets
) where

import Asterisell.Process
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

import GHC.Generics (Generic)
import Data.List as L
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Control.Monad as M
import Control.Monad.IO.Class       (liftIO)
import Data.Word
import qualified Data.Vector as V
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
import System.IO as IO
import System.Directory as IO
import Debug.Trace
import Data.Maybe
import Data.Set as Set
import Data.IntMap as IMap
import Data.IORef
import qualified Data.Serialize as DBS
import qualified Data.Serialize.Text as DBS
import Data.String

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

import System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S
import qualified Data.Vector as V

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Control.DeepSeq
import Control.DeepSeq.Generics
import Control.Parallel
import Control.Parallel.Strategies

import Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc

-- ---------------------------------
-- Params

-- | Number of CDRs to process in each phase: parse, rate, write to DB.
--   A bigger chunk reduces the number of DB round-trips, because more CDRs are sent to the DB.
param_chunkSize :: Int
param_chunkSize = 512

-- | The number of source CDRs to fetch from the DB, before splitting the rating event.
--   Up to date the Haskell MySQL driver does not support the forward-only cursor mode,
--   and it stores all the CDRS of the rating query in RAM.
--   So the query is splitted (efficiently) in smaller queries, and they are rated separately.
--   NOTE: 30K CDRs requires approximately 100M - 200M of RAM (according the number of rates to load)
--   NOTE: the cost of splitting the query is rather neglible.
param_chunkSize2 :: Int
param_chunkSize2 = 1024 * 30

-- | Used for testing the application in reduced running contexts
data RunLevel
  = RunLevelFull
  | RunLevelUntilRating
  | RunLevelUntilPortedTelephoneNumberConversion
  | RunLevelUntilSourceCDRParsing
  | RunLevelUntilReadingFromDB
 deriving (Eq, Ord, Show, Generic, NFData)

-- | Meta info about a SourceDataFile.
--   At this stage only the calldate is imported and processed.
type SourceDataSummaryInfo
  = ( Maybe (LocalTime, LocalTime)
      -- ^ max and minimum found call date
    , Int
      -- ^ total number of lines
    , Int
      -- ^ lines with errors
    )

-- -------------------------------------------------------------------
-- DB Process

-- | Give a unique human readable name to a prepared statement.
data StmtName
  = Stmt_insertExpandedExtension
  | Stmt_insertCDRWithError
 deriving (Eq, Ord, Generic)

instance Hashable StmtName

-- | Store prepared statements.
type PreparedStms = H.HashMap StmtName StmtID

preparedStmts_empty :: PreparedStms
preparedStmts_empty = H.empty

data DBProcessState
       = DBProcessState {
           dbps_preparedStatements :: PreparedStms
         , dbps_errorsDictionary :: AsterisellErrorsDictionary
         }

dbProcessState_empty :: DBProcessState
dbProcessState_empty
    = DBProcessState {
        dbps_preparedStatements = preparedStmts_empty
      , dbps_errorsDictionary = asterisellErrorsDictionary_empty
      }

dbProcess_getPreparedStmt
  :: DB.MySQLConn
  -> IORef DBProcessState
  -> StmtName
  -> IO StmtID

dbProcess_getPreparedStmt conn pStateR stmtName = do
  pState <- readIORef pStateR
  let map1 = dbps_preparedStatements pState
  case H.lookup stmtName map1 of
    Just id
      -> return id
    Nothing
      -> do id <- DB.prepareStmt conn getQuery
            let map2 = H.insert stmtName id map1
            writeIORef pStateR (pState { dbps_preparedStatements = map2 })
            return id

 where

  getQuery
    = case stmtName of
        Stmt_insertExpandedExtension
          -> [str|INSERT INTO ar_expanded_extensions
                 |SET ar_organization_unit_id = ?
                 |,   extension_code = ?
                 |]

        Stmt_insertCDRWithError
          -> [str|INSERT INTO ar_new_problem(
                 |   ar_problem_type_id, ar_problem_domain_id, ar_problem_responsible_id, created_at
                 | , duplication_key, garbage_collection_key, garbage_collection_from
                 | , garbage_collection_to
                 | , description, effect, proposed_solution
                 | , signaled_to_admin)
                 |VALUES(?,?,?, NOW(), ?, ?, ?, ?, ?, ?, ?, 0)
                 |ON DUPLICATE KEY UPDATE
                 |  garbage_collection_from = LEAST(garbage_collection_from, VALUES(garbage_collection_from))
                 |, garbage_collection_to = GREATEST(garbage_collection_to, VALUES(garbage_collection_to))
                 |, ar_problem_type_id = VALUES(ar_problem_type_id)
                 |, ar_problem_domain_id  = VALUES(ar_problem_domain_id)
                 |, ar_problem_responsible_id  = VALUES(ar_problem_responsible_id)
                 |, created_at = VALUES(created_at)
                 |, garbage_collection_key = VALUES(garbage_collection_key )
                 |, description = VALUES(description )
                 |, effect = VALUES(effect)
                 |, proposed_solution = VALUES(proposed_solution)
                 |, signaled_to_admin  = signaled_to_admin ;
                 |]


data Cmd
  = Cmd_wait (MVar ())
    -- ^ wait the process is ready for accepting new commands.
    --   Useful in few computation phases where it is important starting a reading query,
    --   when the writing process is ready to write values.

  | Cmd_deleteFromArSource PhysicalFormatId ProviderId (Maybe (LocalTime, LocalTime))
    -- ^ Remove old status CDRs with the same cdr provider, and logical-type,
    -- in a timeframe that is the same of this file, because this new piece of information
    -- is a status replacing completely old values.
    -- Pass Nothing as time-frame for deleting all the CDRs of the provider.

  | Cmd_insertSourceCDR
      [(  LocalTime     -- ^ the call date
        , Int           -- ^ providerId
        , Int           -- ^ logicalTypeId
        , Int           -- ^ formatTypeId
        , Bool          -- ^ areImportedServiceCDRS
        , BS.ByteString -- ^ source CDR content in native format
       )]

  | Cmd_insertCDR [CDR]

  | Cmd_insertExpandedExtension ExpandedExtension

  | Cmd_updateRatingTimeFrame
      -- ^ inform the application that there are new calls to rate in the specified time-frame
      Bool
      -- ^ True for imported service CDR, False for normal CDRs
      LocalTime
      -- ^ from call time
      LocalTime
      -- ^ to call time

  | Cmd_deleteCDRS
    -- ^ delete old calculated CDRs (but not imported service CDRS) before calculating new ones.
      LocalTime -- ^ bundle state from call date
      LocalTime -- ^ normal calls from call date
      LocalTime -- ^ normal calls to call date
      LocalTime -- ^ bundle state can be saved from call date
      LocalTime -- ^ bundle state to call date

  | Cmd_deleteOnlyImportedServiceCDRS LocalTime LocalTime

  | Cmd_saveBundleState LocalTime BundleState

  | Cmd_insertCDRWithError
      AsterisellError
      (Maybe (BS.ByteString, CallDate, CallDate))
      -- ^ the garbage-key and the timeframe in which the error is present
      (Maybe CDR)
    -- NOTE: it is safe using the same transaction both for CDR and errors here, because if CDRs are not saved, errors are not generated.
    -- Critical errors aborting the rating process are not signaled here,
    -- but from other PHP code.

  | Cmd_close
      Bool
      -- ^ True for closing a onlyImportedServiceCDRS event
      LocalTime
      -- ^ the fromCallDate
      LocalTime
      -- ^ the bundleStateToCallDate
      RatingStatus
      -- ^ additional info to write
    -- @requires: this is the last called command
    -- close the rating process, writing the rating status.

 deriving (Show, Generic, NFData)

instance Show (MVar ()) where
  show _ = "MVar ()"

-- | Receive commands and execute them on the DB, inside a transaction.
--   Throw an exception in case of error, and rollback the transaction.
--   Commit the transaction when the stream is closed.
--
process_db
  :: Bool
  -- ^ True for debug mode
  -> DBConf
  -- ^ connection parameters
  -> CurrencyPrecisionDigits
  -> Maybe FilePath
  -- ^ the file to delete in case the info is successfully committed to the database
  -> Chan Cmd
  -> IO ()

process_db isDebugMode dbConf pd maybeFileName inChan
  = safeBracket
      (openDBConnection dbConf True)
      (\maybeExc conn -> do
          case maybeExc of
            Just exc
              -> do hPutStrLn IO.stderr $! "process_db: exception " ++ displayException exc
                    DB.execute_ conn "ROLLBACK"
                    return ()
            Nothing
              -> do return ()
          DB.close conn)
      (\conn -> do
          initialState <- newIORef dbProcessState_empty
          processCmds conn initialState
          DB.execute_ conn "COMMIT"

          -- Bet/hope that after the commit the delete operation always suceedee,
          -- otherwise in case it is not a status file then repeated info will be added.
          case maybeFileName of
            Nothing
              -> do return ()
            Just fileName
              -> do removeFile fileName
                    return ())

 where

   processCmds conn stateR = do
     cmds <- readChan inChan

     case isEOFChan cmds of
       True -> do return ()
       False -> do V.mapM_ (processCmd conn stateR) cmds
                   processCmds conn stateR

   processCmd :: DB.MySQLConn -> IORef DBProcessState -> Cmd -> IO ()
   processCmd conn pStateR cmd
     = case cmd of
         Cmd_wait v
           -> do putMVar v ()

         Cmd_deleteFromArSource formatId providerId Nothing
           -> do let query = "DELETE FROM ar_source_cdr WHERE ar_physical_format_id = ? AND ar_cdr_provider_id  = ? "
                 DB.execute conn query [toDBInt64 formatId, toDBInt64 providerId]
                 return ()

         Cmd_deleteFromArSource formatId providerId (Just (fromDate, toDate))
           -> do let query = [str|DELETE FROM ar_source_cdr
                                 |WHERE ar_physical_format_id = ?
                                 |AND ar_cdr_provider_id  = ?
                                 |AND calldate >= ?
                                 |AND calldate < ?
                                 |]

                 DB.execute conn query [ toDBInt64 formatId
                                       , toDBInt64 providerId
                                       , toDBLocalTime fromDate
                                       , toDBLocalTime toDate]
                 return ()

         Cmd_insertSourceCDR cdrs
           -> let sqlStmt
                     = [str|INSERT INTO ar_source_cdr(
                           |  calldate
                           |, ar_cdr_provider_id
                           |, ar_physical_format_id
                           |, is_imported_service_cdr
                           |, content
                           |) VALUES(?,?,?,?,?)
                           |]
                  toCdr (callDate, providerId, _, versionId, isImportedServiceCDR, source)
                     = [ toDBLocalTime callDate
                       , toDBInt64 providerId
                       , toDBInt64 versionId
                       , toDBBool isImportedServiceCDR
                       , DB.MySQLBytes source]

              in case L.null cdrs of
                   True ->  do return ()
                   False -> do DB.executeMany conn sqlStmt (L.map toCdr cdrs)
                               return ()

         Cmd_updateRatingTimeFrame importedServiceCDRS fromDate toDate
           -> do let query
                       = case importedServiceCDRS of
                           True
                             -> [str|UPDATE ar_params
                                    |SET scheduled_imported_services_rerate_from_specific_calldate = IFNULL(LEAST(scheduled_imported_services_rerate_from_specific_calldate, ?), ?)
                                    |,   scheduled_imported_services_rerate_to_specific_calldate = IFNULL(GREATEST(scheduled_imported_services_rerate_to_specific_calldate, ?), ?)
                                    |WHERE is_default = 1
                                    |]
                           False
                             -> [str|UPDATE ar_params
                                    |SET new_imported_cdrs_from_calldate = IFNULL(LEAST(new_imported_cdrs_from_calldate, ?), ?)
                                    |,   new_imported_cdrs_to_calldate = IFNULL(GREATEST(new_imported_cdrs_to_calldate, ?), ?)
                                    |WHERE is_default = 1
                                    |]

                 DB.execute conn query [toDBLocalTime fromDate, toDBLocalTime fromDate, toDBLocalTime toDate, toDBLocalTime toDate]
                 return ()

         Cmd_deleteCDRS bundleStateFromCallDate fromCallDate toCallDate bundleStateCanBeSavedFromCallDate bundleStateToCallDate
           -> do
                  let d0 = toDBLocalTime bundleStateFromCallDate
                  let d1 = toDBLocalTime fromCallDate
                  let d2 = toDBLocalTime toCallDate
                  let d4 = toDBLocalTime bundleStateToCallDate

                  -- The temporal order is
                  -- d0 <= d1 < d2 <= d4

                  -- Remove all CDRS and services. They can be of these types:
                  -- * normal CDRS
                  -- * service CDRS associated to bundle rates
                  -- * pure service CDRS
                  -- * imported services
                  --
                  -- The contracts to respect are:
                  -- 1. if a type of CDR is deleted, then it is also generated and saved in the rating phase
                  -- 2. if a type of CDR is not deleted, then it is not saved in the rating phase
                  -- 3. optionally a CDR can be generated/rated, for updating some state, but not saved
                  -- 4. all source CDRS are rated and saved if all time frames are rerated 
                  --
                  -- Delete normal CDRS, from d1 (the start of rating period), to d4.
                  -- Consider d4 instead of d2, because if initial CDRS changes, then the bundle state can change too, and CDRS after d2 can be not
                  -- anymore in synchro/correctly-rated.
                  --
                  -- The service CDRS associated to bundle rates,
                  -- with to_calldate > d0 and to_calldate <= d4, must be deleted, rerated, and saved, because:
                  -- * there can not be service CDRS after `bundleStateCanBeSavedFromCallDate`
                  --
                  -- Pure services are deleted, generated and saved if their starting date is inside d1 and d2.
                  -- The to_calldate of a pure service cdr is not taken in consideration because it can be larger than the bundle time frame.
                  --
                  -- Imported services are not deleted in any case.
                  --
                  -- NOTE: modification of a pure service (params, or customer assignation) or of a bundle rate, generate a rerate event of unbilled time-frame,
                  -- so they are recomputed automatically.

                  let q1 = [str| DELETE FROM ar_cdr
                               | WHERE
                               |    (to_calldate IS NULL AND calldate >= ? AND calldate < ? AND is_imported_service_cdr = 0)
                               | OR (to_calldate IS NOT NULL AND debug_cost_rate IS NOT NULL AND debug_income_rate IS NOT NULL AND to_calldate > ? AND to_calldate <= ? AND is_imported_service_cdr = 0)
                               | OR (to_calldate IS NOT NULL AND debug_cost_rate IS NULL AND debug_income_rate IS NULL AND calldate >= ? AND calldate < ? AND is_imported_service_cdr = 0)
                               |]
                  -- NOTE: delete in order:
                  -- * normal calls having to_calldate IS NULL
                  -- * bundle rate services having to_calldate IS NOT NULL, and debug_cost_rate and debug_income_rate IS NOT NULL
                  -- * pure services having to_calldate IS NOT NULL, and debug_cost_rate and debug_income_rate IS NULL

                  DB.execute
                    conn
                    (DB.Query q1)
                    [ d1, d4
                    , d0, d4
                    , d1, d2]

                  let qb = "DELETE FROM ar_bundle_state WHERE to_time >= ?"
                  DB.execute conn qb [toDBLocalTime bundleStateCanBeSavedFromCallDate]

                  return ()

         Cmd_deleteOnlyImportedServiceCDRS fromCallDate toCallDate
           -> do -- Delete imported service cdrs inside the rating period.
                 -- They will be imported again from ar_source_cdr, according their calldate, so only their calldate is taken in consideartion, and not to_calldate.

                 let q = [str|DELETE FROM ar_cdr
                             |WHERE is_imported_service_cdr = 1
                             |AND calldate >= ?
                             |AND calldate < ?
                             |]

                 let v = [toDBLocalTime fromCallDate, toDBLocalTime toCallDate]

                 DB.execute conn q v
                 return ()

         Cmd_insertCDR cdrs
           -> do
                 let q =  [str|INSERT INTO ar_cdr(
                              |  calldate, to_calldate, count_of_calls, is_imported_service_cdr, destination_type
                              |, is_redirect, duration, billsec, ar_organization_unit_id, cached_parent_id_hierarchy
                              |, billable_ar_organization_unit_id, bundle_ar_organization_unit_id, income, cost_saving
                              |, ar_vendor_id, ar_communication_channel_type_id, cost, expected_cost, ar_telephone_prefix_id
                              |, cached_external_telephone_number, external_telephone_number_with_applied_portability
                              |, cached_masked_external_telephone_number, error_destination_type, ar_problem_duplication_key
                              |, debug_cost_rate, debug_income_rate, debug_residual_income_rate, debug_residual_call_duration
                              |, debug_bundle_left_calls, debug_bundle_left_duration, debug_bundle_left_cost, debug_rating_details
                              |) VALUES(?,?,?,?,?,?,?,?,?,?,
                              |         ?,?,?,?,?,?,?,?,?,?,
                              |         ?,?,?,?,?,?,?,?,?,?,
                              |         ?,?)
                              |]

                 case L.null cdrs of
                   True -> do return ()
                   False -> do DB.executeMany
                                 conn
                                 q
                                 (L.map (\cdr -> [ toDBMaybeLocalTime $ Just $ cdr_calldate cdr
                                                 , toDBMaybeLocalTime $ cdr_toCalldate cdr
                                                 , toDBInt64 $ cdr_countOfCalls cdr
                                                 , toDBBool $ cdr_isImportedServiceCDR cdr
                                                 , toDBInt64 $ cdrDirection_asterisellCode $ cdr_direction cdr
                                                 , toDBBool $ cdr_isRedirect cdr
                                                 , toMaybeInt64 $ cdr_duration cdr
                                                 , toMaybeInt64 $ cdr_billsec cdr
                                                 , toMaybeInt64 $ cdr_organizationUnitId cdr
                                                 , toDBIds $ cdr_cachedParentIdHierarchy cdr
                                                 , toMaybeInt64 $ cdr_billableOrganizationUnitId cdr
                                                 , toMaybeInt64 $ cdr_bundleOrganizationUnitId cdr
                                                 , toMaybeMoney pd $ Just $ cdr_income cdr
                                                 , toMaybeMoney pd $ Just $ cdr_costSaving cdr
                                                 , toMaybeInt64 $ cdr_vendorId cdr
                                                 , toMaybeInt64 $ cdr_communicationChannelTypeId cdr
                                                 , toMaybeMoney pd $ Just $ cdr_cost cdr
                                                 , toMaybeMoney pd $ cdr_expectedCost cdr
                                                 , toMaybeInt64 $ cdr_telephonePrefixId cdr
                                                 , toDBText $ cdr_externalTelephoneNumber cdr
                                                 , toDBMaybeText $ cdr_externalTelephoneNumberWithAppliedPortability cdr
                                                 , toDBMaybeText $ cdr_displayedMaskedExternalTelephoneNumber cdr
                                                 , toDBInt64 $ cdrDirection_asterisellCode $ cdr_errorDirection cdr
                                                 , toDBMaybeText $   cdr_problemDuplicationKey cdr
                                                 , toDBMaybeText $ cdr_debug_cost_rate cdr
                                                 , toDBMaybeText $ cdr_debug_income_rate cdr
                                                 , toDBMaybeText $ cdr_debug_residual_income_rate cdr
                                                 , toMaybeInt64 $ cdr_debug_residual_call_duration cdr
                                                 , toMaybeInt64 $ cdr_debug_bundle_left_calls cdr
                                                 , toMaybeInt64 $ cdr_debug_bundle_left_duration cdr
                                                 , toMaybeMoney pd $ cdr_debug_bundle_left_cost cdr
                                                 , toDBMaybeText $ cdr_debug_rating_details cdr
                                                 ]) cdrs)
                               return ()

         Cmd_saveBundleState toCallDate bundleState
           -> do let serializatedBundleState = LZ.compress $ DBS.encode bundleState
                 let q = "INSERT INTO ar_bundle_state(to_time, data_file) VALUES(?, ?)"
                 let v = [toDBLocalTime toCallDate, toDBByteString serializatedBundleState]
                 DB.execute conn q v
                 return ()

         Cmd_insertExpandedExtension ee
           -> do
                 (DBProcessState preparedStmts errors) <- readIORef pStateR
                 !stmtId <- dbProcess_getPreparedStmt conn pStateR Stmt_insertExpandedExtension
                 DB.executeStmt conn stmtId [toDBInt64 $ ee_organizationId ee, toDBByteString $ ee_specificExtensionCode ee]
                 return ()

         Cmd_insertCDRWithError err maybeGarbageKey maybeCDR
           -> do
                 (DBProcessState !preparedStmts1 !errors1) <- readIORef pStateR
                 !stmtId <- dbProcess_getPreparedStmt conn pStateR Stmt_insertCDRWithError
                 let garbageKeyParams
                       = case maybeGarbageKey of
                           Nothing
                             -> [MySQLNull, MySQLNull, MySQLNull]
                           Just (garbageKey, fromDate, toDate)
                             -> [MySQLBytes garbageKey, toDBLocalTime fromDate, toDBLocalTime toDate]

                 let dupKey = asterisellError_dupKey err

                 let params = [ MySQLInt64 $ fromIntegral $ errorType_toPHPCode $ asterisellError_type err
                              , MySQLInt64 $ fromIntegral $ errorDomain_toPHPCode $ asterisellError_domain err
                              , MySQLInt64 $ fromIntegral $ errorResponsible_toPHPCode $ asterisellError_responsible err
                              , MySQLText dupKey
                              ]
                              ++ garbageKeyParams
                              ++ [ MySQLText $ Text.pack $ asterisellError_description err
                                 , MySQLText $ Text.pack $ asterisellError_effect err
                                 , MySQLText $ Text.pack $ asterisellError_proposedSolution err
                                 ]

                 let (insertInDB, errors2)
                       = case HS.member dupKey errors1 of
                           True ->  (False, errors1)
                           False -> (True, HS.insert dupKey errors1)

                 (DBProcessState !preparedStmts2 _) <- readIORef pStateR
                 writeIORef pStateR (DBProcessState preparedStmts2 errors2)

                 -- NOTE: insert the error in any case, because maybe the date with the error range is updated.
                 DB.executeStmt conn stmtId params

                 -- NOTE: insertt the CDR in any case because we must mantain stats abouts CDRS with errors.
                 case maybeCDR of
                   Nothing -> return ()
                   Just cdr1
                     -> let newErrorDirection
                              = case cdr_direction cdr1 of
                                  CDR_error -> cdr_errorDirection cdr1
                                  _ -> cdr_direction cdr1

                            cdr2 = cdr1 {
                                     cdr_errorDirection = newErrorDirection
                                   , cdr_direction = CDR_error
                                   , cdr_problemDuplicationKey = Just dupKey
                                   , cdr_duration = case cdr_duration cdr1 of
                                                      Nothing -> Just 0
                                                      Just r -> Just r
                                   , cdr_billsec = case cdr_billsec cdr1 of
                                                      Nothing -> Just 0
                                                      Just r -> Just r
                                   }

                        in do processCmd conn pStateR (Cmd_insertCDR [cdr2])
                              return ()

         Cmd_close onlyImportedServiceCDRS fromCallDate bundleStateToCallDate state1
           -> do
                 case onlyImportedServiceCDRS of
                   True -> return ()
                   False -> do
                     !stmtId <- DB.prepareStmt conn "CALL add_daily_status_change_event(?)"
                     let (allDays, _) = timeFrame_getYearsAndMonth (fromCallDate, bundleStateToCallDate)

                     let toCallDate' d = toDBLocalTime $ LocalTime { localDay = d, localTimeOfDay = midnight}
                     M.mapM_ (\d -> DB.executeStmt conn stmtId [toCallDate' d]) (Set.toList allDays)

-- | Receive commands but does not execute them.
--   It is a fake process, for testing maximum speed of other processes.
process_fakeDB
  :: Bool
  -> DBConf
  -- ^ connection parameters
  -> CurrencyPrecisionDigits
  -> Maybe FilePath
  -- ^ the file to delete in case the info is successfully committed to the database
  -> Chan Cmd
  -> IO ()

process_fakeDB isDebugMode dbConf pd maybeFileName inS
  = safeBracket
     (return ())
     (\maybeExc _ -> do
       case maybeExc of
         Just exc
           -> do hPutStrLn IO.stderr $! "process_db exception: " ++ displayException exc
                 return ()
         _ -> do return ())
     (\_ -> do hashR <- newIORef 0
               processCmds hashR)
 where

   processCmds hashR = do
     !cmds <- readChan inS
     case isEOFChan cmds of
       True -> do h <- readIORef hashR
                  case h == 1753 of
                    True -> error "unlucky event tested only for forcing evaluation"
                    False -> return ()
       False -> do V.mapM_ (process hashR) cmds
                   processCmds hashR

   process hashR cmd = do
     -- putStrLn $ "!! DB CMD: " ++ show cmd
     !h <- readIORef hashR
     case cmd of
       (Cmd_wait v)
         -> do putMVar v ()
       (Cmd_insertSourceCDR cdrs)
         -> do let !h3 = L.foldl' (\h1 (_, _, _, _, _, s) -> h1 + (hash s)) h cdrs
               writeIORef hashR h3
       _ -> do return ()

-- -------------------------------------------------------------------
-- Initial Import of source/raw CDRS

-- | Parse a file and import into database as ar_source_cdr records, returning summary info on its content.
--   Report only serious errors, because all errors specific of CDRs will be reported during rating phase.
--   CDRs without a clear calldate, will be imported using the previous calldate, or the calldate of today.
--
-- The design is this:
-- * in a first phase source data files, with source CDRS are imported into DB, ordered by calldate
-- * only the calldate is parsed
-- * the rest of the content is only a string
-- * the source CDRs can be ordered by call date directly from the DB
-- * during rating phases the source CDRs are read by calldate from the DB, and the string with the content is converted to a CDR and rated
-- * the engine signal if after the changing of a parsing algo, the calldate is interpreted in a different way
--
rateEngine_importDataFile
  :: Maybe FilePath
  -> FilePath
  -> Bool
  -- ^ True if the input file must be deleted at the end of the processing
  -> Bool
  -- ^ True if the file contains imported service CDRS
  -> CDRProviderName
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> Bool
  -> Maybe (LocalTime, LocalTime)
  -> DBConf
  -> CurrencyPrecisionDigits
  -> Bool
  -- ^ True if it is a real importing, False if it is only a test of the code,
  --   and no data must be written on the DB.
  -> IO SourceDataSummaryInfo

rateEngine_importDataFile
  maybeDebugFile
  inputFile
  deleteInputFile
  areImportedServiceCDRS
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
  isStatusFile
  maybeStatusTimeFrame
  dbConf
  precision
  loadRealData = do

   let isDebugMode = isJust maybeDebugFile

   (CDRFormatSpec !sourceCDRParams !theType)
     <- case getSupportedCDRSImporters logicalTypeName formatTypeName of
          Nothing
            -> throwIO $ AsterisellException ("There is no known/configured source data file importer for format " ++ (Text.unpack logicalTypeName) ++ ", and version " ++ (Text.unpack formatTypeName))
          Just r
            -> return r

   cmdsChan <- newChan (Just 4)
   -- NOTE: use a small bounded chan because the source-cdrs are already batched during parsing,
   -- and they are sent in batched mode to the DB engine.

   handleAny handleImportErrors $
     withAsync
      (case loadRealData of
         True -> process_db isDebugMode dbConf precision (if deleteInputFile then (Just inputFile) else Nothing) cmdsChan
         False -> process_fakeDB isDebugMode dbConf precision (if deleteInputFile then (Just inputFile) else Nothing) cmdsChan)
      (\dbThread -> do
          when isStatusFile (writeChan cmdsChan (V.singleton $ Cmd_deleteFromArSource formatTypeId providerId maybeStatusTimeFrame))
          case maybeStatusTimeFrame of
            Just (d1, d2)
              -> writeChan cmdsChan (V.singleton $ Cmd_updateRatingTimeFrame areImportedServiceCDRS d1 d2)
            _ -> return ()
            -- updated at the end of importing when imported calldates are known

          -- Process CDRs.
          nowLocalTime1 <- getZonedTime
          let nowLocalTime = zonedTimeToLocalTime nowLocalTime1
          summaryInfo@(!maybeTimeFrame, !totLines, !linesWithErrors)
            <- S.withFileAsInput inputFile (\fileContentStream -> do
                 defaultCallDateRef <- newIORef nowLocalTime
                 summaryInfoRef <- newIORef (Nothing, 0, 0)
                 sourceCDRStream <- deriveSourceCDRImporter theType sourceCDRParams fileContentStream
                 callDateStream <- S.mapM (extractCallDate defaultCallDateRef summaryInfoRef) sourceCDRStream
                 callDateStream2 <- S.chunkVector param_chunkSize callDateStream
                 cdrStream <- S.map insertCDRS callDateStream2
                 writeStreamToChan cdrStream cmdsChan
                 readIORef summaryInfoRef)

          -- Update the rating frame again.
          case totLines > 0 && linesWithErrors == totLines of
            True -> throw $ AsterisellException $ "The code can not parse correctly the content. The code processing the file contains errors, or the file has not the specified format."
            False -> do case maybeTimeFrame of
                          Nothing
                            -> do return ()
                          Just (rateFromCallDate, rateToCallDate)
                            -> do writeChan cmdsChan (V.singleton $ Cmd_updateRatingTimeFrame areImportedServiceCDRS rateFromCallDate rateToCallDate)
                                  return ()

          -- Send the signal about the end of rating process, and close the db write thread.
          -- If it is all ok also the input file is (in case) deleted from the input directory.
          writeEOFChan cmdsChan

          wait dbThread
          return summaryInfo)

 where

   handleImportErrors :: SomeException -> IO SourceDataSummaryInfo
   handleImportErrors e
     = throw $ AsterisellException $ "There is an error during the importing of file \"" ++ inputFile ++ "\", with provider " ++ (Text.unpack providerName) ++ ", format " ++  (Text.unpack logicalTypeName) ++ "__" ++  (Text.unpack formatTypeName) ++ ". " ++ (show e) ++ ".\nAll the source data files with this format, and/or similar problems, will be not imported and rated. So there can be an high number of CDRs that are not rated. Note that there is only one error message for a file of this type, but this error message implies that all files of the same type are not rated.\nThis is probably an error in the application configuration. Contact the assistance."

   newMaxMinCallDate :: LocalTime -> Maybe (LocalTime, LocalTime) -> Maybe (LocalTime, LocalTime)
   newMaxMinCallDate d Nothing = Just (d, d)
   newMaxMinCallDate d (Just (minDD, maxDD)) = Just (min minDD d, max maxDD d)

   extractCallDate
     :: IORef LocalTime
     -- ^ default call date to use in case of critical errors
     -> IORef SourceDataSummaryInfo
     -> (BS.ByteString, Either AsterisellError LocalTime)
     -- ^ info about source CDR
     -> IO (Bool -- ^ True if there are no errors
           , LocalTime
           , BS.ByteString)

   extractCallDate lastCallDateRef summaryInfoRef (!nativeCDR, !maybeLocalTime)
       = do callDateInfo
              <- case maybeLocalTime of
                   Left !err
                     -> do !lastCallDate <- readIORef lastCallDateRef
                           return (False, lastCallDate, nativeCDR)
                           -- NOTE: in case of error update only the count of lines with errors,
                           -- and use a "current date".
                           -- In this way the error will be reported in details when the user try to rate recent calls.
                           -- This is an hack but 99% of the times it is the correct thing to do.
                           -- Note that only very critical errors with CDRs without a calldate are reported here.
                   Right !callDate
                     -> do writeIORef lastCallDateRef callDate
                           return (True, callDate, nativeCDR)

            modifyIORef' summaryInfoRef (updateSummaryInfo callDateInfo)
            return callDateInfo

   updateSummaryInfo :: (Bool, LocalTime, BS.ByteString) -> SourceDataSummaryInfo -> SourceDataSummaryInfo
   updateSummaryInfo (isOK, callDate, _) (maybeMaxMinCallDate, totLines, linesWithErrors)
     = ( newMaxMinCallDate callDate maybeMaxMinCallDate
       , if isOK then (totLines + 1) else totLines
       , if isOK then linesWithErrors else (linesWithErrors + 1))

   insertCDRS :: V.Vector (Bool, LocalTime, BS.ByteString) -> Cmd
   insertCDRS v
     = let f (_, callDate, nativeCDR) = (callDate, providerId, logicalTypeId, formatTypeId, areImportedServiceCDRS, nativeCDR)
       in  Cmd_insertSourceCDR (V.toList $!! fmap f v)

-- | Export the CDRs in a human processable format.
--   Usually the export is done for fixing errors in the original source CDRs.
rateEngine_exportDataFile
  :: FilePath
  -> CDRProviderName
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> (LocalTime, LocalTime)
  -> Bool
  -- ^ True for using only source cdrs in ar_source_cdr_to_move
  -> DBConf
  -> IO Int

rateEngine_exportDataFile
  outFile
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
  (fromCallDate, toCallDate)
  useOnlySourceCdrsToMove
  dbConf = do

      (CDRFormatSpec !sourceCDRParams !theType)
        <- case getSupportedCDRSImporters logicalTypeName formatTypeName of
             Nothing
               -> throwIO $ AsterisellException $ "There is no known/configured source data file importer for format " ++ (Text.unpack logicalTypeName) ++ ",  and version " ++ (Text.unpack formatTypeName)
             Just r
               -> return r

      case sourceCDRParams of
        SourceCDRParamsCSVFile _ _ _ UseUTF8
          -> return ()
        -- SourceCDRParamsCSVFile _ _ _ locale
        --  -> throwIO $ AsterisellException $ "(err 573) The code can not manage character locale conversions " ++ show locale
        -- NOTE: code disabled because up to date only UseUTF8 is implemented

      safeBracket
        (openDBConnection dbConf False)
        (\maybeExc readConn -> do DB.close readConn)
        (\readConn -> do

           let queryIn1 = "SELECT content FROM ar_source_cdr FORCE INDEX(ar_source_cdr_I_1)"

           let queryIn2 = if useOnlySourceCdrsToMove
                          then " INNER JOIN ar_source_cdr_to_move ON ar_source_cdr.id = ar_source_cdr_to_move.ar_source_cdr_id "
                          else ""

           let queryIn3 = [str| WHERE ar_physical_format_id = ?
                              | AND ar_cdr_provider_id  = ?
                              | AND calldate >= ?
                              | AND calldate < ?
                              | ORDER BY calldate
                              | LIMIT ? OFFSET ?
                              |]

           let query = LBS.concat $ [queryIn1, queryIn2, queryIn3]
           stmtId <- DB.prepareStmt readConn (DB.Query query)

           S.withFileAsOutput outFile $ \outS -> do
             case sourceCDRParams of
               SourceCDRParamsCSVFile (Just header) _ _ UseUTF8
                 -> do S.write (Just $ fromTextToByteString header) outS
                       S.write (Just "\r\n") outS

               SourceCDRParamsCSVFile Nothing _ _ UseUTF8
                 -> do return ()

             processAllSplits readConn stmtId outS param_chunkSize2 0)

 where

   processAllSplits
       :: DB.MySQLConn
       -> StmtID
       -- ^ the statement to use for retrieving the CDRs
       -> S.OutputStream BS.ByteString
       -> Int
       -- ^ the limit (chunk size) to use
       -> Int
       -- ^ the current offeset
       -> IO Int
       -- ^ the wrote CDRS
   processAllSplits conn stmtId outS splitSize currentOffset = do
     let queryParams
           = [toDBInt64 formatTypeId
             ,toDBInt64 providerId
             ,toDBLocalTime fromCallDate
             ,toDBLocalTime toCallDate
             ,toDBInt64 splitSize
             ,toDBInt64 currentOffset]

     (_, inS) <- DB.queryStmt conn stmtId queryParams
     count <- stream_connectWithMapAndFoldM
                (\c1 [sourceCDR] -> return (c1 + 1, [fromDBByteString sourceCDR, "\r\n"]))
                0
                inS
                outS
                False
     case count < splitSize of
       True -> return $ currentOffset + count
       False -> processAllSplits conn stmtId outS splitSize (currentOffset + count)

-- -------------------------------------------------------------
-- Rating of CDRs
--

-- | Used internally for managing in a cached way the search of rates,
--   and the status of the rating process.
data RatingStatus
  = RatingStatus {
      rs_incomeRate :: Maybe (CheckedMainRatePlan, Maybe LocalTime)
      -- ^ the cached result, and until when it is valid (exclusive)
    , rs_costRate :: Maybe (CheckedMainRatePlan, Maybe LocalTime)
      -- ^ the cached result, and until when it is valid (exclusive)
    , rs_incomeBundleState :: Maybe BundleState
    -- ^ Nothing if the rating process is starting from an empty/not initializated database
    , rs_expandedExtensions :: HS.HashSet ExpandedExtension
    -- ^ the found expanded extensions that are not matched by an explicit expanded extension,
    --   but that are matched from a generic extension.
    , rs_errors :: AsterisellErrorsDictionary
    -- ^ prevent the generation of new errrors, if there are similar errors already generated
    , rs_criticalError :: Maybe (AsterisellError, AsterisellErrorDupKey)
    -- ^ Nothing if the rating process can continue, a critical error with the problem key otherwise
    }

instance NFData RatingStatus where
  rnf r = seq (r { rs_incomeBundleState = force (rs_incomeBundleState r) }) ()

instance Show RatingStatus where
  show rs = "<RatingStatus>"

ratingStatus_empty :: RatingStatus
ratingStatus_empty
  = RatingStatus {
       rs_incomeRate = Nothing
     , rs_costRate = Nothing
     , rs_incomeBundleState = Nothing
     , rs_expandedExtensions = HS.empty
     , rs_errors = asterisellErrorsDictionary_empty
     , rs_criticalError = Nothing
    }

-- | A monad returning an error, or returning some result, inside IO.
--   The returned type is an implicit parameter of the type.
--   The error must always of type (CDR, AsterisellError).
--   If the CDR is specified, then the error is related to it.
--   Otherwise if the CDR is Nothing, then it is a critical error, affecting all the rating process,
--   that will be aborted.
type RatingMonad = ExceptT (Maybe CDR, AsterisellError) IO

type YearAndMonth = (Integer, Int)

-- | Rate from (inclusive) to (exclusive)
type MaybeTimeFrame = Maybe (LocalTime, LocalTime)

maybeTimeFrame_larger :: MaybeTimeFrame -> MaybeTimeFrame -> MaybeTimeFrame
maybeTimeFrame_larger Nothing Nothing = Nothing
maybeTimeFrame_larger (Just r1) Nothing = (Just r1)
maybeTimeFrame_larger Nothing (Just r2) = (Just r2)
maybeTimeFrame_larger (Just (x1, y1)) (Just (x2, y2)) = Just (min x1 x2, max y1 y2)
{-# INLINABLE maybeTimeFrame_larger #-}

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
-- Service CDRS imported from provider
-- ===================================
--
-- Service CDRS can derive from bundle rates, or from pure services.
--
-- In this case they are imported from provider.
--
-- Requirements
-- ------------
--
-- * services generated on the provider side are sent to the reseller side
-- * services influence a big rating time-frame, but they are usually stable (also if generated often) and does not influence other calls, so they should not force a rerating of all calls on the reseller side, but only an update of previously imported services
-- * imported services are only costs, and should not start an export of rates from the reseller to the reseller of the reseller
-- * change of services generate a full-rerate-event, and they must be sent to the reseller
-- * the starting of a new time-frame, generate new services and they must be sent to the reseller
-- * support the fact that pure services can have different time-frames respect services associated to bundle rates
--
-- Design
-- ------
--
-- * they are only a cost from a provider
-- * they are rated without affecting the day change status event because:
-- ** if they change, a global rerate event is generated in any case
-- ** I assume (hack) that there is at least one call for day, and not only imported costs
-- ** they are generated often (in a repeated mode) and we will have a lot of wasted resources processing them
-- * export services to resellers grouped by month status file, because
-- ** also if it is not the correct time-frame it is good enough
-- ** there are few services for month, proportional to customers, and they can be exported every time there is a rating event
-- ** the drawbacks are that cost of services in already billed time-frame can change, but it is acceptable
-- * imported services are explicitely known from the reseller, because they are read from files with the appropiate status file, and they have then a special flag in ar_cdr table
-- * imported services are rated easily importing the cost indicated by the provider
-- * there is no double check of the imported cost, so there must be some trust. Also because services are not configurable as costs
-- * the CDR importer signal apart when there is an event for processing imported CDRS
--
--   @require bundleStateFromCallDate <= fromCallDate
--   @ensure write in debugFileName (debugHandle) only if isDebugMode
--
rateEngine_rate
  :: RunLevel
  -> InitialRatingParams
  -> IO Int
  -- ^ the number of processed CDRs.
  --   Normal errors are added directly to the error table, and associated to rated CDRs.
  --   Critical errors are signaled using an exception, because they are critical and they block the entire rating process.

rateEngine_rate runLevel initialParams = do

  env <- ratePlan_loadRatingParams initialParams

  let ratingStatus1
        = ratingStatus_empty { rs_incomeBundleState = params_initialBundleState env }

  let isDebugMode = iparams_isDebugMode initialParams
  let precisionDigits = iparams_currencyPrecision initialParams
  let dbConf = iparams_dbConf initialParams
  let onlyImportedServiceCDRS = iparams_onlyImportedServices initialParams
  let isOfficialRatingEvent = iparams_isRateUnbilledCallsEvent initialParams

  parsedCDRSChan <- newChan (Just 8)
  cdrsToRateChan <- newChan (Just 8)
  cmdsChan <- newChan (Just 8)
  -- NOTE: the DB is by far the slowest process, so use a short channel because:
  -- * this stop sooner other producer processes, and give priority to the DB process
  -- * write operations are already batched in `param_chunkSize`, so a large channel is not so much important
  -- * the uagi channels use a x2 burst of the channel bound sometime

  tot1 <- withAsync
    (case runLevel of
       RunLevelFull -> process_db isDebugMode dbConf precisionDigits Nothing cmdsChan
       _ -> process_fakeDB isDebugMode dbConf precisionDigits Nothing cmdsChan
    )
    (\dbThread -> do

      case onlyImportedServiceCDRS of
        False -> writeChan
                   cmdsChan
                   (V.singleton $ Cmd_deleteCDRS
                      (params_bundleStateFromCallDate env)
                      (params_fromDate env)
                      (params_toDate env)
                      (params_bundleStateCanBeSavedFromCallDate env)
                      (params_bundleStateToCallDate env))

        True -> writeChan
                  cmdsChan
                  (V.singleton $ Cmd_deleteOnlyImportedServiceCDRS
                     (params_fromDate env)
                     (params_toDate env))

      -- Before starting the CDR read process, wait the DB process is ready to accept commands.
      -- Do this because:
      -- * the previous write commands affect many CDRs and they can suspend the DB work for a lot of time
      -- * if we start a read query, there can be a timeout
      waitV <- newEmptyMVar
      writeChan cmdsChan (V.singleton $ Cmd_wait waitV)
      _ <- takeMVar waitV

      finalStateOrError
        <- withAsync
             (process_parseSourceCDRS runLevel env parsedCDRSChan)
             (\parseThread -> do
               (_, r) <- concurrently
                           (case runLevel <= RunLevelUntilPortedTelephoneNumberConversion of
                              True -> process_completePortedTelephoneNumbers env parsedCDRSChan cdrsToRateChan
                              False -> process_fakeCompletePortedTelephoneNumbers env parsedCDRSChan cdrsToRateChan
                           )
                           (case runLevel <= RunLevelUntilRating of
                              True -> process_rate env cdrsToRateChan cmdsChan (0, ratingStatus1, params_bundleStateFromCallDate env)
                              False -> process_fakeRate env cdrsToRateChan cmdsChan (0, ratingStatus1, params_bundleStateFromCallDate env)
                           )

               wait parseThread
               return r)

      case finalStateOrError of
         Left err
           -> do
                 hPutStrLn IO.stderr $! "rateEngine: received rating error " ++ asterisellError_userShow err
                 throwIO $ AsterisellException $ asterisellError_userShow err

         Right (totCDRS, ratingStatus2, lastProcessedCallDate)
           -> do
                 -- Write ExpandedExtensions.
                 -- NOTE: internal extensions are few, and so they can be store all in RAM, and processed as a list.
                 M.mapM_ (writeChan cmdsChan) $ (L.map (\e -> V.singleton $ Cmd_insertExpandedExtension e) (HS.toList $ rs_expandedExtensions ratingStatus2))

                 -- NOTE: this is the last command to call, and it writes the rating status and other info to the DB
                 writeChan
                   cmdsChan
                   (V.singleton $ Cmd_close
                      onlyImportedServiceCDRS
                      (params_fromDate env)
                      (params_bundleStateToCallDate env)
                      ratingStatus2)

                 writeEOFChan cmdsChan

                 wait dbThread
                 return totCDRS)

  case (params_isShorterSafeTimeFrame env) of
    False
      -> return tot1
    True
      -> do -- TODO delete later
            putStrLn $ "Rating event was splitted: " ++ show (fromLocalTimeToMySQLDateTime $ params_fromDate env) ++ " - " ++ show (fromLocalTimeToMySQLDateTime $ params_toDate env) ++ " - " ++ show (fromLocalTimeToMySQLDateTime $ iparams_toDate initialParams)
            let params2 = initialParams { iparams_fromDate = params_toDate env }
            tot2 <- rateEngine_rate runLevel params2
            return $ tot1 + tot2

-- | Read source CDRS from the DB, and parse them.
process_parseSourceCDRS :: RunLevel -> RatingParams -> Chan ParsedCDR -> IO ()
process_parseSourceCDRS runLevel env outChan
  = do
      safeBracket
        (openDBConnection (params_dbConf env) False)
        (\maybeExc inConn
         -> do
               case maybeExc of
                 Just exc
                   -> do hPutStrLn IO.stderr $! "process_parseSourceCDRS exception: " ++ displayException exc
                         return ()
                 _ -> do return ()
               DB.close inConn)
        (\inConn -> do
          -- Send an init CDR to the chain of rating processes. In this way at least one CDR is generated,
          -- and other process can start their initialization work.
          let cdrForProcessInit
                = (cdr_empty bundleStateFromCallDate precisionDigits) {
                     cdr_direction = CDR_ignored
                   , cdr_billsec = Just 0
                   , cdr_duration = Just 0 }
          writeChan outChan (V.singleton $ ParsedCDR ("", bundleStateFromCallDate, 0, "", Right cdrForProcessInit))

          -- Prepare the query
          let inQ1 = [str| SELECT p.internal_name, s.calldate, s.ar_physical_format_id, s.content
                         | FROM ar_source_cdr AS s
                         | INNER JOIN ar_cdr_provider AS p
                         | ON s.ar_cdr_provider_id = p.id
                         | WHERE calldate >= ?
                         | AND calldate < ?
                         |]

          let inQ2 = case onlyImportedServiceCDRS of
                       True
                         -> " AND is_imported_service_cdr = 1 "
                       False
                         ->  " "

          let inQ = LBS.concat [inQ1, inQ2, " ORDER BY s.calldate LIMIT ? OFFSET ?"]
          stmtId <- DB.prepareStmt inConn (DB.Query inQ)
          processAllSplits inConn stmtId param_chunkSize2 0
          writeEOFChan outChan
          return ())

 where

   precisionDigits = params_currencyPrecision env
   bundleStateFromCallDate = params_bundleStateFromCallDate env
   bundleStateToCallDate = params_bundleStateToCallDate env
   onlyImportedServiceCDRS = iparams_onlyImportedServices $ params_initial env
   isOfficialRatingEvent = iparams_isRateUnbilledCallsEvent $ params_initial env
   fastLookupCDRImporters = params_fastLookupCDRImporters env

   processAllSplits
       :: DB.MySQLConn
       -> StmtID
       -- ^ the statement to use for retrieving the CDRs
       -> Int
       -- ^ the limit (chunk size) to use
       -> Int
       -- ^ current offset (start with 0)
       -> IO ()
   processAllSplits conn stmtId splitSize currentOffset = do
          (_, inS1) <- DB.queryStmt conn stmtId [toDBLocalTime bundleStateFromCallDate, toDBLocalTime bundleStateToCallDate, toDBInt64 splitSize, toDBInt64 currentOffset]


          inS2 <- S.map (\[ providerName
                          , callDate
                          , formatId
                          , content]
                            -> ( fromDBText providerName
                               , fromDBLocalTime callDate
                               , fromDBInt formatId
                               , fromDBByteString content))
                        inS1

          inS3 <- S.chunkVector param_chunkSize inS2

          countR <- newIORef 0
          parseChunksAndSend inS3 countR 0 []

          count <- readIORef countR
          case count < splitSize of
            True -> return ()
            False -> processAllSplits conn stmtId splitSize (currentOffset + count)

   parseChunksAndSend :: S.InputStream (V.Vector (CDRProviderName, LocalTime, FormatId, BS.ByteString)) -> IORef Int -> Int -> [ParsedCDR] -> IO ()
   parseChunksAndSend sIn countR bufferedCDRSCount bufferedCDRS = do
     maybeSourceCDRS <- S.read sIn
     case maybeSourceCDRS of
       Nothing
         -> case bufferedCDRSCount > 0 of
              True  -> writeChan outChan (V.fromList $! bufferedCDRS)
              False -> return ()

       Just sourceCDRS
         -> do modifyIORef' countR (\i -> i + (V.length sourceCDRS))
               let cdrs1 = case runLevel <= RunLevelUntilSourceCDRParsing of
                             True -> L.concatMap (parseSourceCDR precisionDigits fastLookupCDRImporters) (V.toList sourceCDRS)
                             -- MAYBE True -> parMap rseq (parseSourceCDR precisionDigits fastLookupCDRImporters) sourceCDRS
                             -- very minimal gain using parallel decoding (probably bigger in case of complex data formats)
                             -- so mantain traditional/safer map based code
                             False -> []

               let cdrs1Count = L.length cdrs1

               case (cdrs1Count + bufferedCDRSCount) > (div param_chunkSize 2) of
                 True
                   -> do writeChan outChan (V.fromList $ bufferedCDRS ++ cdrs1)
                         parseChunksAndSend sIn countR 0 []
                      -- NOTE: the length of the results is not known in advance because cdrs to ignore are removed,
                      -- and other source CDRs can produce more than a final CDR.
                      -- But it is aproximately `param_chunkSize`
                 False
                   -> parseChunksAndSend sIn countR (bufferedCDRSCount + cdrs1Count) (bufferedCDRS ++ cdrs1)

-- | Complete the info about ported telephone numbers.
--   It is useful separating this passage here, in a distinct process, because
--   other rating calculations are instead pure, and so they can be parallilized
--   using pure parallel instructions, and queries to the DB can be chunked.
process_completePortedTelephoneNumbers
    :: RatingParams
    -> Chan ParsedCDR
    -> Chan ParsedCDR
    -> IO ()

process_completePortedTelephoneNumbers env inChan outChan
 = safeBracket
    (openDBConnection (params_dbConf env) True)
    (\maybeExc conn
         -> do
               case maybeExc of
                 Just exc
                   -> do hPutStrLn IO.stderr $! "process_completePortedTelephoneNumbers exception: " ++ displayException exc
                         return ()
                 _ -> do return ()
               _ <- DB.execute_ conn "COMMIT"
               DB.close conn)
    (\conn -> do
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

       processChan
         inChan
         (\cdrs -> do
              let valuesToPort
                    = L.concatMap
                        (\(i, ParsedCDR (_, localTime, _, _, errOrCDR))
                               -> case errOrCDR of
                                   Left err
                                     -> []
                                   Right cdr
                                     -> case cdr_externalTelephoneNumberWithAppliedPortability cdr of
                                          Just _
                                            -> []
                                               -- NOTE: nothing to port because the portability was already applied by the importer
                                          Nothing
                                            -> [[toDBInt64 i, toDBText $ cdr_externalTelephoneNumber cdr, toDBLocalTime localTime]]
                        ) (L.zip [0 ..] (V.toList cdrs))

              _ <- DB.executeStmt conn stmtTruncate []
              _ <- DB.executeMany conn stmtInsertQ valuesToPort

              (_, portedRS) <- DB.queryStmt conn stmtGetPortedTelephoneNumbers []
              portedNumbers1 <- S.map (\[dbId, dbN] -> (fromDBInt dbId, fromDBText dbN)) portedRS
              portedNumbers2 <- S.toList portedNumbers1
              let portedCDRS1
                    = L.map (\(id, portedNumber)
                                 -> let ParsedCDR (n, lt, fid, cdrc, Right cdr1) = ((V.!) cdrs id)
                                        cdr2 = cdr1 { cdr_externalTelephoneNumberWithAppliedPortability = Just portedNumber }
                                    in  (id, ParsedCDR (n, lt, fid, cdrc, Right cdr2))) portedNumbers2


              let cdrs2 = (V.//) cdrs portedCDRS1

              -- assign default telephone numbers for numbers that are not ported
              let cdrs3 = V.map (\unchanged@(ParsedCDR (n, lt, fid, cdrc, mcdr))
                                  -> case mcdr of
                                       Left err
                                         -> unchanged
                                       Right cdr1
                                         -> case cdr_externalTelephoneNumberWithAppliedPortability cdr1 of
                                              Just _
                                                -> unchanged
                                              Nothing
                                                -> let cdr2 = cdr1 { cdr_externalTelephoneNumberWithAppliedPortability = Just $ cdr_externalTelephoneNumber cdr1 }
                                                   in ParsedCDR (n, lt, fid, cdrc, Right cdr2)
                                ) cdrs2

              writeChan outChan cdrs3
         )

       writeEOFChan outChan)

-- | Does nothing: used for testing the speed of process before this.
process_fakeCompletePortedTelephoneNumbers
    :: RatingParams
    -> Chan ParsedCDR
    -> Chan ParsedCDR
    -> IO ()

process_fakeCompletePortedTelephoneNumbers env inChan outChan = do
  processChan inChan (\_ -> return ())
  writeEOFChan outChan
  return ()

type CountOfCDRS = Int

-- | Does nothing: for testing the speed of process before this.
process_fakeRate
    :: RatingParams
    -> Chan ParsedCDR
    -> Chan Cmd
    -> (CountOfCDRS, RatingStatus, CallDate)
        -- ^ Initial state: number of rated CDRS, the last rating status and processed call date
    -> IO (Either AsterisellError (CountOfCDRS, RatingStatus, CallDate))
       -- ^ return an error only in case of critical errors compromizing the entire rating process,
       --   otherwise signal errors on the DB in the usual way.
       --   Return stats terminating the process when a Nothing value is received on the input channel.

process_fakeRate env inChan outChan (c, s, d) = do
  processChan inChan (\_ -> return ())
  writeEOFChan outChan
  return (Right (c, s, d))

-- | Rate CDRS.
process_rate
    :: RatingParams
    -> Chan ParsedCDR
    -- @require number portability is already applied
    -> Chan Cmd
    -> (CountOfCDRS, RatingStatus, CallDate)
        -- ^ Initial state: number of rated CDRS, the last rating status and processed call date
    -> IO (Either AsterisellError (CountOfCDRS, RatingStatus, CallDate))
       -- ^ return an error only in case of critical errors compromizing the entire rating process,
       --   otherwise signal errors on the DB in the usual way.
       --   Return stats terminating the process when a Nothing value is received on the input channel.

process_rate env inChan outChan initialState = do
   r <- runExceptT $ ratePass initialState
   case r of
     Left (_, err) -> return $!! Left $ err { asterisellError_type = Type_Critical }
     -- every error escaping normal processing is critical

     Right rr -> return $ Right $ force rr
     -- return the final computation state

 where

   ratePass :: (CountOfCDRS, RatingStatus, CallDate) -> RatingMonad (CountOfCDRS, RatingStatus, CallDate)
   ratePass (countCDRS, state1, lastCallDate) = do
     cdrsToRate <- liftIO $ readChan inChan
     case isEOFChan cdrsToRate  of
       True
         -> do (count1, state2) <- processCloseRequest (state1, lastCallDate)
               return (countCDRS + count1, state2, lastCallDate)
       False
         -> do
               (!newCountCDRS, !newRatingStatus, !newLastCallDate, !newRatedCDRS)
                 <- V.foldM
                      (\(count2, state2, lastCallDate2, ratedCDRS2) cdrToRate@(ParsedCDR (_, lastCallDate3, _, _, _)) -> do
                          (!count3, !state3, !maybeRatedCDR) <- rateCDR state2 cdrToRate
                          let ratedCDRS3 = case maybeRatedCDR of
                                             Nothing -> ratedCDRS2
                                             Just c -> ratedCDRS2 ++ [c]
                          return (count2 + count3, state3, lastCallDate3, ratedCDRS3))
                      (countCDRS, state1, lastCallDate, [])
                      cdrsToRate

               lift $ writeChan outChan (V.singleton $ Cmd_insertCDR (force $ newRatedCDRS))
               -- the CDRS are insert now, and not on the `rateCDR` side, so they are inserted in one pass in batches.

               ratePass (newCountCDRS, newRatingStatus, newLastCallDate)
               -- continue processing the next part of the input stream

   rateGarbageKey = fromString $ "rating-process"

   currencyPrecision = params_currencyPrecision env
   fromCallDate = params_fromDate env
   toCallDate = params_toDate env
   bundleFromCallDate = params_bundleStateFromCallDate env
   bundleToCallDate = params_bundleStateToCallDate env
   onlyImportedServiceCDRS = params_onlyImportedServices env
   cdrImporters = params_fastLookupCDRImporters env
   isOfficialRatingEvent = params_isRateUnbilledCallsEvent env

   insideRateCallTime' bundleToCallDate cdr = insideRateCallTime'' bundleToCallDate (cdr_calldate cdr)

   insideRateCallTime'' bundleToCallDate callDate = validCallTime False callDate fromCallDate (Just bundleToCallDate)

   -- | Rate a CDR.
   --   Manage errors internally, and return an error only in case of critical errors.
   --   Write directly all updates to rating status, and errors, but postpone the writing of correctly rated CDRs,
   --   because they are collected and updated in chunk from the caller.
   rateCDR
     :: RatingStatus
     -> ParsedCDR
     -> RatingMonad (CountOfCDRS, RatingStatus, Maybe CDR)

   rateCDR state1 (ParsedCDR (providerName, callDate, formatId, sourceCDR, Left err)) = do
     lift $ processError (providerName, callDate, formatId, sourceCDR) (Just $ cdr_empty callDate 4, err)
     -- signal the error, generating a fake CDR to insert in the database for the online stats.
     -- These are the best info we have, because there were some error during parsing of the CDR.
     return (1, state1, Nothing)

   rateCDR state1 (ParsedCDR (providerName, callDateToVerify, formatId, sourceCDR, Right cdr1)) = do
     let cdr2
           = if cdr_isServiceCDR cdr1
             -- NOTE: by definition a service CDR imported from a ar_source_cdr is a imported service CDR,
             -- and it has a direction CDR_system by default.
              then if onlyImportedServiceCDRS
                   then (cdr1 { cdr_isImportedServiceCDR = True, cdr_direction = CDR_system})
                   else (cdr1 { cdr_isImportedServiceCDR = True, cdr_direction = CDR_ignored })
                        -- ignore the CDR because it will be managed during the onlyImportedServiceCDRS phase
             else cdr1

     let cdrTime = cdr_calldate cdr2

     -- Before rating the CDR update the bundle state.
     -- NOTE: a rating error in this phase will be considered a critical error, and it will be returned immediately.
     (countOfCDRS2, state2) <- updateRatingStatusAndRateServiceCDRS state1 CostRate cdr2

     (countOfCDRS3, state3) <- updateRatingStatusAndRateServiceCDRS state2 IncomeRate cdr2

     -- NOTE: an error generated during CDR rating is not propagated as a critical error, but considered a normal error
     (state5, maybeRatedCDR)
       <- catchError (do
            (ratedCDR, state4) <- cdr_rate callDateToVerify env state3 formatId cdr2
            let r = if (insideRateCallTime' bundleToCallDate ratedCDR && cdr_direction ratedCDR /= CDR_ignored)
                    then (Just ratedCDR)
                    else Nothing
            return (state4, r))
            (\err -> do lift $ processError (providerName, callDateToVerify, formatId, sourceCDR) err
                        return (state3, Nothing))

     return $ (countOfCDRS2 + countOfCDRS3 + 1, state5, maybeRatedCDR)

   -- | Update the rating status, and rate the corresponding bundle rates service CDRS.
   --   These are two distinct logical operations, done in one pass because the same input data must be processed.
   --   NOTE: all service-cdrs can be saved on the db without checking their call date, because they were for sure deleted from the db
   --   Return an error if the service CDR can not be initializated correctly.
   --   @require rs_criticalError is not set
   updateRatingStatusAndRateServiceCDRS
     :: RatingStatus
     -> RateRole
     -> CDR
        -- ^ an initializated (but not rated) CDR, used for preparing the rating info
     -> RatingMonad (CountOfCDRS, RatingStatus)

   updateRatingStatusAndRateServiceCDRS cachedState1 rateRole cdr
     = do    let cdrTime = cdr_calldate cdr

             let rateRefName
                  = case rateRole of
                      IncomeRate -> "main-income-rate"
                      CostRate -> "main-cost-rate"

             let (maybeCachedRate, maybeBundleState)
                  = case rateRole of
                      IncomeRate
                        -> (rs_incomeRate cachedState1, rs_incomeBundleState cachedState1)
                      CostRate
                        -> (rs_costRate cachedState1, Just bundleState_empty)

             let (ratePlan0 :: Maybe CheckedMainRatePlan, validUntil0 :: Maybe LocalTime)
                    = case maybeCachedRate of
                          Just (ratePlan, maybeCallDate)
                            -> case maybeCallDate of
                                 Nothing
                                   -> (Just ratePlan, Nothing)
                                 Just d
                                   -> case cdrTime < d of
                                        True -> (Just ratePlan, Just d)
                                        False -> (Nothing, Nothing)
                          Nothing -> (Nothing, Nothing)

             (ratePlan :: CheckedMainRatePlan, validUntil :: Maybe LocalTime)
               <- case ratePlan0 of
                     Just r
                       -> return (r, validUntil0)
                     Nothing
                       -> case env_getRate env rateRefName cdrTime of
                               Nothing
                                 -> throwError
                                      (Nothing
                                      , createError
                                                   Type_Critical
                                                   Domain_RATES
                                                   ("unknown referenced rate - " ++ Text.unpack rateRefName)
                                                   ("Unknown rate with name \"" ++ Text.unpack rateRefName ++ "\", of type " ++ show rateRole ++ ", at date " ++ showLocalTime cdrTime)
                                                   ("All CDRs from the specified calldate, will be not rated. The call report stats will signal the unrated calls.")
                                                   ("Add a rate specification, or change the starting/ending dates of current rate specifications.")
                                      )

                               Just (ratePlan, validDate)
                                 -> let tempBundleStateOnlyForInit
                                          = case maybeBundleState of
                                              Nothing -> bundleState_empty
                                              Just r -> r

                                    in case mainRatePlan_assignSharedUniqueSystemId ratePlan tempBundleStateOnlyForInit of
                                         Left err
                                           -> throwError
                                                ( Nothing
                                                , createError
                                                             Type_Critical
                                                             Domain_RATES
                                                             ("error in redefinition of rates - " ++ show rateRole ++ " at date " ++ showLocalTime cdrTime)
                                                             ("The " ++ show rateRole ++ " rate, with name \"" ++ Text.unpack rateRefName ++ "\", defined for a CDR at date " ++ showLocalTime cdrTime ++ ", can not replace correctly pending bundle rates, defined previously. " ++ err)
                                                             ("All the CDRs from this date, will be not rated. The call report stats will contain the correct totals of all CDRs with errors/not-rated.")
                                                             ("Correct the rate specification.")
                                                )

                                         Right checkedRatePlan
                                           -> return (checkedRatePlan, validDate)

             case rateRole of
               IncomeRate
                -> do let bundleState1 :: BundleState
                            = case maybeBundleState of
                                Just r
                                  -> r
                                Nothing
                                  -> case (params_onlyImportedServices env) of
                                       True
                                         -> bundleState_empty
                                       False
                                         -> bundleState_initAccordingRatingParams env ratePlan (Left bundleFromCallDate) bundleState_empty
                                            -- initialize for the first time the bundle state according the rate plan.
                                            -- NOTE: I'm using bundleStateFromCallDate because it will be updated later, according the CDRs before the strict rate from date

                      let (maybePreviousBundleStateCallDate, bundleState2 :: BundleState, serviceCDRS1 :: [ServiceCDR])
                             = case onlyImportedServiceCDRS of
                                 True  -> (Nothing, bundleState1, [])
                                 False -> (bundleState_serviceCdrs env ratePlan bundleState1 cdrTime)

                      let serviceCDRS2 = L.filter (\cdr -> let td = (fromJust1 "753744" $ cdr_toCalldate cdr)
                                                           in  td > bundleFromCallDate && td <= bundleToCallDate
                                                               -- NOTE: this time-frame is kept in synchro with the time-frame of deleted bundle-rate services
                                                               -- for Cmd_deleteCDRS
                                                  ) serviceCDRS1 

                      let cachedState2
                            = cachedState1 { rs_incomeRate = Just (ratePlan, validUntil)
                                           , rs_incomeBundleState = Just bundleState2 }

                      (serviceCount, cachedState3) <- rateAndSendServiceCDRS (0, cachedState2) serviceCDRS2

                      -- Write bundle state
                      case maybePreviousBundleStateCallDate of
                               Nothing
                                 -> return ()
                               Just previousBundleStateCallDate
                                 -> case previousBundleStateCallDate >= bundleFromCallDate of
                                      True
                                        -> lift $ writeChan outChan (V.singleton $ Cmd_saveBundleState previousBundleStateCallDate bundleState2)
                                           -- in this case the bundle state time frame were closed, and so we are saving it on disk,
                                           -- because the changes of time-frames can be important, and they are not soo much.
                                      False
                                        -> return ()

                      return (serviceCount, cachedState3)

               CostRate
                 -> return (0, cachedState1 { rs_costRate = Just (ratePlan, validUntil) })

   -- | Complete error information and send to the database.
   processError
     :: (CDRProviderName, LocalTime, FormatId, BS.ByteString)
     -> (Maybe CDR, AsterisellError)
     -> IO ()

   processError (providerName, callDate, formatId, sourceCDR) (maybeCDR, err)
     = let
           cdrDetails
             = describeSourceCDRForErrorReporting cdrImporters (providerName, formatId, sourceCDR)

           errDescription
             = asterisellError_description err ++ cdrDetails

           err2
             = err { asterisellError_description = errDescription
                   , asterisellError_effect = "This CDR will be not rated. " ++ asterisellError_effect err
                   , asterisellError_proposedSolution = asterisellError_proposedSolution err ++ "If there are many errors of this type, there can be a problem in the input format specification or in the application code. In case contact the assistance. "
                   }

           toCallDate
             = case maybeCDR of
                 Nothing -> callDate
                 Just cdr -> case cdr_toCalldate cdr of
                               Nothing -> callDate
                               Just r -> r

       in case (asterisellError_type err == Type_Critical) || insideRateCallTime'' bundleToCallDate callDate of
                   False
                     -> do return ()
                   True
                     -> do writeChan outChan (V.singleton $ Cmd_insertCDRWithError err2 (Just (rateGarbageKey, callDate, toCallDate)) maybeCDR)
                           return ()

   bundleStateOrEmpty :: Maybe BundleState -> BundleState
   bundleStateOrEmpty Nothing = bundleState_empty
   bundleStateOrEmpty (Just b) = b

   processCloseRequest :: (RatingStatus, CallDate) -> RatingMonad (CountOfCDRS, RatingStatus)
   processCloseRequest (state1, lastCallDate)
     = do -- Save the incremental BundleState. This value is associated to the last processed CDR, so there are no new CDRS in the DB
          -- after this date (according the logical time-frame). If new CDRS will be added, they will have (probably) a bigger calldate,
          -- and so the rating can start from this saved BundleState.
          -- I can not use for this porpouse bundleStateToCallDate because it is date far in future. In reality I'm not interested to it.
          when (not onlyImportedServiceCDRS)
               (case rs_incomeBundleState state1 of
                   Nothing
                     -> return ()
                   Just bundleState1
                     -> lift $ writeChan outChan (V.singleton $ Cmd_saveBundleState lastCallDate bundleState1))

          -- Generate BundleRate serviceCDRS at bundleStateToCallDate.
          -- I can generate always these CDRs, because they are deleted before rating them.
          -- Doing so I'm generating pending service-cdrs for not already closed time-frame,
          -- or if I'm rerating an already closed time-frame, I'm updating with new values.
          (count1, state2)
            <- case onlyImportedServiceCDRS of
                 True -> return (0, state1)
                 False -> do case (rs_incomeBundleState state1, rs_incomeRate state1) of
                               (Just bundleState2, Just (mainRatePlan2, _))
                                 -> let (_, _, pendingServiceCDRS1) = bundleState_serviceCdrs env mainRatePlan2 bundleState2 bundleToCallDate
                                        pendingServiceCDRS2 = L.filter (\cdr -> let td = (fromJust1 "175377" $ cdr_toCalldate cdr)
                                                                                in  td > bundleFromCallDate && td <= bundleToCallDate
                                                              -- NOTE: this condition must be kept in synchro with Cmd_deleteCDRS deleted CDRS
                                                                       ) pendingServiceCDRS1
                                    in  rateAndSendServiceCDRS (0, state1) pendingServiceCDRS2
                               _ -> return (0, state1)

          -- Generate serviceCDRS for pure services (not relate to bundle rates), in the rating time frame.
          (count2, state3)
            <- case (onlyImportedServiceCDRS) of
                 True  -> do return (0, state2)
                 False -> do case service_generate env fromCallDate toCallDate True of
                              Left errMsg
                                -> throwError $ (Just $ cdr_empty fromCallDate 4
                                                ,createError
                                                   Type_Critical
                                                   Domain_RATES
                                                   errMsg
                                                   ("Error in specification of Services. " ++ errMsg)
                                                   ("Corresponding ServiceCDRs will be not produced, and inserted in call report. The call report does not contain stats about these missing service CDRs.")
                                                   ("Correct the error in Service specification, and rerate them."))

                              Right pureServices
                                -> rateAndSendServiceCDRS (0, state2) pureServices
          return (count1 + count2, state3)

   rateAndSendServiceCDRS :: (CountOfCDRS, RatingStatus) -> [ServiceCDR] -> RatingMonad (CountOfCDRS, RatingStatus)
   rateAndSendServiceCDRS (count0, state0) services = do
     ((!lastCount, !lastStatus), !lastRatedServices)
         <- M.foldM
              (\((count1, state1), ss) s1
                  -> do (s2, _) <- cdr_initialClassification env s1
                        return ((count1 + 1, state1), ss ++ [s2]))
              ((count0, state0), [])
              services

     when (not (L.null lastRatedServices)) (lift $ writeChan outChan (V.singleton $! Cmd_insertCDR lastRatedServices))
     return (lastCount, lastStatus)

-- | Rate a CDR or return an error.
cdr_rate :: LocalTime -> RatingParams -> RatingStatus -> Int -> CDR -> RatingMonad (CDR, RatingStatus)
cdr_rate callDateToVerify env state1 formatId cdr
  = do let isDebugMode = params_isDebugMode env
       let isVoipReseller = iparams_isVoipReseller $ params_initial env
       (cdr1, isExtensionalMatch) <- cdr_initialClassification env cdr
       when (cdr_direction cdr1 == CDR_error)
            (throwError (Just cdr1
                        , createError
                                 Type_Critical
                                 Domain_RATES
                                 "unexpected error code 1075375"
                                 "Unexpected error in the code. Contact the assistance signaling ERR CODE 17253"
                                 ""
                                 ""))
       when (not (cdr_calldate cdr1 == callDateToVerify))
            (let err = (createError
                          Type_Error
                          Domain_RATES
                          ("changed rating classification code")
                          ("This CDR was first imported, using the call date " ++ fromLocalTimeToMySQLDateTime callDateToVerify ++ ", but now during rating phase, the parsed calldate is " ++ fromLocalTimeToMySQLDateTime (cdr_calldate cdr1) ++ ". Probably the code interpreting the CDR format was changed in the past, and now during rerating there is some mismatch.")
                          ("All CDRs of this type will be not rated, because the database content is potentially corrupted.")
                          ("Extract calls at the specified calldate (the first one of the list), and of the same provider/type/version, and resubmit them. They will be deleted from the database, and resubmitted with the correct call date (the second one of the list).")
                       )

             in throwError (Just $ cdr1 { cdr_calldate = min callDateToVerify (cdr_calldate cdr1)}, err))

       let cdrTime = cdr_calldate cdr1

       (state3, cdr2)
         <- case cdr_direction cdr1 == CDR_ignored || (isNothing $ cdr_organizationUnitId cdr1) of
              True ->  return $ (state1, cdr1)
              False -> let
                           -- manage ExpandedExtensions
                           state2
                             = case isExtensionalMatch of
                                 True -> state1
                                 False -> let ee = ExpandedExtension {
                                                     ee_organizationId = fromJust1 "p557" $ cdr_organizationUnitId cdr1
                                                   , ee_specificExtensionCode = fromTextToMySQLResult $ cdr_internalTelephoneNumber cdr1
                                                   }
                                          in state1 { rs_expandedExtensions = HS.insert ee (rs_expandedExtensions state1) }

                       in do r <- mainRate_apply isDebugMode isVoipReseller env state2 cdr1
                             return $ r

       return $ (cdr2, state3)


-- | Apply initial classification to the CDR, completing the fields that are not completed from
--   the initial custom CDR importer, or complete fields of ServiceCDR.
--
--   Exclude from this initialization the ported telephone number, and the telephone prefix,
--   because they are done in a next phase requiring access to the main Asterisell database.
--
--   The CDR error fields are setted from the caller, in case of errors.
cdr_initialClassification :: RatingParams -> CDR -> RatingMonad (CDR, IsExtensionalMatch)
cdr_initialClassification env cdr1
  =
      case (cdr_direction cdr1 == CDR_error || cdr_direction cdr1 == CDR_ignored) of
        True
          -> return (cdr1, True)
             -- nothing to do, because because the problem was already signaled
             -- from the module generating this CDR.
        False
          -> case runStateT pass1 cdr1 of
               Left err
                 -> throwError err
               Right (isExtensionalMatch, cdr2)
                 -> case execStateT pass2 cdr2 of
                      Left err -> throwError err
                      Right cdr4 -> return (cdr4, isExtensionalMatch)

 where

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
                      -> case channelDomains_match channelDomains channelName cdrTime of
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
                       -> do let accountCode = Text.unpack $ cdr_internalTelephoneNumber cdr1
                             when (L.length accountCode == 0)
                                  (throwError $ createRateError cdr1 ("unknown ArAsteriskAccount NULL")
                                                                     ("Asterisk VoIP code can not be empty.")
                                                                     ("All these CDRs with empty VoIP code will not be rated.")
                                                                     ("This is an error in the CDR format. Contact the assistance.")
                                  )

                             (unitInfo, isExtensionalMatch)
                               <- case info_getDataInfoForExtensionCode organizationsInfo accountCode cdrTime of
                                    Right Nothing
                                      -> throwError $ createRateError
                                                        cdr1
                                                        ("unknown accountcode - " ++ accountCode)
                                                        ("\"" ++ accountCode ++ "\" VoIP account code is not associated to an extension at call-date " ++ showLocalTime cdrTime)
                                                        ("All CDRs with this VoIP code will not rated.")
                                                        ("Complete the VoIP account codes.")
                                    Left err
                                      -> throwError (Just cdr1, err)
                                    Right (Just r)
                                      -> return r

                             return $ (unit_id unitInfo, unitInfo, isExtensionalMatch)

          let dataInfoParents = info_getDataInfoParentHierarchy organizationsInfo unitInfo cdrTime False
          let parentIds = info_getParentIdHierarchy dataInfoParents
          let parentIdsText = info_getParentHiearchyIds dataInfoParents
          let maybeBillableDataInfo = info_getBillableDataInfo dataInfoParents
          let maybeRateCategoryId = info_getRateCategoryId dataInfoParents
          let organizationName = info_getFullName dataInfoParents 0 False False False

          when (isNothing maybeBillableDataInfo)
               (throwError $ createRateError cdr1 ("unknown billable organization for " ++ Text.unpack parentIdsText)
                                                  ("Organization unit with id " ++ show unitId ++ ", and name " ++ organizationName ++ " has not a billable parent.")
                                                  ("These CDRs can not be assigned to a billable organization, and they can not be billed correctly.")
                                                  ("Define a billable organization in the organization hierarchy.")
               )

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
                  case trie_getMatch trie_getMatch_initial telephonePrefixes ported of
                    Just (_, prefixId)
                      -> do modify (\c -> c { cdr_telephonePrefixId = Just $ prefixId })
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
    = (Just cdr
      , createError Type_Error Domain_RATES key descr effect solution)

-- | Use the main rate plan for rating a CDR: cost and income.
mainRate_apply :: Bool -> Bool -> RatingParams -> RatingStatus -> CDR -> RatingMonad (RatingStatus, CDR)
mainRate_apply isDebugMode isVoipReseller env state1 cdr1
  = case isVoipReseller of
      True -> do (state2, cdr2) <- searchRateAndApply state1 CostRate cdr1
                 state2 `deepseq` (searchRateAndApply state2 IncomeRate cdr2)
      False -> do (state2, cdr2) <- searchRateAndApply state1 IncomeRate cdr1
                  -- in case of call reporting the cost and income are the same
                  let cdr3 = cdr2 { cdr_cost = cdr_income cdr2
                                   , cdr_debug_cost_rate = cdr_debug_income_rate cdr2
                                   }
                  return (state2, cdr3)

 where

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

  bundleDebugInfo state1 Nothing _ = (Nothing, Nothing, Nothing)

  bundleDebugInfo state1 _ Nothing = (Nothing, Nothing, Nothing)

  bundleDebugInfo state1 (Just rateId) (Just unitId)
     = case IMap.lookup rateId state1 of
         Nothing
           -> (Nothing, Nothing, Nothing)
         Just state2@(_, _, map2)
           -> case IMap.lookup unitId map2 of
                Nothing
                  -> (Nothing, Nothing, Nothing)
                Just [(params, _)]
                  -> (  bundle_leftCalls params
                      , bundle_leftDuration params
                      , Just $ max 0 ((bundle_minCost params) - (bundle_appliedCost params))
                     )


  cdrTime = cdr_calldate cdr1

  -- | Rate the CDR calculating cost or income.
  --   @require RatingStatus is correctly initializated.
  --   @require RatingStatus has no critical error.
  searchRateAndApply :: RatingStatus -> RateRole -> CDR -> RatingMonad (RatingStatus, CDR)
  searchRateAndApply cachedState1 rateRole cdr
    = do let (ratePlan :: CheckedMainRatePlan, bundleState1 :: BundleState)
               = case rateRole of
                   IncomeRate
                     -> (fst $ fromJust1 "c1" $ rs_incomeRate cachedState1, fromJust1 "c2" $ rs_incomeBundleState cachedState1)
                   CostRate
                     -> (fst $ fromJust1 ("c3: on CDR " ++ cdr_showDebug cdr) $ rs_costRate cachedState1, bundleState_empty)

         (value, bundleRateUnitId, bundleState2, debugInfo)
            <- case mainRatePlan_calcCost isDebugMode rateRole env bundleState1 ratePlan cdr of
                 Right r
                   -> return r
                 Left err
                   -> throwError (Just cdr, err)

         let debugRatingDetails1
               = case info_ratingDetails debugInfo of
                   Nothing -> Nothing
                   Just s -> let rateRoleName
                                   = case rateRole of
                                       IncomeRate -> "\nDetails during calculation of Income:\n"
                                       CostRate -> "\nDetails during calculation of Cost:\n"
                             in Just $ Text.append rateRoleName s

         let debugRatingDetails2
               = case cdr_debug_rating_details cdr of
                   Nothing
                     -> debugRatingDetails1
                   Just s1
                     -> case debugRatingDetails1 of
                          Nothing
                            -> Nothing
                          Just s2
                            -> Just $ Text.concat [s1, Text.pack "\n", s2]

         appliedRateName
           <- case IMap.lookup (info_appliedRate debugInfo) (mainRatePlan_systemIdToUserIdPath ratePlan) of
                   Just r -> return r
                   Nothing -> let err = createError
                                          Type_Error
                                          Domain_APPLICATION
                                          ("unknown rate with name " ++ (show $ info_appliedRate debugInfo))
                                          ("Unexpected error: the rate with code " ++ show (info_appliedRate debugInfo) ++ " is unknown on rate plan " ++ show ratePlan ++ " during rating of \n\n" ++ cdr_showDebug cdr ++ "\n\n")
                                          "This and similar CDRs will be not rated."
                                          "Contact the assistance because this is an error in the code."
                              in throwError (Just cdr, err)
                                 -- DEV-NOTE: before in this point I were using an "error ..." instruction, but it generates a not clear message about conflicts between threads, and not the real message.

         let residualAppliedRate
               = case info_residualAppliedRate debugInfo of
                   Nothing
                     -> Nothing
                   Just i
                     -> IMap.lookup i (mainRatePlan_systemIdToUserIdPath ratePlan)

         case rateRole of
           IncomeRate
             -> let (leftCalls, leftDuration, leftCost)
                      = bundleDebugInfo bundleState2 (info_bundleRateSystemId debugInfo) bundleRateUnitId



                in return $  (   cachedState1 { rs_incomeBundleState = Just bundleState2 }
                               , cdr { cdr_income = value
                                     , cdr_bundleOrganizationUnitId = bundleRateUnitId
                                     , cdr_debug_income_rate = Just appliedRateName
                                     , cdr_debug_residual_income_rate = residualAppliedRate
                                     , cdr_debug_residual_call_duration = info_residualCallDuration debugInfo
                                     , cdr_debug_bundle_left_calls = leftCalls
                                     , cdr_debug_bundle_left_duration = leftDuration
                                     , cdr_debug_bundle_left_cost = leftCost
                                     , cdr_debug_rating_details = debugRatingDetails2
                                     }
                               )

           CostRate
             -> return $ (   cachedState1
                           , cdr { cdr_cost = value
                                 , cdr_debug_cost_rate = Just appliedRateName
                                 , cdr_debug_rating_details = debugRatingDetails2
                                 }
                           )

-- ---------------------------------------------------------------
-- Utils
--

fromLazyByteStringToString :: LBS.ByteString -> String
fromLazyByteStringToString s = LText.unpack $ LText.decodeUtf8 s

-- | Extract Days, and YearAndMonth from a TimeFrame
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
