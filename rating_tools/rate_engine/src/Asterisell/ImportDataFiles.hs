{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Import CDRS from external files or external databases inside `ar_source_cdr` table.
--   Signal only problems about callDate, because all other problems will be reported
--   during rating from `ar_source_cdr` to `ar_cdr`.
--
module Asterisell.ImportDataFiles (
  SourceDataSummaryInfo,
  rateEngine_importDataFile,
  rateEngine_importFromMySQLDB,
  rateEngine_importIntoDB,
  rateEngine_exportDataFile,
  db_updateRatingTimeFrame,
) where

import Asterisell.DB
import Asterisell.Cdr
import Asterisell.Error
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.CustomerSpecificImporters

import qualified Control.Monad as M
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS
import Data.Time.LocalTime
import qualified Data.Text as Text
import Data.List as L
import Database.MySQL.Base as DB
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)

import Control.Exception.Base (ErrorCall(..))
import System.IO.Streams as S
import System.IO.Streams.Vector as SV
import qualified Data.Vector as V
import Control.DeepSeq as DeepSeq
import Control.Concurrent.MVar
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc
import qualified Data.Csv as CSV
import Control.Concurrent.Async
import System.IO
import Data.Either
import Data.Traversable
import Data.Maybe

-- -------------------------------------------
-- Common data types

-- | Meta info about a SourceDataFile.
--   At this stage only the calldate is imported and processed.
type SourceDataSummaryInfo
  = ( Maybe (LocalTime, LocalTime)
      -- ^ max and minimum found call date
    , Int
      -- ^ correctly parsed lines
    , Int
      -- ^ lines with errors
    )

type RemoteCDRId = Int

{-# INLINE newMaxMinCallDate #-}
newMaxMinCallDate :: LocalTime -> Maybe (LocalTime, LocalTime) -> Maybe (LocalTime, LocalTime)
newMaxMinCallDate d Nothing = Just (d, d)
newMaxMinCallDate d (Just (minDD, maxDD)) = Just (min minDD d, max maxDD d)

-- -------------------------------------------------------------------
-- File as source of CDRS

-- | Parse a file and import into database as ar_source_cdr records, returning summary info on its content.
--
rateEngine_importDataFile
  :: FilePath
  -> Maybe FilePath
  -> CDRProviderName
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> CallDate
  -- ^ import from this date
  -> Bool
  -> Maybe (LocalTime, LocalTime)
  -> DBConf
  -> CurrencyPrecisionDigits
  -> IO SourceDataSummaryInfo

rateEngine_importDataFile
  inputFile
  maybeDebugFileName
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
  fromCallDate
  isStatusFile
  maybeStatusTimeFrame
  dbConf
  precision = do

    (CDRFormatSpec !params !theType)
      <- case getSupportedCDRSImporters logicalTypeName formatTypeName of
           Nothing
             -> throwIO $ AsterisellException ("There is no known/configured source data file importer for format " ++ (Text.unpack logicalTypeName) ++ ", and version " ++  (Text.unpack formatTypeName))
           Just r
             -> return r

    withResource'
      (return ())
      (\_ -> S.withFileAsInput
               inputFile
               (\fileContentStream -> do
                   rawCDRS <-
                       (allLines params fileContentStream)
                         >>= (maybeRemoveHeader params)
                         >>= (utf8Lines params)
                         >>= (S.map (\l -> (0, l)))
                         >>= SV.chunkVector param_chunkSize

                   rawCDRSChan <- newEmptyMVar

                   writeJob <- async $
                     case maybeDebugFileName of
                       Nothing -> do
                         rateEngine_importIntoDB
                           providerName
                           providerId
                           logicalTypeName
                           logicalTypeId
                           formatTypeName
                           formatTypeId
                           fromCallDate
                           isStatusFile
                           maybeStatusTimeFrame
                           dbConf
                           precision
                           rawCDRSChan
                           (\ conn -> void $ DB.execute conn  "REPLACE INTO ar_local_file_to_delete(name) VALUES(?)" [toDBText $ Text.pack inputFile])
                       Just debugFileName -> do
                         rateEngine_debugCDRParsing
                           providerName
                           providerId
                           logicalTypeName
                           logicalTypeId
                           formatTypeName
                           formatTypeId
                           dbConf
                           precision
                           rawCDRSChan
                           debugFileName

                   _ <- stream_toOrderedStream True rawCDRS rawCDRSChan Nothing
                   wait writeJob
               ))

      (\_ _ -> return ())
      createImportError

  where

   utf8Lines params stream1
       = case params of
           SourceCDRParamsCSVFile _ _ _ UseUTF8
             -> return stream1
           SourceCDRParamsDBTableWithAutoincrementId _ _ _
             -> return stream1
           _   -> throwIO $ ErrorCallWithLocation ("The data format " ++ show params ++ " is not supported. Contact the assistance. ") ""

   maybeRemoveHeader params stream1
     = case params of
         SourceCDRParamsCSVFile (Just _) _ _ _
           -> S.drop 1 stream1
         _ -> return stream1

   allLines :: SourceCDRParams -> InputStream BS.ByteString -> IO (InputStream RawSourceCDR)
   allLines params stream1
     = case params of
         SourceCDRParamsCSVFile _ _ False _
           -> do stream2 <- S.splitOn (\c -> c == '\n' || c == '\r') stream1
                 stream3 <- S.filter (\s -> not $ BS.null s) stream2
                 return stream3
         _ -> pError "Parsing of CSV files with new lines inside quoted fields is not yet supported by the application. Contact the assistance. "

   createImportError e
     = SomeException $ AsterisellException $ "There is an error during the importing of file \"" ++ inputFile ++ "\". " ++ (displayException e)

-- --------------------------------------------
-- MySQL database as external source of CDRS

-- | Read CDRS from an external MySQL database, in an incremental way.
--   Assumes that there is an auto incrementing field: every new added CDR will have
--   an ID grater than previous CDRS.
--
--   It uses auto_increment IDS for recognizing new inserted CDRS,
--   and the algo is safe (no data loss, also in case of run-time exceptions)
--   also without using transactions on the remote database, that can be not supported
--   in case of MyISAM tables.
--   The code is transaction safe because a transaction is open on the destination
--   local database, and the job will be repeated in case of problems.
--   The reader does not need transactions because the source DB table is assumed append-only
--   and the ID is auto-increment.
--
--   ID is a good field to use because there is for sure an index on it,
--   and it is likely that the call-date is more or less ordered about it.
--
--   Relatively small chunks of CDRS are used because Haskell MySQL driver does not support
--   streaming, but only complete query materialization.
--
rateEngine_importFromMySQLDB
  :: DB.ConnectInfo  -- ^ remote source DB
  -> CDRProviderName
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> String
  -> CallDate  -- ^ import only CDRS >= this date
  -> DBConf    -- ^ local destination DB
  -> IO SourceDataSummaryInfo

rateEngine_importFromMySQLDB
  remoteDBConf
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
  remoteTableName
  fromDate
  dbConf = do

    (CDRFormatSpec !params !theType)
      <- case getSupportedCDRSImporters logicalTypeName formatTypeName of
           Nothing
             -> throwIO $ AsterisellException ("There is no known/configured source data file importer for format " ++ (Text.unpack logicalTypeName) ++ ", and version " ++  (Text.unpack formatTypeName))
           Just r
             -> return r

    (fields, fieldIdName, loadQuery)
      <- case params of
           SourceCDRParamsDBTableWithAutoincrementId fields _ Nothing -> do
             let fieldId = head fields
             let loadQuery1 =
                   "SELECT "
                       ++ (L.concat $ L.intersperse ", " fields)
                       ++ " FROM " ++ remoteTableName
                       ++ " WHERE " ++ fieldId ++ " > ? AND " ++ fieldId ++ " <= ? "
                       ++ " ORDER BY " ++ fieldId
                       ++ " LIMIT ?"
             return (fields, fieldId, loadQuery1)

           SourceCDRParamsDBTableWithAutoincrementId fields _ (Just loadQuer1) -> do
             let fieldId = head fields
             let loadQuery2 =
                   loadQuer1 ++ " AND " ++ fieldId ++ " > ? AND " ++ fieldId ++ " <= ? "
                     ++ " ORDER BY " ++ fieldId ++ " LIMIT ?"
             return (fields, fieldId, loadQuery2)

           _ -> throwIO $ AsterisellException ("The format " ++ (Text.unpack logicalTypeName) ++ ", and version " ++ (Text.unpack formatTypeName) ++ " is not associated to a remote database format.")

    conn <- db_openConnection dbConf False
    let inQ0 = "SELECT last_imported_id FROM ar_cdr_provider WHERE id = ?"
    (_, inS0) <- DB.query conn (DB.Query $ LBS.fromStrict $ fromStringToByteString inQ0) [toDBInt64 providerId]
    rs0 <- S.toList inS0
    let lastSavedId
          = case rs0 of
              [] -> 0
              [[DB.MySQLNull]] -> 0
              [[v]] -> fromDBInt v
              _ -> pError "ERR 5203: unexpeted result"
    DB.close conn

    withResource'
      (DB.connect remoteDBConf)
      (\remoteConn -> do

          -- Find the last remote fieldIdName.
          -- NOTE: use this value as limit, so we are sure that also if new remote CDRS are added,
          -- the retrieving process will terminate. New added CDRS will be retrieved in next execution
          -- pass of the rating process.
          DB.execute_ remoteConn "SET autocommit = 0"
          DB.execute_ remoteConn "SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED"
          let remoteQ0 =
                "SELECT " ++ fieldIdName ++ " FROM " ++ remoteTableName ++ " ORDER BY " ++ fieldIdName ++ " DESC LIMIT 1"
          (_, remoteS0) <- DB.query_ remoteConn (DB.Query $ LBS.fromStrict $ fromStringToByteString remoteQ0)
          remoteRS0 <- S.toList remoteS0
          let lastRemoteId
                = case remoteRS0 of
                    [] -> 0
                    [[DB.MySQLNull]] -> 0
                    [[v]] -> fromDBInt v
                    _ -> pError "ERR 5204: unexpeted result"

          -- Prepare query for retrieving remote source CDRS
          DB.execute_ remoteConn "SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED"
          -- NOTE: the remoteDB is only append-only so this poses no problem, and we are sure there will be no lock
          -- on remote-tables

          readStmt <- DB.prepareStmt remoteConn (DB.Query $ LBS.fromStrict $ fromStringToByteString loadQuery)

          outChan <- newEmptyMVar
          job1 <- async $ readAllRawCDRS remoteConn readStmt outChan lastRemoteId lastSavedId  

          job2 <-
            async $
              rateEngine_importIntoDB
                 providerName
                 providerId
                 logicalTypeName
                 logicalTypeId
                 formatTypeName
                 formatTypeId
                 fromDate
                 False
                 Nothing
                 dbConf
                 4
                 outChan
                 (\ conn -> do 
                      _ <- DB.execute conn  "UPDATE ar_cdr_provider SET last_imported_id = ? WHERE id = ?" [toDBInt64 lastRemoteId, toDBInt64 providerId]
                      return ())

          _ <- wait job1
          wait job2)
      (\_ remoteConn -> DB.close remoteConn)
      (createImportError)

  where

   readAllRawCDRS remoteConn readStmt outChan  maxRemoteId lastRemoteId = do
       (_, stream1) <- DB.queryStmt remoteConn readStmt [toDBInt64 lastRemoteId, toDBInt64 maxRemoteId, toDBInt64 $ param_chunkSize2]

       stream2 <- S.map toRawSourceCDR stream1 >>= SV.chunkVector param_chunkSize
       maybeLastChunk <- stream_toOrderedStream False stream2 outChan Nothing
       case maybeLastChunk of
         Nothing -> do
           orderedStream_put outChan Nothing
           return lastRemoteId
         Just lastChunk -> do
           let !lastRemoteId' = fst lastChunk 
           readAllRawCDRS remoteConn readStmt outChan maxRemoteId lastRemoteId'

   -- | Convert to a CSV record, removing the last "\n" character.
   toRawSourceCDR :: [DB.MySQLValue] -> (RemoteCDRId, RawSourceCDR)
   toRawSourceCDR cdr1 = (fromDBInt $ L.head cdr1,  BS.init $ LBS.toStrict $ CSV.encodeWith opts [cdr1])
   {-# INLINE toRawSourceCDR #-}

   !opts = CSV.defaultEncodeOptions {
             CSV.encDelimiter     = 44  -- comma
           , CSV.encUseCrLf       = False
           , CSV.encIncludeHeader = False
           , CSV.encQuoting       = CSV.QuoteMinimal
           }
   {-# INLINE opts #-}

   createImportError e
     = SomeException $
         AsterisellException $
           "There is an error during CDRS importing from remote MySQL database of the provider " ++ show providerName ++ ". " ++ (displayException e)

-- ----------------------------
-- Write data to the DB

-- | Insert a stream of source CDRS inside `ar_source_cdr`.
--
--   Report only serious errors, because all errors specific of CDRs will be reported during rating phase.
--
--   CDRs without a clear calldate, will be imported using the previous calldate, or the calldate of today.
--
rateEngine_importIntoDB
  :: CDRProviderName
  -> Int             -- ^ ProviderId
  -> Text.Text       -- ^ LogicalTypeName
  -> Int             -- ^ LogicalTypeId
  -> Text.Text       -- ^ FormatTypeName
  -> Int             -- ^ FormatTypeId
  -> CallDate        -- ^ import CDRS >= of this date
  -> Bool            -- ^ is status file
  -> Maybe (CallDate, CallDate) -- ^ the Status TimeFrame in case
  -> DBConf          -- ^ the DB were writing
  -> CurrencyPrecisionDigits
  -> OrderedStream (RemoteCDRId, RawSourceCDR) -- ^ the input CDRS
  -> (DB.MySQLConn -> IO ())    -- ^ an action for annotating successfully imports
  -> IO SourceDataSummaryInfo

rateEngine_importIntoDB
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
  fromDate
  isStatusFile
  maybeStatusTimeFrame
  dbConf
  precision
  inChan
  commitAction = do

   nrOfRatingJobs <- process_initCores 0

   (CDRFormatSpec !sourceCDRParams !theType)
     <- case getSupportedCDRSImporters logicalTypeName formatTypeName of
          Nothing
            -> throwIO $ AsterisellException ("There is no known/configured source data file importer for format " ++ (Text.unpack logicalTypeName) ++ ", and version " ++ (Text.unpack formatTypeName))
          Just r
            -> return r

   let pipeName = BS.concat ["/var/tmp/import_raw_cdrs_pipe__", dbConf_dbName dbConf]

   withResource'
    (db_init dbConf Nothing precision)
    (\dbState -> do
       let conn = fromRight (error "err 2553") $ dbps_conn dbState

       when isStatusFile (db_deleteFromArSource conn formatTypeId providerId maybeStatusTimeFrame)

       nowLocalTime1 <- getZonedTime
       let callDate0 = zonedTimeToLocalTime nowLocalTime1
       let summaryInfo0 = (Nothing, 0, 0)
       -- NOTE: use an MVar instead of an IORef, because I had space-leak problems

       (outChan, writeToDBJobs)
         <- db_loadDataFromNamedPipe
              dbState
              pipeName
              "ar_source_cdr"
              sourceCDR_mysqlCSVLoadCmd
              sourceCDR_toMySQLCSV

       (_, summaryInfo@(maybeTimeFrame, correctLines, linesWithErrors)) <- do
         let parseCallDate (_, rawCDR)
               = case (parseTypedRawSourceCDRToCallDate theType sourceCDRParams 4 providerName rawCDR) of
                   Left err -> Just $ (rawCDR, Left err)
                   Right Nothing -> Nothing
                   Right (Just callDate) -> if callDate >= fromDate then Just $ (rawCDR, Right callDate) else Nothing

         processJob <- async $ process parseCallDate outChan (callDate0, summaryInfo0)

         r <- wait processJob
         V.mapM_ wait writeToDBJobs
         return $ DeepSeq.force r

       _ <- takeMVar $ dbps_semaphore dbState

       -- Update the rating frame again.
       case linesWithErrors > correctLines of
         True -> throwIO $ AsterisellException $ "The input file contains more errors (" ++ show linesWithErrors ++ "), than correctly imported CDRS (" ++ show correctLines ++ "). This is suspect (i.e. input file in different format, error in the code). The file will be not imported, and it will remain in the input directory. For more details about the parsing errors, use the admin command `php asterisell.php dev test-import-cdrs`."
         False -> do case maybeStatusTimeFrame of
                       Just (d1, _) -> db_updateRatingTimeFrame conn d1
                       Nothing -> return ()

                     case maybeTimeFrame of
                       Nothing -> do return ()
                       Just (rateFromCallDate, _) -> db_updateRatingTimeFrame conn rateFromCallDate

       putMVar (dbps_semaphore dbState) ()

       commitAction conn

       return summaryInfo)
    (\isOk dbState -> do
       when (not isOk) $ do
         tryPutMVar (dbps_semaphore dbState) ()
         return ()
         -- NOTE: in case of interrupted processing, there can be no ownership in the DB,
         -- so force it
       db_releaseResourceR isOk dbState)
    (createImportError)

 where

   process parseCallDate outChan state1 = do
     maybeRawCDRS <- takeMVar inChan
     case maybeRawCDRS of
       Just rawCDRS -> do
         let (!state2, !sourceCDRS) =
                 mapAccumL toSourceCDR state1 $
                   V.map fromJust $
                     V.filter isJust $
                       V.map parseCallDate rawCDRS

         orderedStream_put outChan (Just sourceCDRS)
         process parseCallDate outChan state2
       Nothing -> do
         orderedStream_put outChan Nothing 
         return state1

   toSourceCDR :: (CallDate, SourceDataSummaryInfo) -> (RawSourceCDR, Either AsterisellError CallDate) -> ((CallDate, SourceDataSummaryInfo), DBSourceCDR)
   toSourceCDR state1@(!lastCallDate, (!maybeMaxMinCallDate, !correctLines, !linesWithErrors)) (!rawCDR, maybeError)
     = let (!callDate, !state2) =
             DeepSeq.force $ 
               case maybeError of
                 Left err ->
                   (lastCallDate, (maybeMaxMinCallDate, correctLines, linesWithErrors + 1))
                         -- NOTE: in case of error update only the count of lines with errors,
                         -- and use a "current date".
                         -- In this way the error will be reported in details when the user try to rate recent calls.
                         -- This is an hack but 99% of the times it is the correct thing to do.
                 Right parsedCallDate ->
                   (parsedCallDate, (newMaxMinCallDate parsedCallDate maybeMaxMinCallDate, correctLines + 1, linesWithErrors))

       in ((callDate, state2), DBSourceCDR callDate providerId logicalTypeId formatTypeId rawCDR)

   createImportError e
     = SomeException $ AsterisellException $ "There is a critical error during the importing from provider " ++ (Text.unpack providerName) ++ ", format " ++  (Text.unpack logicalTypeName) ++ "__" ++  (Text.unpack formatTypeName) ++ ". " ++ (displayException e) ++ ".\nAll data from the provider or with the same format will be not rated.\nThis is probably an error in the application configuration. Contact the assistance."

-- ---------------------------------------
-- Debug

-- | Parse CDRS and print debug informations.
--
rateEngine_debugCDRParsing
  :: CDRProviderName
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> DBConf
  -> CurrencyPrecisionDigits
  -> OrderedStream (RemoteCDRId, RawSourceCDR)
  -- ^ the CDRs in source/raw format
  -> FilePath
  -> IO SourceDataSummaryInfo

rateEngine_debugCDRParsing
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
  dbConf
  precision
  inChan
  debugFileName = do

   nrOfRatingJobs <- process_initCores 0

   (CDRFormatSpec !sourceCDRParams !theType)
     <- case getSupportedCDRSImporters logicalTypeName formatTypeName of
          Nothing
            -> throwIO $ AsterisellException ("There is no known/configured source data file importer for format " ++ (Text.unpack logicalTypeName) ++ ", and version " ++ (Text.unpack formatTypeName))
          Just r
            -> return r

   nowLocalTime1 <- getZonedTime
   let callDate0 = zonedTimeToLocalTime nowLocalTime1
   -- NOTE: use an MVar instead of an IORef, because I had space-leak problems

   let parseCDR (_, rawCDR) = (rawCDR, parseTypedRawSourceCDR theType sourceCDRParams precision providerName rawCDR)

   withFile debugFileName WriteMode $ \outH -> do
     (_, r@(timeFrame, correctLines, linesWithErrors)) <- processCDRS outH parseCDR (callDate0, (Nothing, 0, 0))

     hPutStrLn outH $ "\nTime frame: " ++ show timeFrame
     hPutStrLn outH $ "Total correctly parsed lines: " ++ show correctLines ++ ", total lines with errors: " ++ show linesWithErrors

     return r

 where

   processCDRS outH parseCDR state1 = do
     mcdrs <- readMVar inChan
     case mcdrs of
       Just cdrs -> do
         state2 <- V.foldM (processCDR outH) state1 $ V.map parseCDR cdrs
         processCDRS outH parseCDR state2
       Nothing -> do
         return state1

   processCDR
     :: Handle
     -> (LocalTime, SourceDataSummaryInfo)
     -> (RawSourceCDR, Either AsterisellError (Maybe (LocalTime, Either AsterisellError [CDR])))
     -> IO (LocalTime, SourceDataSummaryInfo)

   processCDR outH (!lastCallDate, (!maybeMaxMinCallDate, !correctLines, !linesWithErrors)) (!rawCDR, !maybeParsedCDR) = do
     hPutStrLn outH $ "\nSource CDR: " ++ fromByteStringToString rawCDR
     case maybeParsedCDR of
       Left err -> do
           hPutStrLn outH $ "Critical error during parsing: " ++ show err
           return (lastCallDate, (maybeMaxMinCallDate, correctLines, linesWithErrors + 1))
       Right Nothing -> do
           hPutStrLn outH $ "CDR to ignore"
           return (lastCallDate, (maybeMaxMinCallDate, correctLines + 1, linesWithErrors))
       Right (Just (callDate, Left err)) -> do
           hPutStrLn outH $ "Call date is " ++ show callDate ++ ", but error during parsing: " ++ show err
           return (callDate, (newMaxMinCallDate callDate maybeMaxMinCallDate, correctLines, linesWithErrors + 1))
       Right (Just (callDate, Right [])) -> do
           hPutStrLn outH $ "CDR to ignore"
           return (callDate, (newMaxMinCallDate callDate maybeMaxMinCallDate, correctLines + 1, linesWithErrors))
       Right (Just (callDate, Right cdrs)) -> do
           M.mapM_ (\cdr -> hPutStrLn outH $ cdr_showDebug cdr) cdrs
           return (callDate, (newMaxMinCallDate callDate maybeMaxMinCallDate, correctLines + 1, linesWithErrors))

-- ---------------------------------------
-- Export CDRS

-- | Export the CDRs in a human processable format.
--   Usually the export is done for fixing errors in the original source CDRs,
--   and then for importing again, or for legal porpouse.
rateEngine_exportDataFile
  :: FilePath
  -> CDRProviderName
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> (LocalTime, LocalTime)
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
        SourceCDRParamsDBTableWithAutoincrementId _ _ _
          -> return ()

      withResource'
        (db_openConnection dbConf False)
        (\readConn -> do
           let q = [str| SELECT content
                       | FROM ar_source_cdr
                       | WHERE ar_physical_format_id = ?
                       | AND ar_cdr_provider_id  = ?
                       | AND calldate >= ?
                       | AND calldate < ?
                       | ORDER BY calldate, id
                       | LIMIT ? OFFSET ?
                       |]

           stmtId <- DB.prepareStmt readConn (DB.Query q)

           S.withFileAsOutput outFile $ \outS -> do
             case sourceCDRParams of
               SourceCDRParamsCSVFile (Just header) _ _ UseUTF8
                 -> do S.write (Just $ fromTextToByteString header) outS
                       S.write (Just "\r\n") outS

               SourceCDRParamsCSVFile Nothing _ _ UseUTF8
                 -> do return ()

               SourceCDRParamsDBTableWithAutoincrementId _ _ _
                 -> do return ()

             processAllSplits readConn stmtId outS param_chunkSize2 0)
        (\_ db -> DB.close db)
        (id)

 where

   processAllSplits
       :: DB.MySQLConn
       -> StmtID
       -- ^ the statement to use for retrieving the CDRs
       -> S.OutputStream BS.ByteString
       -> Int
       -- ^ the limit (chunk size) to use
       -> Int
       -- ^ the current offset
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

     (_, inS1) <- DB.queryStmt conn stmtId queryParams
     inS2 <- S.map (\[rawCDR] -> BS.concat [fromDBByteString rawCDR, "\r\n"]) inS1
     (inS3, getCountCDRS) <- S.inputFoldM (\c b -> return $ c + 1) 0 inS2
     S.supply inS3 outS

     countCDRS <- getCountCDRS
     let newOffset = currentOffset + countCDRS
     case newOffset == currentOffset of
       True -> do S.write Nothing outS
                  return newOffset
       False -> processAllSplits conn stmtId outS splitSize newOffset

-- ---------------------------
-- DB operations

-- | Remove old status CDRs with the same cdr provider, and logical-type,
-- in a timeframe that is the same of this file, because this new piece of information
-- is a status replacing completely old values.
-- Pass Nothing as time-frame for deleting all the CDRs of the provider.
db_deleteFromArSource :: DB.MySQLConn -> PhysicalFormatId -> ProviderId -> (Maybe (LocalTime, LocalTime)) -> IO ()

db_deleteFromArSource conn formatId providerId Nothing = do
  let query = "DELETE FROM ar_source_cdr WHERE ar_physical_format_id = ? AND ar_cdr_provider_id  = ? "
  DB.execute conn query [toDBInt64 formatId, toDBInt64 providerId]
  return ()

db_deleteFromArSource conn formatId providerId (Just (fromDate, toDate)) = do
  let query = [str|DELETE FROM ar_source_cdr
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

-- | Inform the application that there are new calls to rate in the specified time-frame
db_updateRatingTimeFrame
  :: DB.MySQLConn
  -> LocalTime
  -- ^ from call time
  -> IO ()

db_updateRatingTimeFrame conn fromDate = do
  let query = [str|UPDATE ar_params
                  |SET new_imported_cdrs_from_calldate = IFNULL(LEAST(new_imported_cdrs_from_calldate, ?), ?)
                  |WHERE is_default = 1
                  |]

  DB.execute conn query [toDBLocalTime fromDate, toDBLocalTime fromDate]
  return ()
