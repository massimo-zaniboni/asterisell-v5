{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Import CDRS from external files or external databases inside `ar_source_cdr` table.
--   Signal only problems about callDate, because all other problems will be reported
--   during rating.
--
module Asterisell.ImportDataFiles (
  SourceDataSummaryInfo,
  rateEngine_importDataFile,
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

import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import Data.Time.LocalTime
import qualified Data.Text as Text

import Database.MySQL.Base as DB
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import System.IO.Streams as S
import qualified Data.Vector as V
import Control.DeepSeq as DeepSeq
import Control.Concurrent.MVar
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc

-- ---------------------------------
-- Params

-- | The number of source CDRs to fetch from the DB, before splitting the rating event.
--   Up to date the Haskell MySQL driver does not support the forward-only cursor mode,
--   and it stores all the CDRS of the rating query in RAM.
--   So the query is splitted (efficiently) in smaller queries, and they are rated separately.
--   NOTE: the cost of splitting the query is rather neglible, so use low numbers.
param_chunkSize2 :: Int
param_chunkSize2 = 1024 * 5
{-# INLINE param_chunkSize2 #-}

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
-- File as source of CDRS

-- | Parse a file and import into database as ar_source_cdr records, returning summary info on its content.
--
rateEngine_importDataFile
  :: FilePath
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
  -> IO SourceDataSummaryInfo

rateEngine_importDataFile
  inputFile
  providerName
  providerId
  logicalTypeName
  logicalTypeId
  formatTypeName
  formatTypeId
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
                   rawCDRS <- (allLines params fileContentStream) >>= (maybeRemoveHeader params) >>= (utf8Lines params)

                   rateEngine_importIntoDB
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
                     rawCDRS
                     (\ conn -> void $ DB.execute conn  "REPLACE INTO ar_local_file_to_delete(name) VALUES(?)" [toDBText $ Text.pack inputFile])
               ))

      (\_ _ -> return ())
      (createImportError)

  where

   utf8Lines params stream1
       = case params of
         SourceCDRParamsCSVFile _ _ _ UseUTF8
           -> return stream1

         -- _ -> error "Parsing of CSV files not in UTF8 format is not yet supported by the application. Contact the assistance. "
         -- NOTE:  up to date not needed because `UseUTF8`

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
         _ -> error "Parsing of CSV files with new lines inside quoted fields is not yet supported by the application. Contact the assistance. "

   createImportError e
     = SomeException $ AsterisellException $ "There is an error during the importing of file \"" ++ inputFile ++ "\". " ++ (displayException e)

-- --------------------------------------------
-- MySQL database as external source of CDRS

-- TODO

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
  -> Int
  -> Text.Text
  -> Int
  -> Text.Text
  -> Int
  -> Bool
  -> Maybe (LocalTime, LocalTime)
  -> DBConf
  -> CurrencyPrecisionDigits
  -> InputStream RawSourceCDR
  -- ^ the CDRs in source/raw format
  -> (DB.MySQLConn -> IO ())
  -- ^ an action for annotating successfully imports
  -> IO SourceDataSummaryInfo

rateEngine_importIntoDB
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
  inStream1
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
     (do conn <- db_openConnection dbConf False
         db_openTransaction conn
         return conn)
     (\conn -> do

       when isStatusFile (db_deleteFromArSource conn formatTypeId providerId maybeStatusTimeFrame)

       nowLocalTime1 <- getZonedTime
       let callDate0 = zonedTimeToLocalTime nowLocalTime1
       let summaryInfo0 = (Nothing, 0, 0)
       -- NOTE: use an MVar instead of an IORef, because I had space-leak problems 

       (rawCDRSChan, writeToDBProcess)
         <- db_loadDataFromNamedPipe
              (Right conn)
              pipeName
              "ar_source_cdr"
              sourceCDR_mysqlCSVLoadCmd
              sourceCDR_toMySQLCSV

       (_, summaryInfo@(maybeTimeFrame, totLines, linesWithErrors)) <- do
         let parseCallDate rawCDR
               = case (parseTypedRawSourceCDRToCallDate theType sourceCDRParams 4 providerName rawCDR) of
                   Left err -> Just $ (rawCDR, Left err)
                   Right Nothing -> Nothing
                   Right (Just callDate) -> Just $ (rawCDR, Right callDate)
 
         inStream2 <- (S.map parseCallDate inStream1) >>= stream_toJust
         !r <- S.foldM (\ (!s) (!cdr) -> sendToDB rawCDRSChan s cdr) (callDate0, summaryInfo0) inStream2

         putMVar rawCDRSChan Nothing
         _ <- waitAll (V.toList writeToDBProcess) []
         return $ DeepSeq.force r

       -- Update the rating frame again.
       case totLines > 0 && linesWithErrors == totLines of
         True -> throwIO $ AsterisellException $ "The code can not parse correctly the content. The code processing the file contains errors, or the file has not the specified format."
         False -> do case maybeStatusTimeFrame of
                       Just (d1, _) -> db_updateRatingTimeFrame conn d1
                       Nothing -> return ()

                     case maybeTimeFrame of
                       Nothing -> do return ()
                       Just (rateFromCallDate, _) -> db_updateRatingTimeFrame conn rateFromCallDate

       commitAction conn

       return summaryInfo)
     (\isOk conn -> do
         db_releaseResource isOk conn)
     (createImportError)

 where

   sendToDB
     :: OrderedStream DBSourceCDR
     -- ^ where sending CDRS
     -> (LocalTime, SourceDataSummaryInfo)
     -> (RawSourceCDR, Either AsterisellError LocalTime)
     -> IO (LocalTime, SourceDataSummaryInfo)

   sendToDB outChan (!lastCallDate, (!maybeMaxMinCallDate, !totLines, !linesWithErrors)) (!rawCDR, !maybeLocalTime)
     = let (lastCallDate', summaryInfo')
             = case maybeLocalTime of
                 Left err
                   -> (lastCallDate, (maybeMaxMinCallDate, totLines, linesWithErrors + 1))
                      -- NOTE: in case of error update only the count of lines with errors,
                      -- and use a "current date".
                      -- In this way the error will be reported in details when the user try to rate recent calls.
                      -- This is an hack but 99% of the times it is the correct thing to do.
                 Right callDate
                   -> (callDate, (newMaxMinCallDate callDate maybeMaxMinCallDate, totLines + 1, linesWithErrors))

           cdr' = DBSourceCDR lastCallDate' providerId logicalTypeId formatTypeId rawCDR

       in do putMVar outChan $ DeepSeq.force (Just (V.singleton cdr'))
             return $ DeepSeq.force (lastCallDate', summaryInfo')

   createImportError e
     = SomeException $ AsterisellException $ "There is a critical error during the importing from provider " ++ (Text.unpack providerName) ++ ", format " ++  (Text.unpack logicalTypeName) ++ "__" ++  (Text.unpack formatTypeName) ++ ". " ++ (displayException e) ++ ".\nAll data from the provider or with the same format will be not rated.\nThis is probably an error in the application configuration. Contact the assistance."

   {-# INLINE newMaxMinCallDate #-}
   newMaxMinCallDate :: LocalTime -> Maybe (LocalTime, LocalTime) -> Maybe (LocalTime, LocalTime)
   newMaxMinCallDate d Nothing = Just (d, d)
   newMaxMinCallDate d (Just (minDD, maxDD)) = Just (min minDD d, max maxDD d)

-- ---------------------------------------
-- Export CDRS

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
