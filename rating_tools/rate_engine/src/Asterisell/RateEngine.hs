{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, DeriveGeneric #-}

{- $LICENSE 2013, 2014, 2015, 2016, 2017
 * Copyright (C) 2013-2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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


-- | Rate CDRs.
--
--   See Cdr module, for extending the support for new CDRs source data files.
--   This file use the services of Cdr module, and of the Rates, for reading and rating the CDRs.
--
--   The MySQL interaction code is based on notes of https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell
module Asterisell.RateEngine (
  rateEngine_rate,
  rateEngine_incrementalRate,
  SourceDataSummaryInfo,
  DBConf(..),
  MaybeTimeFrame,
  extractLinesFromCSVFile,
  importSourceCDRFromCSVLine,
  rateEngine_importDataFile,
  rateEngine_testImportDataFile,
  rateEngine_exportDataFile,
  rateEngine_updateCachedGroupedCDRS,
  rateEngine_openDBAndUpdateCachedGroupedCDRS,
  tt_rateSpecificationsTests,
  tt_mainRateCalcTets
) where

import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.MainRatePlan
import Asterisell.CSVFileRatePlan
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.CdrsToRate
import Asterisell.CustomerSpecificImporters
import Asterisell.Services

import GHC.Generics (Generic)
import Data.List as List
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe
import Control.Monad as M
import Control.Monad.IO.Class       (liftIO)
import Data.Word
import qualified Data.Vector as V
import System.FilePath.Posix
import Data.Time.LocalTime
import Data.Time.Calendar
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char (ord, chr)
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Test.HUnit as HUnit
import qualified Data.Csv as CSV
import System.IO
import System.Directory as IO
import System.Posix.Files (fileExist, setFileMode)
import Debug.Trace
import Data.Maybe
import Data.Set as Set
import Data.IntMap as IMap
import qualified Data.Serialize as DBS
import qualified Data.Serialize.Text as DBS
import Data.String
import qualified System.Posix as Posix
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.BoundedChan
import Database.MySQL.Base as DB
import System.Posix.Files as Posix
import System.Directory
import qualified Codec.Compression.QuickLZ as LZ
import Control.Monad.Except
import Control.Monad.Catch (bracket, bracketOnError, handleAll)
import Data.Text.Encoding
import qualified Pipes.Safe as PS
import Control.Concurrent.Async
import Control.Exception (throwIO, ErrorCall(..), SomeException)
import GHC.IO.Handle.FD (openFileBlocking)
import System.Posix.Process
import System.Posix.Signals
import qualified Data.Serialize as Serialize
import Data.ByteString.Lazy.Builder.ASCII
import Data.ByteString.Lazy.Builder
import Data.Monoid
import Control.DeepSeq
import qualified Data.Fixed as Fixed
import qualified Data.Text.Read as T
import Data.Text.Encoding
import Data.Attoparsec.ByteString as P
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import Data.Hashable

--
-- Common Params
--

-- | DB access configurations.
data DBConf
   = DBConf {
       dbConf_dbName :: String
     , dbConf_user :: String
     , dbConf_password :: String
     } deriving (Eq, Show)

-- | An Extension like "123" that is matched by "12X" or "12*"
--   but not by an explicit "123".
data ExpandedExtension
  = ExpandedExtension {
      ee_organizationId :: !Int
    , ee_specificExtensionCode :: !BS.ByteString
    } deriving (Eq, Show, Generic)

instance Hashable ExpandedExtension

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
    , rs_expandedExtensions :: S.HashSet ExpandedExtension
    -- ^ the found expanded extensions that are not matched by an explicit expanded extension,
    --   but that are matched from a generic extension.
    , rs_errors :: AsterisellErrorsDictionary
    -- ^ prevent the generation of new errrors, if there are similar errors already generated
    , rs_criticalError :: Maybe (AsterisellError, AsterisellErrorDupKey)
    -- ^ Nothing if the rating process can continue, a critical error with the problem key otherwise
    }

instance NFData RatingStatus where
  rnf r = seq (r { rs_incomeBundleState = force (rs_incomeBundleState r) }) ()

ratingStatus_empty :: RatingStatus
ratingStatus_empty
  = RatingStatus {
       rs_incomeRate = Nothing
     , rs_costRate = Nothing
     , rs_incomeBundleState = Nothing
     , rs_expandedExtensions = S.empty
     , rs_errors = asterisellErrorsDictionary_empty
     , rs_criticalError = Nothing
    }

-- | A monad returning an error, or returning some result, inside IO
type RatingMonad = ExceptT (CDR, AsterisellError) IO

type YearAndMonth = (Integer, Int)

-- | A CDR with strings replaced with MySQL escaped strings.
data EscapedCDR
  = EscapedCDR {
      ecdr_internalTelephoneNumber :: !BS.ByteString
    , ecdr_externalTelephoneNumber :: !BS.ByteString
    , ecdr_channel :: !(Maybe BS.ByteString)
    , ecdr_externalTelephoneNumberWithAppliedPortability :: !(Maybe BS.ByteString)
    , ecdr_displayedExternalTelephoneNumber :: !(Maybe BS.ByteString)
    , ecdr_displayedMaskedExternalTelephoneNumber :: !(Maybe BS.ByteString)
    , ecdr_problemDuplicationKey  :: ! (Maybe BS.ByteString)
    , ecdr_debug_cost_rate :: ! (Maybe BS.ByteString)
    , ecdr_debug_income_rate :: ! (Maybe BS.ByteString)
    , ecdr_debug_residual_income_rate :: ! (Maybe BS.ByteString)
    , ecdr_debug_rating_details :: ! (Maybe BS.ByteString)
    }

instance NFData EscapedCDR where
  rnf c = seq c ()

type RawSourceCDR
       = (BS.ByteString
         -- ^id
         , BS.ByteString
         -- ^ call date
         , BS.ByteString
         -- ^ format name
         , BS.ByteString
         -- ^ version name
         , BS.ByteString
         -- ^ version id
         , BS.ByteString
         -- ^ provider name
         , BS.ByteString
         -- ^ provider id
         , BS.ByteString
         -- ^ source CDR content
         )

-- | A CDR to rate, with associated debug info.
type CDRToRate =
       ( CDR
       , BS.ByteString
         -- ^ the ar_rource_cdr.id field
       , BS.ByteString
         -- ^ logical type name
       , BS.ByteString
         -- ^ version type name
       , BS.ByteString
         -- ^ versionId
       , BS.ByteString
         -- ^ provider name
       , BS.ByteString
         -- ^ providerId
       , BS.ByteString
         -- ^ CDR source line
       , Either AsterisellError SourceCDRDescription
         -- ^ the critical error or the human readable description of the parsed CDR
       )

data RateCmd
  = RateCmd_close
  | RateCmd_rate CDRToRate
  | RateCmd_rateService ServiceCDR
 deriving(Show)

type CmdWait = MVar ()

cmdWait_wait :: CmdWait -> IO ()
cmdWait_wait w = takeMVar w

cmdWait_wake :: CmdWait -> IO ()
cmdWait_wake w = putMVar w ()

-- | A command to execute on the write transaction of the database.
data WriteCmd
  = WriteCmd_close !CallDate
  | WriteCmd_insert !CDR !BundleState
    -- ^ write a CDR. The BundleState is passed, only for forcing the the strict computation of the state, during rating, but it is not written.
  | WriteCmd_exec (Maybe CmdWait) !BS.ByteString
  | WriteCmd_saveBundleState !LocalTime !BundleState
  | WriteCmd_insertSourceCDRToExport !BS.ByteString
    -- ^ accept the ar_source_cdr.id to export
  | WriteCmd_insertExpandedExtension !ExpandedExtension

instance Show WriteCmd where
  show (WriteCmd_close d) = "WriteCmd_close " ++ show d
  show (WriteCmd_insert c s) = "WriteCmd_insert " ++ cdr_showDebug c
  show (WriteCmd_exec w c) = "WriteCmd_exec " ++ show c
  show (WriteCmd_saveBundleState t s) = "WriteCmd_saveBundleState " ++ show t
  show (WriteCmd_insertSourceCDRToExport i) = "WriteCmd_insertSourceCDRToExport " ++ show i

instance NFData WriteCmd where
  rnf (WriteCmd_insert cdr bundleState) = deepseq (cdr, bundleState) ()
  rnf w = seq w ()

-- | Command to send to the error process, for registering new errors.
data ErrorCmd
  = ErrorCmd_insert
      Bool -- ^ True for writing the error on DB
      AsterisellError
      (Maybe ( BS.ByteString -- ^ garbage key
             , CallDate -- ^ from date
             , CallDate -- ^ to date
             )) -- ^ Nothing for an error withou garbage collection
  | ErrorCmd_close

--
-- Initial Import of CDRS
--

-- | Extract each line of the CSV file in a more robust way respect a pure Cassava based parser.
--   From a theorical point of view the Cassava approach is the correct approach:
--   * new lines inside ".." are correctly recognized
--   * new lines not inside ".." generate an error
--   But I can not use Cassava in this way because in case of a new-line in some bad position, the remaining content of the file is ignored.
--   This approach of parsing line by line is in practice more robust, and fit better with the usual type of content of CDRs files.
--
--   I need this approach also because the ar_source_cdr table need an entry for each CDR, and so splitting a big file in lines
--   is the best approach.
extractLinesFromCSVFile
  :: SourceCDRParams
  -> SourceCDRImporter

extractLinesFromCSVFile params handle cdrsChan
  = do case localeConversion of
            UseUTF8
              -> hSetEncoding handle utf8_bom

       emitLines maybeDropFirstLine handle

 where

   localeConversion
     = case params of
              SourceCDRParamsCSVFile _ _ r -> r

   maybeDropFirstLine
     = case params of
         SourceCDRParamsCSVFile mh _ _
           -> isJust mh

   emitLines skipFirst h
     = do
         eof <- hIsEOF h
         case eof of
           True
             -> writeChan cdrsChan Nothing
           False
             -> do str <- LText.hGetLine h
                   -- NOTE: divide in lines taking in account the UTF8 format
                   when (not skipFirst) (writeChan cdrsChan (Just $ LText.encodeUtf8 str))
                   emitLines False h

-- | Return a CDR, assuming it is a CSV line in UTF8 format.
importSourceCDRFromCSVLine
  :: forall a . (CDRFormat a)
  => Maybe a    -- ^ used only for forcing the type constraints on the pipe, the value is ignored, so you can pass Nothing
  -> CurrencyPrecisionDigits
  -> SourceCDRParams
  -> CDRImporter

importSourceCDRFromCSVLine notUsed precision params !content !provider
  = let fieldDelimiter = case params of
                           SourceCDRParamsCSVFile _ r _ -> r

        csvOptions = CSV.defaultDecodeOptions {
                           CSV.decDelimiter = fromIntegral (Char.ord fieldDelimiter)
                     }

        d :: Either String (V.Vector a)
        !d = CSV.decodeWith csvOptions CSV.NoHeader content
    in case d of
      Left !err
        -> (Nothing, Left $ (createError
                                Type_Error
                                Domain_RATES
                                ("parsing error" ++ err)
                                ("Parsing error. " ++ err)
                                ("")
                                ("")))
      Right !vs
        -> case V.length vs of
             1 -> let !v = V.head vs
                  in case getCallDate v of
                       Left !err
                         -> (Nothing, Left err)
                       Right !Nothing
                         -> (Nothing, Right [])
                       Right (Just !callDate)
                         -> case toCDR precision provider v of
                              Left !err
                                -> (Just callDate, Left err)
                              Right !cdrs
                                -> let cdrsWithDescription = List.map (\cdr -> (cdr, show cdr)) cdrs
                                   in (Just callDate, Right cdrsWithDescription)
             n -> (Nothing, Left $ (createError
                                Type_Error
                                Domain_RATES
                                ("parsing error, unexpected len " ++ show n)
                                ("Parsing error. Unexpexted decode with " ++ show n ++ " records.")
                                ("")
                                ("")))

-- | Export a CDR to a file.
exportSourceCDRToCSVLine
  :: Handle
     -- ^ where to wirte
  -> SourceCDRParams
     -- ^ the format of the line
  -> Text.Text
     -- ^ the CSV line in UTF8 format
  -> IO ()
     -- ^ write the result to the file, converting to the right format.

exportSourceCDRToCSVLine h params@(SourceCDRParamsCSVFile _ _ UseUTF8) line
  = do Text.hPutStr h line
       Text.hPutStr h "\r\n"
       return ()

-- | Update in the database, the rating time frame.
db_updateRatingTimeFrame :: DB.Connection -> Bool -> LocalTime -> LocalTime -> IO ()
db_updateRatingTimeFrame db importedServiceCDRS  rateFromDate1 rateToDate1
  = do
       let query1
             = case importedServiceCDRS of
                 True
                   -> "SELECT scheduled_imported_services_rerate_from_specific_calldate, scheduled_imported_services_rerate_to_specific_calldate FROM ar_params WHERE is_default = 1 LIMIT 1"
                 False
                   -> "SELECT new_imported_cdrs_from_calldate, new_imported_cdrs_to_calldate FROM ar_params WHERE is_default = 1 LIMIT 1"

       DB.query db $ fromString query1
       r <- DB.storeResult db
       maybeRow <- DB.fetchRow r
       when (not $ List.null maybeRow) $ do
         let [maybeOldFromDate1, maybeOldToDate1] = maybeRow
         let rateFromDate2 = fromString $ fromLocalTimeToMySQLDateTime rateFromDate1
         let rateToDate2 = fromString $ fromLocalTimeToMySQLDateTime rateToDate1

         let rateFromDate3 :: BS.ByteString
               = case maybeOldFromDate1 of
                   Nothing
                     -> rateFromDate2
                   Just oldFromDate1
                     -> min oldFromDate1 rateFromDate2

         let rateToDate3 :: BS.ByteString
               = case maybeOldToDate1 of
                   Nothing
                     -> rateToDate2
                   Just oldToDate1
                     -> max oldToDate1 rateToDate2

         DB.freeResult r

         let (query2_1, query2_2)
               = case importedServiceCDRS of
                   True
                     -> ("UPDATE ar_params SET scheduled_imported_services_rerate_from_specific_calldate = '", "', scheduled_imported_services_rerate_to_specific_calldate = '")
                   False
                     -> ("UPDATE ar_params SET new_imported_cdrs_from_calldate = '", "', new_imported_cdrs_to_calldate = '")

         DB.query db $ BS.concat [query2_1, rateFromDate3, query2_2, rateToDate3, "'"]
         return ()

-- | Meta info about a SourceDataFile.
--   At this stage only the calldate is imported and processed.
type SourceDataSummaryInfo
  = ( Maybe (LocalTime, LocalTime)
      -- ^ max and minimum found call date, for normal CDRs
    , Maybe (LocalTime, LocalTime)
      -- ^ max and minimm found call date, for imported service CDRS
    , Int
      -- ^ total number of lines
    , Int
      -- ^ lines with errors
    )

data SourceCDRToImport = SourceCDRToImport !LocalTime !Int !Int !Int !Bool !BS.ByteString

instance NFData SourceCDRToImport where
  rnf c = seq c ()

-- | Import CDRs into the database.
data ImportCmd
       = ImportCmd_close
       | ImportCmd_exec (Maybe (MVar ())) BS.ByteString
       | ImportCmd_updateRatingTimeFrame Bool LocalTime LocalTime
       | ImportCmd_insert !SourceCDRToImport

instance NFData ImportCmd where
  rnf c = seq c ()

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
  -> ConfiguredCDRSImporters
  -> FilePath
  -> Bool
  -> CDRProviderName
  -> Int
  -> BS.ByteString
  -> Int
  -> BS.ByteString
  -> Int
  -> Bool
  -> Maybe (LocalTime, LocalTime)
  -> DBConf
  -> CurrencyPrecisionDigits
  -> IO SourceDataSummaryInfo

rateEngine_importDataFile
  maybeDebugFile
  defaultSourceDataImporter
  inputFile
  deleteInputFile
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

   DB.initLibrary
   DB.initThread

   (params, sourceImporter, importer)
     <- case configuredCDRSImporters_get defaultSourceDataImporter logicalTypeName formatTypeName of
          Nothing
            -> throwIO $ ErrorCall $ "There is no known/configured source data file importer."
          Just r
            -> return r

   sourceCDRSChan <- newBoundedChan (writeBuffSize * 2)
   debugChan <- newBoundedChan writeBuffSize

   Control.Monad.Catch.handleAll handleImportErrors $ (do
     withAsync (debugFileProcess maybeDebugFile debugChan) $ (\debugThread -> do
       withAsyncBound (writeProcess debugChan sourceCDRSChan) (\writeThread -> do

          when isStatusFile $ do

            -- Remove old status CDRs with the same cdr provider, and logical-type,
            -- in a timeframe that is the same of this file, because this new piece of information
            -- is a status replacing completely old values.
            let deleteQuery
                  = BS.concat [
                           "DELETE FROM ar_source_cdr WHERE ar_physical_format_id = ", fromString $ show formatTypeId
                           , " AND ar_cdr_provider_id  = ", fromString $ show providerId
                           , case maybeStatusTimeFrame of
                               Nothing
                                 -> ""
                                    -- in this case the status file is on all the dates of the database, and replace completely the previous info
                               Just (d1, d2)
                                 -> BS.concat [
                                      " AND calldate >= '", fromString $ fromLocalTimeToMySQLDateTime d1, "'"
                                      , " AND calldate < '" , fromString $ fromLocalTimeToMySQLDateTime d2, "'"
                                      ]
                           ]

            debugFile_append maybeDebugFile debugChan $ "Query: " ++ (Text.unpack $ fromMySQLResultToText $ Just deleteQuery)
            w <- newEmptyMVar
            writeChan sourceCDRSChan (ImportCmd_exec (Just w) deleteQuery)
            cmdWait_wait w
            debugFile_append maybeDebugFile debugChan "Pass 2"

            -- Signal that che CDRs must be rerated later (deleted and replaced with the new version on source-cdrs)
            -- DEV-NOTE: changing a DAY force a rerating of the day, but there can be days missing in a statuts file, and so an explicit rerate event is better.
            case maybeStatusTimeFrame of
              Just (d1, d2)
                -> writeChan sourceCDRSChan (ImportCmd_updateRatingTimeFrame False d1 d2)
              Nothing
                -> return ()
                   -- TODO not supported case.
                   -- There will be a rerate event for each day, or the user must force a manual rerating.
                   -- Up to date it is good enough.


          summaryInfo@(maybeTimeFrame, maybeTimeFrame2, totLines, linesWithErrors)
           <- withFile inputFile ReadMode $! \sourceH -> do
                nowLocalTime1 <- liftIO getZonedTime
                let nowLocalTime = zonedTimeToLocalTime nowLocalTime1
                cdrsChan <- newBoundedChan (writeBuffSize * 2)
                (_, r) <- concurrently (sourceImporter sourceH cdrsChan) (processSourceCDR importer cdrsChan sourceCDRSChan (nowLocalTime, (Nothing, Nothing, 0, 0)))
                return r

          case totLines > 0 && linesWithErrors == totLines of
           True -> throwIO $ ErrorCall $ "The code can not parse correctly the content. The code processing the file contains errors, or the file has not the specified format."
           False -> do case maybeTimeFrame of
                         Nothing
                           -> return ()
                         Just (rateFromCallDate, rateToCallDate)
                           -> do writeChan sourceCDRSChan $ ImportCmd_updateRatingTimeFrame False rateFromCallDate rateToCallDate
                                 return ()

                       case maybeTimeFrame2 of
                         Nothing
                           -> return ()
                         Just (rateFromCallDate, rateToCallDate)
                           -> do writeChan sourceCDRSChan $ ImportCmd_updateRatingTimeFrame True rateFromCallDate rateToCallDate
                                 return ()

          -- Send the signal about the end of rating process, and close the db write thread
          writeChan sourceCDRSChan (ImportCmd_close)
          wait writeThread

          -- Bet/hope that after the commit the delete operation always suceedee, otherwise repeated info will be added.
          when deleteInputFile (removeFile inputFile)

          writeChan debugChan (Left ())
          wait debugThread

          return summaryInfo)))

 where

   dbParams = DB.defaultConnectInfo {
                     connectUser = dbConf_user dbConf
                   , connectPassword = dbConf_password dbConf
                   , connectDatabase = dbConf_dbName dbConf
                   , connectOptions = [Reconnect False, CharsetName "utf8"]
                   }

   handleImportErrors :: SomeException -> IO SourceDataSummaryInfo
   handleImportErrors e
     = throwIO $ ErrorCall $ "There is an error during the importing of file \"" ++ inputFile ++ "\", with provider " ++ (Text.unpack providerName) ++ ", format " ++  (Text.unpack $ fromMySQLResultToText $ Just $ logicalTypeName) ++ "__" ++  (Text.unpack $ fromMySQLResultToText $ Just formatTypeName) ++ ". " ++ (show e) ++ ".\nAll the source data files with this format, and/or similar problems, will be not imported and rated. So there can be an high number of CDRs that are not rated. Note that there is only one error message for a file of this type, but this error message implies that all files of the same type are not rated.\nThis is probably an error in the application configuration. Contact the assistance."

   -- | How many CDRS buffering before sending to the database.
   writeBuffSize :: Int
   writeBuffSize = 100

   -- | Write CDRs on the database.
   --   Store some CDRS, before sending them in batch mode to the database, because this operation is faster than a single send.
   writeProcess :: DebugChan -> BoundedChan ImportCmd -> IO ()
   writeProcess debugChan sourceCDRSChan
     = let
           exportBool :: Bool -> Builder
           exportBool True = charUtf8 '1'
           exportBool False = charUtf8 '0'

           exportCDR :: SourceCDRToImport -> [Builder]
           exportCDR (SourceCDRToImport localTime providerId typeId versionId isImportedServiceCDR escapedContent)
             =    [
                    charUtf8 '('
                  , exportLocalTime $ Just localTime
                  , charUtf8 ','
                  , intDec providerId
                  , charUtf8 ','
                  , intDec versionId
                  , charUtf8 ','
                  , exportBool isImportedServiceCDR
                  , charUtf8 ','
                  , charUtf8 '\''
                  , byteString escapedContent
                  , charUtf8 '\''
                  , charUtf8 ')'
                  ]

           -- | Export the CDRS, in inverse order respect the specified list.
           exportCDRS :: [SourceCDRToImport] -> Builder
           exportCDRS cdrs
             = let f r cdr = (exportCDR cdr):r
               in  mconcat $ List.concat $ List.intersperse [charUtf8 ','] $ List.foldl' f [] cdrs

           -- | Write the CDRS in reverse order respect the specified list.
           writeCachedCDRSOnDB :: DB.Connection -> [SourceCDRToImport] -> IO ()
           writeCachedCDRSOnDB writeConn cdrs
             = let cmd1 = stringUtf8 "INSERT INTO ar_source_cdr(calldate, ar_cdr_provider_id, ar_physical_format_id, is_imported_service_cdr, content) VALUES" `mappend` exportCDRS cdrs
                   insertCmd = LBS.toStrict $ toLazyByteString cmd1
               in when (not $ List.null cdrs) $ do
                    DB.query writeConn insertCmd

           processRequests :: DB.Connection -> ([SourceCDRToImport], Int) -> IO ()
           processRequests writeConn (!cachedCDRS, !buffSize) = do
             !maybeInput <- readChan sourceCDRSChan
             case maybeInput of
               ImportCmd_close
                 -> writeCachedCDRSOnDB writeConn $!! cachedCDRS

               ImportCmd_exec (maybeWait) cmd
                 -> do DB.query writeConn cmd
                       case maybeWait of
                         Just w -> cmdWait_wake w
                         Nothing -> return ()
                       processRequests writeConn (cachedCDRS, buffSize)

               ImportCmd_updateRatingTimeFrame importedServiceCDRS fromDate toDate
                 -> do db_updateRatingTimeFrame writeConn importedServiceCDRS fromDate toDate
                       processRequests writeConn (cachedCDRS, buffSize)

               ImportCmd_insert (SourceCDRToImport !localTime !providerId !formatId !versionId !isImportedServiceCDR !source)
                 -> do !escapedSource <- DB.escape writeConn source
                       let !cachedCDRS2 = ((SourceCDRToImport localTime providerId formatId versionId isImportedServiceCDR escapedSource):cachedCDRS)
                       case buffSize >= writeBuffSize of
                          False
                            -> processRequests writeConn $!! (cachedCDRS2, buffSize + 1)
                          True
                            -> do writeCachedCDRSOnDB writeConn $!! cachedCDRS2
                                  processRequests writeConn ([], 0)

       in bracketOnError
            (do DB.initThread
                openConnection dbParams True)
            (\writeConn -> -- sometime these transactions can not be closed, but I want to report the original error message.
                           Control.Monad.Catch.handleAll (\ _ -> return ()) $ do
                             DB.rollback writeConn
                             DB.close writeConn)

            (\writeConn -> do processRequests writeConn ([], 0)
                              DB.commit writeConn
                              DB.close writeConn)

   newMaxMinCallDate :: LocalTime -> Bool -> Maybe (LocalTime, LocalTime) -> Maybe (LocalTime, LocalTime)
   newMaxMinCallDate d False v = v
   newMaxMinCallDate d True Nothing = Just (d, d)
   newMaxMinCallDate d True (Just (minDD, maxDD)) = Just (min minDD d, max maxDD d)

   processSourceCDR :: CDRImporter -> BoundedChan (Maybe LBS.ByteString) -> BoundedChan ImportCmd -> (LocalTime, SourceDataSummaryInfo) -> IO (SourceDataSummaryInfo)
   processSourceCDR importer sourceChan destChan (!lastCallDate, (!maybeMaxMinCallDate, !maybeImportedServiceeMaxMinCallDate, !totLines, !linesWithErrors))
     = do !maybeCdrContent <- readChan sourceChan
          case maybeCdrContent of
            Nothing
              -> -- end of pipe
                 return (maybeMaxMinCallDate, maybeImportedServiceeMaxMinCallDate, totLines, linesWithErrors)

            Just !cdrContent
              -> do let imported@(maybeCDRDate, errorOrCDRS) = importer cdrContent providerName

                    let !thereIsNormalCDR
                          = case errorOrCDRS of
                              Right cdrs
                                -> List.any (\(cdr, _) -> cdr_isNormalCDR cdr) cdrs
                              _ -> True

                    let !thereIsImportedServiceCDR
                          = case errorOrCDRS of
                              Right cdrs
                                -> List.any (\(cdr, _) -> cdr_isServiceCDR cdr) cdrs
                                   -- NOTE: by default an imported CDR that is a service, is an imported service CDR.
                                   -- The field cdr_isImportedServiceCDR is not yet explicitely set in this phase.
                              _ -> False
                                   -- it is already signaled as normal CDR

                    let (!reallyImportCDR, !callDate, !newMaybeMaxMinCallDate, !newMaybeImportedServiceeMaxMinCallDate)
                          = case imported of
                              (Just !d, _)
                                -> force (True
                                         , d
                                         , newMaxMinCallDate d thereIsNormalCDR maybeMaxMinCallDate
                                         , newMaxMinCallDate d thereIsImportedServiceCDR maybeImportedServiceeMaxMinCallDate)

                              (Nothing, Right [])
                                -> force (False, lastCallDate, maybeMaxMinCallDate, maybeImportedServiceeMaxMinCallDate)
                                   -- skip CDRS with a recognize NULL parsing

                              (Nothing, _)
                                -> force (True, lastCallDate, maybeMaxMinCallDate, maybeImportedServiceeMaxMinCallDate)

                    let !nextLinesWithErrors
                          = case imported of
                              (_, Left err)
                                -> linesWithErrors + 1
                              (_, Right _)
                                -> linesWithErrors

                    let !isImportedServiceCDR = thereIsImportedServiceCDR && (not thereIsNormalCDR)

                    (when reallyImportCDR) (writeChan destChan $!! ImportCmd_insert $!! SourceCDRToImport callDate providerId logicalTypeId formatTypeId isImportedServiceCDR (LBS.toStrict cdrContent))
                    -- NOTE: in case of parsing errors use the call date of previously parsed CDR, that is a good aproximation.

                    -- process the rest of the pipe using a recursive call, and passing the new processing "state"
                    processSourceCDR importer sourceChan destChan $!! (callDate, (newMaybeMaxMinCallDate, newMaybeImportedServiceeMaxMinCallDate, totLines + 1, nextLinesWithErrors))


--
-- Export CDRS
--

rateEngine_exportDataFile
  :: ConfiguredCDRSImporters
  -> FilePath
  -> CDRProviderName
  -> Int
  -> BS.ByteString
  -> Int
  -> BS.ByteString
  -> Int
  -> (LocalTime, LocalTime)
  -> Bool -- ^ True for using only source cdrs in ar_source_cdr_to_move
  -> DBConf
  -> IO Int

rateEngine_exportDataFile
  defaultSourceDataImporter
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

      DB.initLibrary
      DB.initThread

      (params, sourceImporter, importer)
        <- case configuredCDRSImporters_get defaultSourceDataImporter logicalTypeName formatTypeName of
             Nothing
               -> throwIO $ ErrorCall $ "There is no known/configured source data file importer."
             Just r
               -> return r

      bracketOnError
        (openConnection dbParams False)
        (\readConn -> do DB.commit readConn
                         DB.close readConn)
        (\readConn -> do

           let queryIn1 = "SELECT content FROM ar_source_cdr USE INDEX(ar_source_cdr_I_1) "

           let queryIn2 = if useOnlySourceCdrsToMove
                          then " INNER JOIN ar_source_cdr_to_move ON ar_source_cdr.id = ar_source_cdr_to_move.ar_source_cdr_id "
                          else ""

           let queryIn3 = [" WHERE ar_physical_format_id = ", fromString $ show formatTypeId
                          , " AND ar_cdr_provider_id  = ", fromString $ show providerId
                          , " AND calldate >= '", fromString $ fromLocalTimeToMySQLDateTime fromCallDate, "'"
                          , " AND calldate < '" , fromString $ fromLocalTimeToMySQLDateTime toCallDate, "'"
                          , " ORDER BY calldate "]

           let query = BS.concat $ queryIn1:queryIn2:queryIn3

           DB.query readConn query
           r <- DB.useResult readConn

           writtenCDRS
             <- withFile outFile WriteMode $! \outH -> do
                  case params of
                    SourceCDRParamsCSVFile (Just header) _ UseUTF8 -> do Text.hPutStr outH header
                                                                         Text.hPutStr outH "\r\n"
                    SourceCDRParamsCSVFile Nothing _ UseUTF8 -> do return ()

                  case params of
                    SourceCDRParamsCSVFile _ _ UseUTF8 -> return ()
                    SourceCDRParamsCSVFile _ _ locale -> throwIO $ ErrorCall $ "(err 573) The code can not manage character locale conversions " ++ show locale

                  readAllCDRS params outH readConn r 0

           DB.freeResult r
           DB.commit readConn
           DB.close readConn
           return writtenCDRS)

 where

   dbParams = DB.defaultConnectInfo {
                     connectUser = dbConf_user dbConf
                   , connectPassword = dbConf_password dbConf
                   , connectDatabase = dbConf_dbName dbConf
                   , connectOptions = [Reconnect False, CharsetName "utf8"]
                   }

   readAllCDRS :: SourceCDRParams -> Handle -> DB.Connection -> DB.Result -> Int -> IO Int
   readAllCDRS params outH readConn r countCDRS
     = do maybeRow <- DB.fetchRow r
          case maybeRow of
            [] -> return countCDRS
            [Just lineB] -> let lineT = fromMySQLResultToText $ Just lineB
                            in do exportSourceCDRToCSVLine outH params lineT
                                  readAllCDRS params outH readConn r (countCDRS + 1)

--
-- Rating of CDRs
--

-- | Rate from (inclusive) to (exclusive)
type MaybeTimeFrame = Maybe (LocalTime, LocalTime)

maybeTimeFrame_larger :: MaybeTimeFrame -> MaybeTimeFrame -> MaybeTimeFrame
maybeTimeFrame_larger Nothing Nothing = Nothing
maybeTimeFrame_larger (Just r1) Nothing = (Just r1)
maybeTimeFrame_larger Nothing (Just r2) = (Just r2)
maybeTimeFrame_larger (Just (x1, y1)) (Just (x2, y2)) = Just (min x1 x2, max y1 y2)


-- | Rate the CDRs some days at a time, for preventing space leak problems in the code.
--   Up to date seems there are no big problems, but use a time-frame split in any case.
rateEngine_incrementalRate
  :: Bool                    -- ^ True for billing, False for call reporting
  -> ConfiguredCDRSImporters -- ^ the importers to use
  -> DBConf           -- ^ the connection params to use
  -> Maybe FilePath   -- ^ debug file name
  -> MaybeTimeFrame   -- ^ rate normal calls
  -> MaybeTimeFrame   -- ^ rate imported service cdrs
  -> Bool             -- ^ True if it must delete also the rating event about official call-date
  -> ConfiguredRatePlanParsers
  -> FilePath
  -> FilePath
  -> EnvParams
  -> ServiceParams
  -> IO Int
  -- ^ return the number of processed CDRs.
  --   Throw an exception in case of critical errors during rating.
  --   Signal normal errors on the error table.

rateEngine_incrementalRate isVoipReseller configuredCDRSImporters dbConf maybeDebugFile maybeNormalCalls maybeImportedServices isOfficialRatingEnv configuredRateParsers ratePlanChanges ratePlanBaseFileName params serviceParams
  = do
       let isDebugMode = isJust maybeDebugFile

       let maybeRatingTimeFrame = maybeTimeFrame_larger maybeNormalCalls maybeImportedServices

       -- First rate imported service-cdrs, using a unique pass, because usually they are very few.
       r1 <- case maybeImportedServices of
               Nothing
                 -> return 0
               Just (d1, d2)
                 -> do r <- rateTimeFrame True (d1, 0) d2
                       return $ snd r

       -- Then rate normal CDRS, using incremental pass (if enabled)
       -- Note that in case of an isOfficialRatingEnv then also imported services are deleted and regenerated.
       r2 <- case maybeNormalCalls of
               Nothing
                 -> return r1
               Just (d1, d2)
                 -> do  r <- foldM (rateTimeFrame False) (d1, r1) (splittedTimeFrames d1 d2)
                        return $ snd r

       return r2

 where

   splitDays = 31 * 12

   dbParams = DB.defaultConnectInfo {
                     connectUser = dbConf_user dbConf
                   , connectPassword = dbConf_password dbConf
                   , connectDatabase = dbConf_dbName dbConf
                   , connectOptions = [Reconnect False, CharsetName "utf8"]
                   }

   rateTimeFrame importedServices (s1, c1) s2
     = do c2 <- rateEngine_rate isVoipReseller configuredCDRSImporters dbConf maybeDebugFile s1 s2 importedServices isOfficialRatingEnv configuredRateParsers ratePlanChanges ratePlanBaseFileName params serviceParams
          return (s2, c1 + c2)

   splittedTimeFrames :: LocalTime -> LocalTime -> [LocalTime]
   splittedTimeFrames s1 e2
     = case isOfficialRatingEnv of
         True
           -> [e2]
              -- ^ an official rating env can not be splitted in smaller time-frames, otherwise the algo does not work correctly
         False
           -> case s1 < e2 of
                True
                  -> let d1 = localDay s1
                         d2 = addDays splitDays d1
                         s2 = LocalTime {
                                localTimeOfDay = localTimeOfDay s1
                              , localDay = d2
                              }
                         s2' = if s2 > e2 then e2 else s2
                     in [s2'] ++ (splittedTimeFrames s2' e2)
                False
                  -> []

-- | Rate the CDRs on the source data file.
--   This is the entry point of the rating process, and it coordinates all the rest:
--   * signal the changed days for other maintanance jobs
--   * recalculate cached sum of days
--
--   Normal calls are re-rated specifying the from and to calldate.
--   Pure service CDRS are re-rated only specifying a re-rerating on all the pending/unbilled time-frame.
--   Imported service CDRs are re-rated if:
--   * there is an explicit rerate event about service CDRs
--   * there is a official rerate event on call-date, and in this case there can be changes of rating params
--
--   The main paradigm is this:
--   * CDRs with errors are reported as CDR with the error field set, so the header in call report can report them
--   * CDRs with errors generates an error message/description
--   * CDRs rated are saved as correct CSV lines
--   * errors of type Type_Error are associated to the single CDR, and rating process continue
--   * errors of type Type_Critical interrupt the rating process, signaling as unrated all next CDRS.
--   * the rating process must be conservative: in doubt an error message is raised, and rating is blocked.
--
--   The BundleState start date (BS1), the Rate start date (R1) and Rate end (R2) date, and the last bundle-state can be closed (logically) at (BS2) calldate.
--   It is important rating all the CDRS in R2 .. BS2 because they are used for calculating the new version of the BundleState,
--   otherwise the rerating can left an inconsistent state.
--   These are the relation:
--   * normal CDRS inside R1 and BS2 are deleted, before generating again
--   * service CDRS partially inside BS1 and R2 are deleted, before generating again
--   * normal CDRs from BS1 to R1 are only rated, for updating the BundleState but:
--   ** they are not saved, because they are not deleted from the CDR table before re-rating (they are not rerated)
--   ** errors are not generated, because they are not deleted from the error table, before re-rating (they are not under garbage collection)
--   * normal CDRs from R1 to BS2 are saved, because they are deleted for regenaration
--   * errors from R1 to BS2 are saved, because they are garbage collected for regeneration
--   * ServiceCDRs (related to BundleState or pure Services) are:
--   ** generated if they are partially inside the BS1 and R2, because they are in this case deleted, before the rerating process
--
--   In case of rating of Imported Service CDRS:
--   * they do not affect the bundle-state and bundle-rates
--   * they are deleted and rerated only according their initial CDR calldate and not the CDR to_calldate, because they are managed like normal calls
--   * because they are CDRs associated to service imported from a provider, their direction is changed automaticall to "service"
--
--   @require bundleStateFromCallDate <= fromCallDate
--   @ensure write in debugFileName (debugHandle) only if isDebugMode
rateEngine_rate
  :: Bool                    -- ^ True for billing, False for call reporting
  -> ConfiguredCDRSImporters -- ^ the importers to use
  -> DBConf           -- ^ the connection params to use
  -> Maybe FilePath   -- ^ debug file name
  -> LocalTime        -- ^ rate all CDRs from this calltime, inclusive
  -> LocalTime        -- ^ rate all CDRs to this calltime, exclusive
  -> Bool             -- ^ True if it must rate Imported Service CDRS, False for importing Normal CDRS
  -> Bool
  -- ^ True if it must delete also the rating event about official call-date
  -- @require if True, then the caller is not splitting the time-frame to rate,
  -- because splitted time-frames generate too much pure services CDRs.
  -> ConfiguredRatePlanParsers
  -> FilePath
  -> FilePath
  -> EnvParams
  -> ServiceParams
  -> IO Int
  -- ^ return the number of processed CDRs.
  --   Normal errors are added directly to the error table.
  --   Critical errors are signaled using an exception.

rateEngine_rate isVoipReseller configuredCDRSImporters dbConf maybeDebugFile fromCallDate toCallDate onlyImportedServiceCDRS isOfficialRatingEnv configuredRateParsers ratePlanChanges ratePlanBaseFileName params serviceParams
  = do

     DB.initLibrary
     -- this must be done only at the beginning at for all threads

     DB.initThread

     debugChan <- newBoundedChan writeBuffSize
     errorChan <- newBoundedChan writeBuffSize
     deserializedCDRSChan <- newBoundedChan (writeBuffSize * 2)
     ratedCDRSChan <- newBoundedChan (writeBuffSize * 2)

     withAsync (debugFileProcess maybeDebugFile debugChan) $ (\debugThread -> do
      withAsyncBound (errorProcess errorChan) $ (\errorThread -> do

       ((!env) :: RatingEnv,
        (!bundleStateFromCallDate) :: CallDate,
        (!bundleStateCanBeSavedFromCallDate) :: CallDate,
        (!bundleStateToCallDate) :: CallDate,
        (!initialBundleState) :: Maybe BundleState)
           <- bracket
                (openConnection dbParams False)
                (\selectConn -> do DB.commit selectConn
                                   DB.close selectConn)
                (\selectConn -> do

                       -- Load only the income rate, and use it for determining the time-frame that must be rated.
                       -- NOTE: in theory the income-rate can change over time, and so it can change in the bundleInitStart1 calldate,
                       -- and a new version should be read, until a fix-point is not reached. But in practice this is ignored.
                       initialEnv1 <- PS.runSafeT $! ratingEnv_load ratePlanChanges ratePlanBaseFileName fromCallDate (Just toCallDate) configuredRateParsers params (Just "main-income-rate")

                       (bundleInitStart1, bundleCanBeSaved1, bundleToCallDate1)
                          <- case initialEnv1 of
                                 Left err
                                   -> throwIO  $ ErrorCall $ asterisellError_description err
                                 Right env
                                   -> case env_getRate env "main-income-rate" fromCallDate of
                                        Nothing
                                          -> throwIO $ ErrorCall $ "missing \"main-income-rate\" plan. Without this plan it is not possible start rating."
                                        Just (ratePlan, _)
                                          -> let (d1, d2) = mainRatePlan_getBundleRateStartCallDate ratePlan fromCallDate
                                                 d3 = mainRatePlan_getBundleRateEndCallDate ratePlan toCallDate
                                             in  return (d1, d2, d3)

                       -- Load the Rating Params from disk.
                       -- DEV-NOTE: now that I have the MySQL driver with direct access, I can read them directly from the database in case
                       maybeEnv :: (Either AsterisellError RatingEnv)
                            <- PS.runSafeT $! ratingEnv_load ratePlanChanges ratePlanBaseFileName bundleInitStart1 (Just bundleToCallDate1) configuredRateParsers params Nothing

                       !env
                            <- case maybeEnv of
                                 Left err
                                   -> throwIO  $ ErrorCall $ asterisellError_description err
                                 Right env
                                   -> ratingEnv_loadExtensionsToExport env selectConn

                       (when isDebugMode) (case env_respectCodeContracts env of
                                                   Right _
                                                     -> return ()
                                                   Left err
                                                     -> throwIO $ ErrorCall ("Error in application code. Internal code code contracts are not respected. " ++ err))

                       -- Return the Init Values

                       case onlyImportedServiceCDRS of
                         True
                           -> return (env, fromCallDate, fromCallDate, toCallDate, Nothing)
                         False
                            -> do let bquery1
                                        = BS.concat [
                                              "SELECT id, to_time, data_file FROM ar_bundle_state WHERE to_time <= '"
                                            , fromString $ fromLocalTimeToMySQLDateTime fromCallDate
                                            , "' ORDER BY to_time DESC LIMIT 1"
                                            ]
                                       -- NOTE: consider <= because the date is exclusive, so all the new CDRS from this date (inclusive)
                                       -- to next dates can be added to the old bundle-state. In other word, it contains all CDRS before to_time.

                                  DB.query selectConn bquery1
                                  r <- DB.storeResult selectConn
                                  maybeRow <- DB.fetchRow r
                                  case maybeRow of
                                   [_, callDate, maybeContent]
                                     -> do DB.freeResult r
                                           let savedBundleCallDate = fromJust1 "err 737" $ fromMySQLDateTimeAsTextToLocalTime $ fromMySQLResultToText callDate
                                           case savedBundleCallDate >= bundleInitStart1 of
                                             True
                                               -> case  DBS.decode $ LZ.decompress $ fromJust1 "re-537" maybeContent of
                                                    Left err
                                                      -> do debugFile_append maybeDebugFile debugChan $ "The bundle state at date " ++ (Text.unpack $ fromMySQLResultToText callDate) ++ " is corrupted. Error message: " ++ err
                                                            return $ (env, bundleInitStart1, bundleCanBeSaved1, bundleToCallDate1, Nothing)

                                                    Right (b :: BundleState)
                                                      -> do debugFile_append maybeDebugFile debugChan $ "Loaded bundle state from date " ++ (Text.unpack $ fromMySQLResultToText callDate)

                                                            return (env, savedBundleCallDate, savedBundleCallDate, bundleToCallDate1, Just b)
                                             False
                                               -> do debugFile_append maybeDebugFile debugChan  $ "process CDRS from calldate " ++ showLocalTime bundleInitStart1 ++ ", starting with an empty bundle state."
                                                     return $ (env, bundleInitStart1, bundleCanBeSaved1, bundleToCallDate1, Nothing)
                                                     -- instead of using a very old bundle state, use a recent bundle state that can be initializated to empty
                                   []
                                     -> do DB.freeResult r
                                           debugFile_append maybeDebugFile debugChan $ "*not loaded* bundle state"
                                           -- DEV-NOTE: use Nothing as BundleState because in this case it will be initializated at the starting of rating process.
                                           return $ (env, bundleInitStart1, bundleCanBeSaved1, bundleToCallDate1, Nothing))

       debugFile_append maybeDebugFile debugChan $ "bundleStateFromCallDate = " ++ showLocalTime bundleStateFromCallDate ++ ", bundleStateCanBeSavedFromCallDate = " ++ showLocalTime bundleStateCanBeSavedFromCallDate ++ ", bundleStateToCallDate = " ++ showLocalTime bundleStateToCallDate

       totCDRS2 <- withAsyncBound (writeProcess env debugChan errorChan ratedCDRSChan) (\writeThread -> do

           let ratingStatus1
                  = ratingStatus_empty { rs_incomeBundleState = initialBundleState }


           let d0 = fromString $ "'" ++ fromLocalTimeToMySQLDateTime bundleStateFromCallDate ++ "'"
           let d1 = fromString $ "'" ++ fromLocalTimeToMySQLDateTime fromCallDate ++ "'"
           let d2 = fromString $ "'" ++ fromLocalTimeToMySQLDateTime toCallDate ++ "'"
           let d3 = fromString $ "'" ++ fromLocalTimeToMySQLDateTime bundleStateCanBeSavedFromCallDate ++ "'"
           let d4 = fromString $ "'" ++ fromLocalTimeToMySQLDateTime bundleStateToCallDate ++ "'"

           -- The temporal order is
           -- d0 <= d3 <= d1 < d2 <= d4

           -- Remove old CDRs, and BundleState CDRs and pure Service CDRS
           -- The contract to respect is:
           -- 1. if a type of CDR is deleted, then it is also generated and saved
           -- 2. if a type of CDR is not deleted, then it is not saved
           -- 3. optionally a CDR can be rated, for updating some state, but not saved
           -- 4. all CDRS of all time-frames are generated in case continuous rating event are processed

           let importedServicesCondition
                 = ["(is_imported_service_cdr = 1 AND calldate >= ", d1, " AND calldate < " , d4, ")"]

           case onlyImportedServiceCDRS of
             False
               -> do -- Delete normal CDRS inside the rating period. Consider the end of the bundle as final rating period
                     -- because these CDRs can have different cost/income, if the previous CDRs are changed, according the BundleRate specifications.
                     -- These CDRs respect the contracts, because:
                     -- 1. the stream of CDRS are all the CDRS between d0 and d4
                     -- 2. only CDRS within d1 and d4 are deletede, and then saved
                     --
                     -- The BundleRate derived serviceCDRS works if "cdr.calldate < cdr.to_calldate", and it does not work if "cdr.calldate = cdr.to_calldate"
                     -- that is not the case service cdrs. The service CDRS with cdr.to_calldate > d1 and cdr.to_calldate <= d4, are the correct
                     -- CDRS to re-process because:
                     -- * there can not be (by construction of bundleStateToCallDate) CDRS after d3. In case they are related to other bundle state time-frame
                     --   starting from d3
                     -- * the normal CDRS before d1 are not saved, so also the bundle-state related service CDRS can not be saved
                     --
                     -- For the pure serviceCDRS use a trick:
                     -- * the d4 date can be not a good enough limit for services, that can be rated with different time-frames,
                     --   respect pure service-cdrs
                     -- * services need recalculation only if there is a change in services assignations/definitions
                     -- * this type of event generate a rerate all unbilled calls event
                     -- * so services are deleted and generate only in this situation
                     -- * this simple algo work, because the rating event of all unrated calls, is never split
                     -- So the code for complex incremental rerating is done only for bundle state services, that have a state are complex
                     -- to manage. Services are re-generated from scratch, with an easier algo only when it is needed.
                     -- Generating service-cdrs also in next dates, requires the generation of many changed days, it is not clear when stop,
                     -- and so it is too much heavy, and in the end not necessary because for sure there were no changes in calculated values.
                     --
                     -- Delete and generate imported services, when global params of rating are changed.
                     let deleteQuery1 = [
                                          "DELETE FROM ar_cdr WHERE (is_imported_service_cdr = 0 AND ((to_calldate IS NULL AND calldate >= ", d1
                                        , " AND calldate < " , d4
                                        , ") OR (to_calldate IS NOT NULL AND debug_cost_rate IS NOT NULL AND debug_income_rate IS NOT NULL AND "
                                        , " to_calldate > ", d0, " AND to_calldate <= ", d4, ") "
                                        ]

                     let deleteQuery2
                           = case isOfficialRatingEnv of
                               True
                                 -> [  " OR (to_calldate IS NOT NULL AND debug_cost_rate IS NULL AND debug_income_rate IS NULL AND "
                                    ,  " calldate >= ", d1, "))) OR "] ++ importedServicesCondition
                               False
                                 -> [ "))" ]

                     let deleteQuery = BS.concat $ deleteQuery1 ++ deleteQuery2

                     -- wait the end of this transaction, because next transactions will retrieve data from the database,
                     -- and they must start when the writer is ready to process them, otherwise the DB process can stall
                     -- and return an error.
                     debugFile_append maybeDebugFile debugChan  $ "rate main pass2: execute query: " ++ (Text.unpack $ fromMySQLResultToText $ Just deleteQuery)
                     w <- newEmptyMVar
                     writeChan ratedCDRSChan (WriteCmd_exec (Just w) deleteQuery)
                     cmdWait_wait w

                     writeChan ratedCDRSChan (WriteCmd_exec (Just w) $ BS.concat [ "DELETE FROM ar_bundle_state WHERE to_time >= ", d3])
                     cmdWait_wait w

             True
               -> do -- Delete imported service cdrs inside the rating period.
                     -- They will be imported again from ar_source_cdr, according their calldate, so only their calldate is taken in consideartion, and not to_calldate.
                     -- Imported service cdrs does not affect bundle-state, so the toCallDate is used.
                     let deleteQuery1
                           = BS.concat $ ["DELETE FROM ar_cdr WHERE "] ++ importedServicesCondition
                     w <- newEmptyMVar
                     writeChan ratedCDRSChan (WriteCmd_exec (Just w) deleteQuery1)
                     cmdWait_wait w

           -- Start the rating process, using a chain of threads passing information on channels.
           -- Every thread perform a distinct pass of the CDR rating process.
           -- The idea it is that each thread can work at full speed exploiting the CPU cache, processing a constant flow of data.
           -- IMPORTANT: stream queries of MySQL requires that they are processed reasonably fast, at level of each single record,
           -- otherwise the connection will be discarded, or something of similar.
           -- Up to date this is the case, because the speed of reading, is comparable with the speed of rating and writing to the database.
           (!totCDRS, !ratingStatus2, !lastProcessedCallDate) <-
             withAsyncBound (readProcess debugChan deserializedCDRSChan bundleStateFromCallDate bundleStateToCallDate) $ \thread1 ->
                 withAsyncBound (rateProcess debugChan errorChan deserializedCDRSChan env ratingStatus1 bundleStateFromCallDate bundleStateCanBeSavedFromCallDate bundleStateToCallDate ratedCDRSChan) $ \thread3 -> do
                     (r2, d) <- wait thread3
                     r1 <- wait thread1
                     return (r1, r2, d)

           -- Write ExpandedExtensions.
           -- NOTE: in case expanded extensions are managed, then only new extensions will be written because
           -- they are not recognized with extension to expand any more.
           -- Otherwise any time all the used extensions will be written in this table, but it is not a big cost.
           mapM_ (\ee -> writeChan ratedCDRSChan (WriteCmd_insertExpandedExtension ee)) (S.toList $ rs_expandedExtensions ratingStatus2)

           -- Send the signal about the end of rating process, and close the db write thread.
           -- The WriteCmd_close take care of completing all the derived info, about changed days, and grouped sums.
           writeChan ratedCDRSChan (WriteCmd_close bundleStateToCallDate)
           wait writeThread
           return totCDRS)

       writeChan errorChan ErrorCmd_close
       writeChan debugChan (Left ())
       wait errorThread
       wait debugThread

       return totCDRS2))

 where

   dbParams = DB.defaultConnectInfo {
                     connectUser = dbConf_user dbConf
                   , connectPassword = dbConf_password dbConf
                   , connectDatabase = dbConf_dbName dbConf
                   , connectOptions = [Reconnect False, CharsetName "utf8"]
                   }


   isDebugMode = isJust maybeDebugFile

   rateGarbageKey = fromString $ "rating-process"

   currencyPrecision = params_currencyPrecision params

   insideRateCallTime' bundleToCallDate cdr = insideRateCallTime'' bundleToCallDate (cdr_calldate cdr)

   insideRateCallTime'' bundleToCallDate callDate = validCallTime False callDate fromCallDate (Just bundleToCallDate)

   -- | A wrap function sending the error to the error process
   sendError
     :: BoundedChan ErrorCmd
     -> Bool
     -- ^ True for writing the error in the database
     -> AsterisellError
     -- ^ the error to write
     -> Maybe ( BS.ByteString
                -- ^ the garbage key
              , LocalTime
                -- ^ error from date
              , LocalTime
                -- ^ error to date
              )
        -- ^ Nothing for no garbage key
     -> IO AsterisellErrorDupKey

   sendError errorChan writeToDB err1 maybeGarbageKey
     = let dupKey1 = asterisellError_dupKey err1
       in  do writeChan errorChan (ErrorCmd_insert writeToDB err1 maybeGarbageKey)
              return dupKey1

   -- | Send errors to the database.
   errorProcess :: BoundedChan ErrorCmd -> IO ()
   errorProcess errorChan
     = let processRequests :: DB.Connection -> AsterisellErrorsDictionary -> IO ()
           processRequests errorConn errorDict1 = do
             !c <- readChan errorChan
             case c of
               ErrorCmd_close
                 -> return ()
               ErrorCmd_insert writeToDB !asterisellError !maybeGarbageKeyInfo
                 -> do (_, errorDict2)
                          <- case maybeGarbageKeyInfo of
                               Just (garbageKey, errorFromDate, errorToDate)
                                 -> writeError writeToDB errorConn errorDict1 asterisellError garbageKey errorFromDate errorToDate
                               Nothing
                                 -> writeErrorWithoutGarbageCollection writeToDB errorConn errorDict1 asterisellError
                       processRequests errorConn errorDict2

       in bracketOnError
            (do DB.initThread
                openConnection dbParams True)
            (\errorConn -> -- sometime these transactions can not be closed, but I want to report the original error message.
                           Control.Monad.Catch.handleAll (\ _ -> return ()) $ do
                             DB.commit errorConn
                             -- I want save errors in any case
                             DB.close errorConn)

            (\errorConn -> do processRequests errorConn asterisellErrorsDictionary_empty
                              DB.commit errorConn
                              DB.close errorConn)

   -- | Read source CDRS from database, deserialize them,  and send to the rating process.
   readProcess :: DebugChan -> BoundedChan RateCmd -> CallDate -> CallDate -> IO Int
   readProcess debugChan chan1 bundleStateFromCallDate bundleStateToCallDate
     = let readLoop :: DB.Connection -> DB.Result -> Int -> IO Int
           readLoop streamConn r !c
             = do !maybeR <- DB.fetchRow r
                  case maybeR of
                    [] -> do -- send the signal about the end of CDRs
                             writeChan chan1 RateCmd_close
                             return c
                    [row_id, row_calldate, row_formatName, row_versionName, row_providerName, row_content, row_formatId, row_providerId]
                      -> do !void <- deserializeSourceCDRAndSend (fromJust1 "c1333" $! row_id, fromJust1 "c1333" $! row_calldate, fromJust1 "c1333" $! row_formatName, fromJust1 "c1333" $! row_versionName, fromJust1 "c1777" $! row_formatId, fromJust1 "c1333" $! row_providerName, fromJust1 "c17537" $! row_providerId, fromJust1 "c1333" $! row_content)
                            readLoop streamConn r (c + 1)
                    _ -> throwIO $ ErrorCall "Unexpected query row format in code (ERR 125537)."

           importedServiceQuery
             = case onlyImportedServiceCDRS of
                     True
                       -> " AND is_imported_service_cdr = 1 "
                     False
                       -> case isOfficialRatingEnv of
                            True
                              -> ""
                                 -- accept both imported services, and normal cdrs
                            False
                              -> " AND is_imported_service_cdr = 0 "
                                 -- do not process importe service cdrs, because they are not deleted, and they are not changed


           streamQuery
                 = BS.concat ["SELECT s.id, s.calldate, l.name, p.name, pv.internal_name, s.content, p.id, pv.id FROM ar_source_cdr AS s USE INDEX(ar_source_cdr_I_1) INNER JOIN ar_physical_format AS p ON s.ar_physical_format_id = p.id INNER JOIN ar_logical_source AS l ON p.ar_logical_source_id = l.id INNER JOIN ar_cdr_provider AS pv ON s.ar_cdr_provider_id = pv.id WHERE calldate >= '", fromString $ fromLocalTimeToMySQLDateTime bundleStateFromCallDate, "' AND calldate < '", fromString $ fromLocalTimeToMySQLDateTime bundleStateToCallDate, "' ", importedServiceQuery, fromString "  ORDER BY calldate"]


           deserializeSourceCDRAndSend :: RawSourceCDR -> IO ()
           deserializeSourceCDRAndSend (!sourceCDRId, !callDateS, !formatName, !formatVersion, !formatId, !cdrProviderName, !cdrProviderId, !sourceCDR)
             = let errMsg = "(ERR 1753) There is no known source data file importer, for CDR with provider name " ++ (Text.unpack $ fromMySQLResultToText $ Just cdrProviderName) ++ ", format " ++ (Text.unpack $ fromMySQLResultToText $ Just formatName) ++ ", version " ++ (Text.unpack $ fromMySQLResultToText $ Just formatVersion)
                   callDate = fromJust1 "re-75375" $ fromMySQLDateTimeAsTextToLocalTime $ fromMySQLResultToText $ Just callDateS

                   providerNameT = fromMySQLResultToText $ Just cdrProviderName

               in do case configuredCDRSImporters_get configuredCDRSImporters formatName formatVersion of
                       Nothing
                         -> throwIO $! ErrorCall errMsg
                       Just (_, _, !cdrImporter)
                         -> case snd $! cdrImporter (LBS.fromStrict sourceCDR) providerNameT of
                              Left !err
                                -> writeChan chan1 $! (RateCmd_rate ((cdr_empty callDate currencyPrecision) { cdr_direction = CDR_outgoing }, sourceCDRId, formatName, formatVersion, formatId, cdrProviderName, cdrProviderId, sourceCDR, Left err))
                                    -- the error will be raised from the receiving process

                              Right !cdrs1
                                -> let newDirection cdr = if (cdr_isServiceCDR cdr) then CDR_system else (cdr_direction cdr)
                                       !cdrs2 = List.map (\(cdr, descr) -> (cdr { cdr_isImportedServiceCDR = (cdr_isServiceCDR cdr), cdr_direction = (newDirection cdr)}, descr)) cdrs1
                                       -- By definition a service CDR imported from a ar_source_cdr is a imported service CDR.
                                       -- An imported service, has direction CDR_system by default.

                                   in  mapM_ (\(cdr, descr) -> writeChan chan1 $! (RateCmd_rate (cdr, sourceCDRId, formatName, formatVersion, formatId, cdrProviderName, cdrProviderId, sourceCDR, Right descr))) cdrs2


       in bracket (do DB.initThread
                      openConnection dbParams False)
                  (\streamConn -> do DB.commit streamConn
                                     DB.close streamConn) $ \streamConn -> do

                    -- Send an init CDR to the chain of rating processes. In this way at least one CDR is generated,
                    -- and other process can start their initialization work.
                    let cdr = (cdr_empty bundleStateFromCallDate 4) { cdr_direction = CDR_ignored, cdr_billsec = Just 0, cdr_duration = Just 0 }
                    writeChan chan1 $! (RateCmd_rate (cdr, "0", "", "", "0", "", "0", "", Right ""))

                    -- Start processing the stream of source CDRS.
                    DB.query streamConn streamQuery
                    r <- DB.useResult streamConn
                    result <- readLoop streamConn r 0
                    DB.freeResult r
                    return result

   -- | Rate CDRS and send to the proper channel.
   --   Do not send the end signal to the write channel.
   rateProcess :: DebugChan -> BoundedChan ErrorCmd -> BoundedChan RateCmd -> RatingEnv -> RatingStatus -> CallDate -> CallDate -> CallDate -> BoundedChan WriteCmd -> IO (RatingStatus, CallDate)
   rateProcess debugChan errorChan sourceCDRSChan env ratingStatus bundleStateFromCallDate bundleStateCanBeSavedFromCallDate bundleStateToCallDate destChan
     = let

           -- | Intercept an error, and generate the CDR with the error stats.
           --   Throw an IO exception in case of critical error.
           processError :: CDRToRate -> AsterisellError -> RatingMonad ()
           processError (!cdr, !sourceCDRId, !logicalTypeName, !versionName, !versionId, !providerName, !providerId, sourceCDR, maybeCdrDebugDescription) err
             = let
                   date1 = cdr_calldate cdr
                   date2  = case cdr_toCalldate cdr of
                              Nothing
                                -> date1
                              Just d
                                -> d

                   cdrDebugDescription
                     = case maybeCdrDebugDescription of
                           Left e -> "<cdr high level view not available>"
                           Right d -> d

                   errDescription
                     = asterisellError_description err ++ "\n\nImported CDR:\n\n" ++ cdr_showDebug cdr ++ "\n\nSource CDR of type " ++ (Text.unpack $ fromMySQLResultToText $ Just logicalTypeName) ++ "__" ++ (Text.unpack $ fromMySQLResultToText $ Just versionName) ++ ":\n\n" ++ cdrDebugDescription ++ "\n\nOriginal source content:\n\n" ++ (Text.unpack $ fromMySQLResultToText (Just sourceCDR)) ++ "\n\n"

                   err2 = err { asterisellError_description = errDescription
                              , asterisellError_effect = "This CDR will be not rated. " ++ asterisellError_effect err
                              , asterisellError_proposedSolution = asterisellError_proposedSolution err ++ "If there are many errors of this type, there can be a problem in the input format specification or in the application code. In case contact the assistance. "
                              }

               in case asterisellError_type err of
                    Type_Critical
                      -> liftIO $ throwIO $ ErrorCall errDescription
                    _ -> case insideRateCallTime' bundleStateToCallDate cdr of
                          False
                             -> do return ()
                          True
                             -> do
                                   dupKey <- liftIO $! sendError errorChan True err2 (Just (rateGarbageKey, date1, date2))
                                   let errorCDR = cdr_convertToError cdr (Just dupKey)
                                   liftIO $! writeChan destChan (WriteCmd_insert errorCDR bundleState_empty)
                                   return ()

           rateProcessPass :: DB.Connection -> CDRToRate -> RatingStatus -> RatingMonad (RatingStatus, CallDate)
           rateProcessPass selectConn cdrToRate@(cdr, sourceCdrId, logicalTypeName, versionName, versionIdS, providerName, providerIdS, sourceCDR, errorOrCdrDebugDescription) state1
             = case errorOrCdrDebugDescription of
                 Left !err -> do processError cdrToRate err
                                 return $!! (state1, cdr_calldate cdr)

                 Right !cdrDebugDescription -> (do
                    let cdrTime = cdr_calldate cdr

                    -- if the bundle-time-frame is closed, then generated related services CDRS.
                    (!state2, !serviceCDRS1) <- updateRatingStatusAndReturnServiceCDRS env destChan state1 bundleStateFromCallDate bundleStateCanBeSavedFromCallDate CostRate cdrTime cdr
                    (!state3, !serviceCDRS2) <- updateRatingStatusAndReturnServiceCDRS env destChan state2 bundleStateFromCallDate bundleStateCanBeSavedFromCallDate IncomeRate cdrTime cdr

                    let providerId = fromJust1 ("p555: providerId " ++ fromByteStringToString providerIdS) $ fromByteStringToInt providerIdS
                    let versionId = fromJust1 "p556" $ fromByteStringToInt versionIdS

                    (!ratedCDR, !state4) <- rateCDR selectConn cdrTime env state3 providerId versionId cdr

                    when (insideRateCallTime' bundleStateToCallDate ratedCDR && cdr_direction ratedCDR /= CDR_ignored)
                         (liftIO $! (writeChan destChan $!! (WriteCmd_insert ratedCDR (bundleStateOrEmpty $!! rs_incomeBundleState $!! state4))))

                    processServiceCDRS selectConn $!! (serviceCDRS1 ++ serviceCDRS2)
                    return $!! (state4, cdrTime)) `catchError` (\(errCDR, err) -> do processError (errCDR, sourceCdrId, logicalTypeName, versionName, versionIdS, providerName, providerIdS, sourceCDR, Right cdrDebugDescription) err
                                                                                     return $!! (state1, cdr_calldate cdr))


           bundleStateOrEmpty :: Maybe BundleState -> BundleState
           bundleStateOrEmpty Nothing = bundleState_empty
           bundleStateOrEmpty (Just b) = b

           processServiceCDR :: DB.Connection -> ServiceCDR -> RatingMonad ServiceCDR
           processServiceCDR !selectConn !serviceCDR
             = do (serviceCDR', _) <- cdr_initialClassification (env_params env) selectConn serviceCDR
                  liftIO $! writeChan destChan $!! (WriteCmd_insert serviceCDR' bundleState_empty)
                  return serviceCDR'

           processServiceCDRS :: DB.Connection -> [ServiceCDR] -> RatingMonad ()
           processServiceCDRS !selectConn !serviceCDRS
             = do mapM_ (processServiceCDR selectConn) serviceCDRS

           processCloseRequest :: DB.Connection -> (RatingStatus, CallDate) -> RatingMonad ()
           processCloseRequest selectConn (!state1, !lastCallDate)
             = do      -- Save the incremental BundleState. This value is associated to the last processed CDR, so there are no new CDRS in the DB
                       -- after this date (according the logical time-frame). If new CDRS will be added, they will have (probably) a bigger calldate,
                       -- and so the rating can start from this saved BundleState.
                       -- I can not use for this porpouse bundleStateToCallDate because it is date far in future. In reality I'm not interested to it.
                       when (not onlyImportedServiceCDRS)
                            (case rs_incomeBundleState state1 of
                                Nothing
                                  -> return ()
                                Just bundleState1
                                  -> liftIO $ writeChan destChan $!! (WriteCmd_saveBundleState lastCallDate bundleState1))

                       -- Generate BundleRate serviceCDRS at bundleStateToCallDate.
                       -- I can generate always these CDRs, because they are deleted before rating them.
                       -- Doing so I'm generating pending service-cdrs for not already closed time-frame,
                       -- or if I'm rerating an already closed time-frame, I'm updating with new values.
                       when (not onlyImportedServiceCDRS)
                            (do case (rs_incomeBundleState state1, rs_incomeRate state1) of
                                     (Just bundleState2, Just (mainRatePlan2, _))
                                       -> let (_, _, pendingServiceCDRS1) = bundleState_serviceCdrs env mainRatePlan2 bundleState2 bundleStateToCallDate
                                              pendingServiceCDRS2 = List.filter (\cdr -> (fromJust1 "175377" $ cdr_toCalldate cdr) > fromCallDate) pendingServiceCDRS1
                                          in  processServiceCDRS selectConn pendingServiceCDRS2
                                     _ -> return ())

                       -- Generate serviceCDRS for pure services, in the rating time frame.
                       when ((not onlyImportedServiceCDRS) && isOfficialRatingEnv)
                            (do case service_generate isDebugMode env serviceParams fromCallDate bundleStateToCallDate True of
                                     Left errMsg
                                       -> throwError $ (cdr_empty fromCallDate 4
                                                       ,createError
                                                          Type_Critical
                                                          Domain_RATES
                                                          errMsg
                                                          ("Error in specification of Services. " ++ errMsg)
                                                          ("Corresponding ServiceCDRs will be not produced, and inserted in call report. The call report does not contain stats about these missing service CDRs.")
                                                          ("Correct the error in Service specification, and rerate them."))

                                     Right cdrs
                                       -> processServiceCDRS selectConn cdrs)

                       return ()

           -- | Some extensions and related CDRs must be exported to other instances.
           processExtensionToExport :: RatingEnv -> CDRToRate -> IO ()
           processExtensionToExport state1 (cdr, sourceId, _, _, _, _, _, _, _)
             = case trie_getMatch trie_getMatch_initial (env_extensionsToExport state1) (Text.unpack $ cdr_internalTelephoneNumber cdr) of
                 Nothing
                   -> return ()
                 Just _
                   -> liftIO $ writeChan destChan $!! (WriteCmd_insertSourceCDRToExport sourceId)

           processRequests :: DB.Connection -> (RatingStatus, CallDate) -> IO (RatingStatus, CallDate)
           processRequests selectConn (!state1, !lastCallDate) = do
             !maybeInput <- readChan sourceCDRSChan
             case maybeInput of
               RateCmd_close
                 -> do !state2OrError <- runExceptT $! processCloseRequest selectConn (state1, lastCallDate)
                       case force state2OrError of
                         Right ()
                           -> return (state1, lastCallDate)
                         Left (errCdr, err)
                           -> throwIO $ ErrorCall $ "(err 75375) " ++ asterisellError_userShow err

               RateCmd_rate !cdrToRate
                 -> do processExtensionToExport env cdrToRate
                       !state2OrError <- runExceptT $! rateProcessPass selectConn cdrToRate state1
                       case force state2OrError of
                         Right (!state2, !lastCallDate2)
                           -> processRequests selectConn (force state2, force lastCallDate2)
                         Left (!errCdr, !err)
                           -> throwIO $ ErrorCall $ "(err 75377) Error in code. The application raised an unexpected error, that should instead managed internally in CDR code processing. " ++ asterisellError_userShow err

               RateCmd_rateService !serviceCDR
                 -> do !cdrOrError <- runExceptT $! processServiceCDR selectConn serviceCDR
                       case force cdrOrError of
                         Right !cdr2
                           -> processRequests selectConn (force state1, force lastCallDate)
                         Left (!errCdr, !err)
                           -> throwIO $ ErrorCall $ "(err 755) Error during rating of service CDR.\n\n" ++ cdr_showDebug serviceCDR ++ "\n\n" ++ asterisellError_userShow err

       in bracketOnError
            (do DB.initThread
                openConnection dbParams False)
            (\selectConn -> -- sometime these transactions can not be closed, but I want to report the original error message.
                            Control.Monad.Catch.handleAll (\ _ -> return ()) (do DB.commit selectConn
                                                                                 DB.close selectConn))
            (\selectConn -> do r <- processRequests selectConn (ratingStatus, bundleStateFromCallDate)
                               DB.commit selectConn
                               DB.close selectConn
                               return r)

   -- | How many CDRS buffering before sending to the database.
   writeBuffSize :: Int
   writeBuffSize = 200

   -- | Write CDRs on the database.
   --   Store some CDRS, before sending them in batch mode to the database, because this operation is faster than a single send.
   writeProcess :: RatingEnv -> BoundedChan (Either () String) -> BoundedChan ErrorCmd -> BoundedChan WriteCmd -> IO ()
   writeProcess env debugChan errorChan ratedCDRSChan
     = let currencyPrecision = params_currencyPrecision $ env_params env

           escapeCDRMaybeText :: DB.Connection -> (CDR -> Maybe Text.Text) -> CDR -> IO (Maybe BS.ByteString)
           escapeCDRMaybeText writeConn f cdr
             = case f cdr of
                 Nothing -> return Nothing
                 Just t -> do s <- DB.escape writeConn $ fromTextToMySQLResult t
                              return $ Just s

           escapeCDRText :: DB.Connection -> (CDR -> Text.Text) -> CDR -> IO BS.ByteString
           escapeCDRText writeConn f cdr
             = DB.escape writeConn $ fromTextToMySQLResult $ f cdr

           escapeCDR :: DB.Connection -> CDR -> IO EscapedCDR
           escapeCDR writeConn cdr
             = do s1 <- escapeCDRText writeConn cdr_internalTelephoneNumber cdr
                  s2 <- escapeCDRText writeConn cdr_externalTelephoneNumber cdr
                  s3 <- escapeCDRMaybeText writeConn cdr_channel cdr
                  s4 <- escapeCDRMaybeText writeConn cdr_externalTelephoneNumberWithAppliedPortability cdr
                  s5 <- escapeCDRMaybeText writeConn cdr_displayedExternalTelephoneNumber cdr
                  s10 <- escapeCDRMaybeText writeConn cdr_displayedMaskedExternalTelephoneNumber cdr
                  s6 <- escapeCDRMaybeText writeConn cdr_problemDuplicationKey cdr
                  s7 <- escapeCDRMaybeText writeConn cdr_debug_cost_rate cdr
                  s8 <- escapeCDRMaybeText writeConn cdr_debug_income_rate cdr
                  s9 <- escapeCDRMaybeText writeConn cdr_debug_residual_income_rate cdr
                  s11 <- escapeCDRMaybeText writeConn cdr_debug_rating_details cdr
                  return EscapedCDR {
                           ecdr_internalTelephoneNumber = s1
                         , ecdr_externalTelephoneNumber = s2
                         , ecdr_channel = s3
                         , ecdr_externalTelephoneNumberWithAppliedPortability = s4
                         , ecdr_displayedExternalTelephoneNumber = s5
                         , ecdr_displayedMaskedExternalTelephoneNumber = s10
                         , ecdr_problemDuplicationKey = s6
                         , ecdr_debug_cost_rate = s7
                         , ecdr_debug_income_rate = s8
                         , ecdr_debug_residual_income_rate = s9
                         , ecdr_debug_rating_details = s11
                         }

           exportBool :: Bool -> Builder
           exportBool True = charUtf8 '1'
           exportBool False = charUtf8 '0'

           exportInt :: Maybe Int -> Builder
           exportInt Nothing = stringUtf8 "NULL"
           exportInt (Just i) = intDec i

           -- | Export something similar to info_getParentHiearchyIds
           exportIds :: Maybe ParentIdHierarchy -> Builder
           exportIds Nothing = stringUtf8 "NULL"
           exportIds (Just ids)
             = let convertedIds = mconcat $ List.map (\i -> intDec i `mappend` charUtf8 '/') ids
               in  charUtf8 '\'' `mappend` charUtf8 '/' `mappend` convertedIds `mappend` charUtf8 '\''

           exportBS :: Maybe BS.ByteString -> Builder
           exportBS Nothing = stringUtf8 "NULL"
           exportBS (Just v) = charUtf8 '\'' `mappend` byteString v `mappend` charUtf8 '\''

           exportMoney :: Maybe MonetaryValue -> Builder
           exportMoney Nothing = stringUtf8 "NULL"
           exportMoney (Just m) = intDec $ toMonetaryValueWithFixedPrecisionInt currencyPrecision m

           exportCDR :: (CDR, EscapedCDR) -> [Builder]
           exportCDR (cdr, ecdr)
             =    [
                    charUtf8 '('
                  , exportLocalTime $ Just $ cdr_calldate cdr
                  , charUtf8 ','
                  , exportLocalTime $ cdr_toCalldate cdr
                  , charUtf8 ','
                  , intDec $ cdr_countOfCalls cdr
                  , charUtf8 ','
                  , exportBool $ cdr_isImportedServiceCDR cdr
                  , charUtf8 ','
                  , intDec $ cdrDirection_asterisellCode $ cdr_direction cdr
                  , charUtf8 ','
                  , exportBool $ cdr_isRedirect cdr
                  , charUtf8 ','
                  , exportInt $ cdr_duration cdr
                  , charUtf8 ','
                  , exportInt $ cdr_billsec cdr
                  , charUtf8 ','
                  , exportInt $ cdr_organizationUnitId cdr
                  , charUtf8 ','
                  , exportIds $ cdr_cachedParentIdHierarchy cdr
                  , charUtf8 ','
                  , exportInt $ cdr_billableOrganizationUnitId cdr
                  , charUtf8 ','
                  , exportInt $ cdr_bundleOrganizationUnitId cdr
                  , charUtf8 ','
                  , exportMoney $ Just $ cdr_income cdr
                  , charUtf8 ','
                  , exportMoney $ Just $ cdr_costSaving cdr
                  , charUtf8 ','
                  , exportInt $ cdr_vendorId cdr
                  , charUtf8 ','
                  , exportInt $ cdr_communicationChannelTypeId cdr
                  , charUtf8 ','
                  , exportMoney $ Just $ cdr_cost cdr
                  , charUtf8 ','
                  , exportMoney $ cdr_expectedCost cdr
                  , charUtf8 ','
                  , exportInt $ cdr_telephonePrefixId cdr
                  , charUtf8 ','
                  , exportBS $ Just $ ecdr_externalTelephoneNumber ecdr
                  , charUtf8 ','
                  , exportBS $ ecdr_externalTelephoneNumberWithAppliedPortability ecdr
                  , charUtf8 ','
                  , exportBS $ ecdr_displayedMaskedExternalTelephoneNumber ecdr
                  , charUtf8 ','
                  , exportInt $ Just $ cdrDirection_asterisellCode $ cdr_errorDirection cdr
                  , charUtf8 ','
                  , exportBS $ ecdr_problemDuplicationKey ecdr
                  , charUtf8 ','
                  , exportBS $ ecdr_debug_cost_rate ecdr
                  , charUtf8 ','
                  , exportBS $ ecdr_debug_income_rate ecdr
                  , charUtf8 ','
                  , exportBS $ ecdr_debug_residual_income_rate ecdr
                  , charUtf8 ','
                  , exportInt $ cdr_debug_residual_call_duration cdr
                  , charUtf8 ','
                  , exportInt $ cdr_debug_bundle_left_calls cdr
                  , charUtf8 ','
                  , exportInt $ cdr_debug_bundle_left_duration cdr
                  , charUtf8 ','
                  , exportMoney $ cdr_debug_bundle_left_cost cdr
                  , charUtf8 ','
                  , exportBS $ ecdr_debug_rating_details ecdr
                  , charUtf8 ')'
                  ]

           -- | Export the CDRS, in inverse order respect the specified list.
           exportCDRS :: [(CDR, EscapedCDR)] -> Builder
           exportCDRS cdrs
             = let f r (cdr, escapedCDR) = (exportCDR (cdr, escapedCDR)):r
               in  mconcat $ List.concat $ List.intersperse [charUtf8 ','] $ List.foldl' f [] cdrs

           -- | Write the CDRS in reverse order respect the specified list.
           writeCachedCDRSOnDB :: DB.Connection -> [(CDR, EscapedCDR)] -> IO ()
           writeCachedCDRSOnDB writeConn cdrs
             = let cmd1 = stringUtf8 "INSERT INTO ar_cdr(calldate,to_calldate, count_of_calls, is_imported_service_cdr, destination_type, is_redirect, duration, billsec, ar_organization_unit_id, cached_parent_id_hierarchy, billable_ar_organization_unit_id, bundle_ar_organization_unit_id, income, cost_saving, ar_vendor_id, ar_communication_channel_type_id, cost, expected_cost, ar_telephone_prefix_id, cached_external_telephone_number, external_telephone_number_with_applied_portability, cached_masked_external_telephone_number, error_destination_type, ar_problem_duplication_key, debug_cost_rate, debug_income_rate, debug_residual_income_rate, debug_residual_call_duration, debug_bundle_left_calls, debug_bundle_left_duration, debug_bundle_left_cost, debug_rating_details) VALUES" `mappend` exportCDRS cdrs

                   insertCmd = LBS.toStrict $ toLazyByteString cmd1

               in when (not $ List.null cdrs) $ do
                    DB.query writeConn insertCmd

           processRequests :: DB.Connection -> Set.Set Day -> ([(CDR, EscapedCDR)], Int) -> IO ()
           processRequests writeConn daysWithServiceCDRS (cachedCDRS, buffSize) = do
             maybeInput <- readChan ratedCDRSChan
             case maybeInput of
               WriteCmd_close bundleStateToCallDate
                 -> do -- write buffered CDRS
                       writeCachedCDRSOnDB writeConn cachedCDRS

                       let (normalDays, yearsAndMonths) = timeFrame_getYearsAndMonth (fromCallDate, bundleStateToCallDate)
                       rateEngine_updateCachedGroupedCDRS writeConn (Just (fromCallDate, bundleStateToCallDate))

                       -- Signal to external jobs the changed days.
                       -- For nomal CDRS they can be only the days in the rating time-frame. This because all the CDRS in this range are deleted and regenerated.
                       -- For serviceCDRS there are few days that are signaled apart. They are signaled apart, also because the external jobs want know when there are normal days and special days
                       let addChangeEvent d s
                             = let d1 = LocalTime { localDay = d, localTimeOfDay = midnight }
                               in  fromStringToByteString $ List.concat ["CALL add_daily_status_change_event('", fromLocalTimeToMySQLDateWith00Time d1,"', ", if s then "1" else "0", ")"]

                       mapM_ (\d -> do DB.query writeConn (addChangeEvent d False)) (Set.toList normalDays)
                       mapM_ (\d -> do DB.query writeConn (addChangeEvent d True)) (Set.toList daysWithServiceCDRS)

                       -- end of recursions/loops, so the processing of commands is terminating

               WriteCmd_exec (maybeWait) cmd
                 -> do DB.query writeConn cmd
                       case maybeWait of
                         Just w -> cmdWait_wake w
                         Nothing -> return ()
                       processRequests writeConn  daysWithServiceCDRS (cachedCDRS, buffSize)

               WriteCmd_saveBundleState !toCallDate !bundleState
                 -> do
                       serializatedBundleState <- DB.escape writeConn $ LZ.compress $ DBS.encode bundleState
                       let q = BS.concat [
                                 "INSERT INTO ar_bundle_state(to_time, data_file) VALUES('"
                               , fromString $ fromLocalTimeToMySQLDateTime toCallDate, "', '"
                               , serializatedBundleState, "')"
                               ]
                       DB.query writeConn q
                       processRequests writeConn daysWithServiceCDRS (cachedCDRS, buffSize)

               WriteCmd_insert !cdr1 !bundleStateToIgnore
                 -> do let !daysWithServiceCDRS2
                             = if cdr_isServiceCDR cdr1
                               then Set.insert (localDay $ cdr_calldate cdr1) daysWithServiceCDRS
                               else daysWithServiceCDRS

                       !escapedCdr1 <- escapeCDR writeConn cdr1
                       let !cachedCDRS2 = ((cdr1, escapedCdr1):cachedCDRS)
                       case buffSize >= writeBuffSize of
                          False
                            -> processRequests writeConn daysWithServiceCDRS2 $!! (cachedCDRS2, buffSize + 1)
                          True
                            -> do writeCachedCDRSOnDB writeConn $!! cachedCDRS2
                                  processRequests writeConn daysWithServiceCDRS2 ([], 0)

               WriteCmd_insertSourceCDRToExport !i
                 -> do let query1 = ["INSERT IGNORE INTO ar_source_cdr_to_move VALUES(", i, ")"]
                       DB.query writeConn (BS.concat query1)
                       processRequests writeConn daysWithServiceCDRS (cachedCDRS, buffSize)

               WriteCmd_insertExpandedExtension !ee
                 -> do extensionCode <- DB.escape writeConn (ee_specificExtensionCode ee)
                       let query1 = "INSERT INTO ar_expanded_extensions SET ar_organization_unit_id=" <> intDec (ee_organizationId ee) <> ", extension_code=\"" <> byteString extensionCode <> "\""
                       DB.query writeConn (fromLazyToStrictByteString $ toLazyByteString query1)
                       processRequests writeConn daysWithServiceCDRS (cachedCDRS, buffSize)

       in bracketOnError
            (do DB.initThread
                openConnection dbParams True)
            (\writeConn -> -- sometime these transactions can not be closed, but I want to report the original error message.
                           Control.Monad.Catch.handleAll (\ _ -> return ()) $ do
                             DB.rollback writeConn
                             DB.close writeConn)

            (\writeConn -> do processRequests writeConn Set.empty ([], 0)
                              DB.commit writeConn
                              DB.close writeConn)

   cdr_convertToError :: CDR -> Maybe AsterisellErrorDupKey -> CDR
   cdr_convertToError cdr0 dupKey
     = let cdr1 = case dupKey of
                    Nothing -> cdr0
                    Just k -> cdr0 { cdr_problemDuplicationKey = Just k }
       in case cdr_direction cdr1 of
         CDR_error
           -> cdr1
         _ -> cdr1 {
                cdr_errorDirection = cdr_direction cdr1
              , cdr_direction = CDR_error
              }

   -- | Rate a CDR or return an error.
   rateCDR :: DB.Connection -> LocalTime -> RatingEnv -> RatingStatus -> Int -> Int -> CDR -> RatingMonad (CDR, RatingStatus)
   rateCDR selectConn callDateToVerify env !state1 !providerId !formatId !cdr
     = do (!cdr1, isExtensionalMatch) <- cdr_initialClassification params selectConn cdr
          when (cdr_direction cdr1 == CDR_error)
               (throwError (cdr1, createError
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

                in throwError (cdr1, err))

          let cdrTime = cdr_calldate cdr1

          (!state3, !cdr2)
            <- case cdr_direction cdr1 == CDR_ignored || (isNothing $ cdr_organizationUnitId cdr1) of
                 True ->  return $!! (state1, cdr1)
                 False -> let
                              -- manage ExpandedExtensions
                              state2
                                = case isExtensionalMatch of
                                    True -> state1
                                    False -> let ee = ExpandedExtension {
                                                        ee_organizationId = fromJust1 "p557" $ cdr_organizationUnitId cdr1
                                                      , ee_specificExtensionCode = fromTextToMySQLResult $ cdr_internalTelephoneNumber cdr1
                                                      }
                                             in state1 { rs_expandedExtensions = S.insert ee (rs_expandedExtensions state1) }

                          in do r <- mainRate_apply env state2 cdr1
                                return $!! r

          return $!! (cdr2, state3)

   -- | Update the rating status, and return the corresponding bundle rates service CDRS.
   --   NOTE: these are two distinct operations, done in one pass because during status update, in case also
   --   service CDRS are returned. But in any case also if there are no serviceCDRS, the status must be updated in any case.
   --   @require rs_criticalError is not set
   updateRatingStatusAndReturnServiceCDRS
     :: RatingEnv
     -> BoundedChan WriteCmd
     -> RatingStatus
     -> CallDate
     -> CallDate
     -> RateRole
     -> LocalTime
     -> CDR
        -- ^ an initializated (but not rated) CDR, used for preparing the rating info
     -> RatingMonad (RatingStatus, [ServiceCDR])

   updateRatingStatusAndReturnServiceCDRS env ratedCDRSChan cachedState1 bundleStateFromCallDate bundleStateCanBeSavedFromCallDate rateRole cdrTime cdr
     = do
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
                                      ( cdr
                                      , createError
                                                   Type_Critical
                                                   Domain_RATES
                                                   ("unknown referenced rate - " ++ rateRefName)
                                                   ("Unknown rate with name \"" ++ rateRefName ++ "\", of type " ++ show rateRole ++ ", at date " ++ showLocalTime cdrTime)
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
                                                ( cdr
                                                , createError
                                                             Type_Critical
                                                             Domain_RATES
                                                             ("error in redefinition of rates - " ++ show rateRole ++ " at date " ++ showLocalTime cdrTime)
                                                             ("The " ++ show rateRole ++ " rate, with name \"" ++ rateRefName ++ "\", defined for a CDR at date " ++ showLocalTime cdrTime ++ ", can not replace correctly pending bundle rates, defined previously. " ++ err)
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
                                  -> case onlyImportedServiceCDRS of
                                       True
                                         -> bundleState_empty
                                       False
                                         -> bundleState_initAccordingRatingEnv env ratePlan (Left bundleStateFromCallDate) bundleState_empty
                                            -- initialize for the first time the bundle state according the rate plan.
                                            -- NOTE: I'm using bundleStateFromCallDate because it will be updated later, according the CDRs before the strict rate from date

                      let (maybePreviousBundleStateCallDate, bundleState2 :: BundleState, serviceCDRS1 :: [ServiceCDR])
                             = case onlyImportedServiceCDRS of
                                 True  -> (Nothing, bundleState1, [])
                                 False -> bundleState_serviceCdrs env ratePlan bundleState1 cdrTime

                      let serviceCDRS2
                            = case onlyImportedServiceCDRS of
                                 True -> []
                                 False -> List.filter (\cdr -> (fromJust1 "753744" $ cdr_toCalldate cdr) > fromCallDate) serviceCDRS1

                      let cachedState2
                            = cachedState1 { rs_incomeRate = Just (ratePlan, validUntil)
                                           , rs_incomeBundleState = Just bundleState2 }

                      case maybePreviousBundleStateCallDate of
                               Nothing
                                 -> return ()
                               Just previousBundleStateCallDate
                                 -> case previousBundleStateCallDate >= bundleStateCanBeSavedFromCallDate of
                                      True
                                        -> lift $ writeChan ratedCDRSChan $!! WriteCmd_saveBundleState previousBundleStateCallDate bundleState2
                                           -- in this case the bundle state time frame were closed, and so we are saving it on disk,
                                           -- because the changes of time-frames can be important, and they are not soo much.
                                      False
                                        -> return ()

                      return (cachedState2, serviceCDRS2)

              CostRate
                -> return $ (cachedState1 { rs_costRate = Just (ratePlan, validUntil) }, [])

   -- | Use the main rate plan for rating a CDR: cost and income.
   mainRate_apply :: RatingEnv -> RatingStatus -> CDR -> RatingMonad (RatingStatus, CDR)
   mainRate_apply !env !state1 !cdr1
     = case isVoipReseller of
         True -> do (!state2, !cdr2) <- searchRateAndApply state1 CostRate cdr1
                    state2 `deepseq` (searchRateAndApply state2 IncomeRate cdr2)
         False -> do (!state2, !cdr2) <- searchRateAndApply state1 IncomeRate cdr1
                     -- in case of call reporting the cost and income are the same
                     let !cdr3 = cdr2 { cdr_cost = cdr_income cdr2
                                      , cdr_debug_cost_rate = cdr_debug_income_rate cdr2
                                      }
                     return (state2, cdr3)

    where

     cdrTime = cdr_calldate cdr1

     -- | Rate the CDR calculating cost or income.
     --   @require RatingStatus is correctly initializated.
     --   @require RatingStatus has no critical error.
     searchRateAndApply :: RatingStatus -> RateRole -> CDR -> RatingMonad (RatingStatus, CDR)
     searchRateAndApply !cachedState1 !rateRole !cdr
       = do let (ratePlan :: CheckedMainRatePlan, bundleState1 :: BundleState)
                  = case rateRole of
                      IncomeRate
                        -> (fst $ fromJust1 "c1" $ rs_incomeRate cachedState1, fromJust1 "c2" $ rs_incomeBundleState cachedState1)
                      CostRate
                        -> (fst $ fromJust1 ("c3: on CDR " ++ cdr_showDebug cdr) $ rs_costRate cachedState1, bundleState_empty)

            (!value, !bundleRateUnitId, !bundleState2, !debugInfo)
               <- case mainRatePlan_calcCost isDebugMode rateRole env bundleState1 ratePlan cdr of
                    Right !r
                      -> return r
                    Left !err
                      -> throwError (cdr, err)

            let debugRatingDetails1
                  = case info_ratingDetails debugInfo of
                      Nothing -> Nothing
                      Just s -> let rateRoleName
                                      = case rateRole of
                                          IncomeRate -> "\nDetails during calculation of Income:\n"
                                          CostRate -> "\nDetails during calculation of Cost:\n"
                                in Just $ Text.pack $ rateRoleName ++ s

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
                                 in throwError (cdr, err)
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



                   in return $!!  ( cachedState1 { rs_incomeBundleState = Just bundleState2 }
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
                -> return $!! ( cachedState1
                              , cdr { cdr_cost = value
                                    , cdr_debug_cost_rate = Just appliedRateName
                                    , cdr_debug_rating_details = debugRatingDetails2
                                    }
                              )

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

   -- | Complete pending fields, and send to the DB the service CDRS.
   writeServiceCDRS :: BoundedChan RateCmd -> [ServiceCDR] -> IO ()
   writeServiceCDRS ratedCDRSChan serviceCDRS
     = mapM_ (\cdr -> writeChan ratedCDRSChan (RateCmd_rateService cdr)) serviceCDRS

rateEngine_updateCachedGroupedCDRS :: DB.Connection -> Maybe (CallDate, CallDate) -> IO ()
rateEngine_updateCachedGroupedCDRS writeConn maybeD1D2
  = let nextDay d = d { localDay = addDays 1 (localDay d) }
        prevDay d = d { localDay = addDays (-1) (localDay d) }

        (d1S, d2S) = case maybeD1D2 of
                       Just (d1, d2) -> (fromStringToByteString $ fromLocalTimeToMySQLDateWith00Time (prevDay d1), fromStringToByteString $ fromLocalTimeToMySQLDateWith00Time (nextDay d2))
                       _ -> error "(err 53207753) unexpected error in the code."

    in do DB.query writeConn $ BS.concat $ ["DELETE FROM ar_cached_grouped_cdr "] ++ (if (isJust maybeD1D2) then [" WHERE calldate >= DATE('", d1S, "') AND calldate < DATE('", d2S, "')"] else [])

          DB.query writeConn $ BS.concat ["INSERT INTO ar_cached_grouped_cdr(`calldate`,`cached_parent_id_hierarchy`,`destination_type`,`count_of_calls`,`billsec`,`income`,`cost_saving`,`cost`) SELECT MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate)), cached_parent_id_hierarchy, destination_type, SUM(count_of_calls), SUM(billsec), SUM(income), SUM(cost_saving), SUM(cost) FROM ar_cdr WHERE ", if (isJust maybeD1D2) then BS.concat [" ar_cdr.calldate >= '", d1S, "' AND ar_cdr.calldate < '", d2S, "' AND "] else "", " destination_type <> ", fromStringToByteString $ show $ cdrDirection_asterisellCode CDR_error, " AND destination_type <> ", fromStringToByteString $ show $ cdrDirection_asterisellCode CDR_ignored, " AND destination_type <> ", fromStringToByteString $ show $ cdrDirection_asterisellCode CDR_none, " GROUP BY YEAR(calldate), DAYOFYEAR(calldate), destination_type, cached_parent_id_hierarchy "]

          return ()

rateEngine_openDBAndUpdateCachedGroupedCDRS :: DBConf -> IO ()
rateEngine_openDBAndUpdateCachedGroupedCDRS dbConf
  = do let dbParams = DB.defaultConnectInfo {
                       connectUser = dbConf_user dbConf
                     , connectPassword = dbConf_password dbConf
                     , connectDatabase = dbConf_dbName dbConf
                     , connectOptions = [Reconnect False, CharsetName "utf8"]
                     }

       writeConn <- openConnection dbParams True
       rateEngine_updateCachedGroupedCDRS writeConn Nothing
       DB.commit writeConn
       DB.close writeConn
       return ()

-- | Apply initial classification to the CDR, completing the fields that are not completed from
--   the initial custom CDR importer, or complete fields of ServiceCDR.
--
--   Exclude from this initialization the ported telephone number, and the telephone prefix,
--   because they are done in a next phase requiring access to the main Asterisell database.
--
--   The CDR error fields are setted from the caller, in case of errors.
cdr_initialClassification :: EnvParams -> DB.Connection -> CDR -> RatingMonad (CDR, IsExtensionalMatch)
cdr_initialClassification params selectConn !cdr1
  =
      case (cdr_direction cdr1 == CDR_error || cdr_direction cdr1 == CDR_ignored) of
        True
          -> return (cdr1, True)
             -- nothing to do, because because the problem was already signaled
             -- from the module generating this CDR.
        False
          -> case runStateT pass1 cdr1 of
               Left err
                 -> throwError (cdr1, err)
               Right (isExtensionalMatch, cdr2)
                 -> do cdr3 <- liftIO $ passTestPortedTelephoneNumbers cdr2
                       case execStateT pass2 cdr3 of
                         Left err
                           -> throwError (cdr3, err)
                         Right cdr4
                           -> return (cdr4, isExtensionalMatch)

 where

  pass1
    = do  let channelDomains = params_channelDomains params

          let organizationsInfo = params_organizations params

          let cdrTime = cdr_calldate cdr1

          modify (\cdr -> cdr { cdr_precision = (params_currencyPrecision params) })

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
                             when (List.length accountCode == 0)
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
                                      -> throwError err
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

  passTestPortedTelephoneNumbers cdr1
    = case cdr_externalTelephoneNumberWithAppliedPortability cdr1 of
        Just _
          -> return cdr1
             -- noting to do, the portability was already applied
        Nothing
          -> do
                str1 <- DB.escape selectConn $ fromTextToMySQLResult $ cdr_externalTelephoneNumber cdr1

                let queryB = mconcat [ stringUtf8 "SELECT ported_telephone_number FROM ar_number_portability WHERE telephone_number = '"
                                     , byteString str1
                                     , stringUtf8 "' AND from_date <= "
                                     , exportLocalTime $ Just $ cdr_calldate cdr1
                                     , stringUtf8 " ORDER BY from_date DESC LIMIT 1"
                                     ]

                let query = LBS.toStrict $ toLazyByteString queryB

                DB.query selectConn query
                r <- DB.storeResult selectConn
                maybeRow <- DB.fetchRow r

                ported
                  <- case (List.null maybeRow) of
                       False -> do let [portedTelephoneNumeber] = maybeRow
                                   return $ Just $ fromMySQLResultToText portedTelephoneNumeber
                       True -> return $ Just $ cdr_externalTelephoneNumber cdr1

                DB.freeResult r

                return $ cdr1 { cdr_externalTelephoneNumberWithAppliedPortability = ported }

  pass2
    = do      cdr1 <- get
              let telephonePrefixes = params_telephonePrefixes params

              let numberOfDigitsToMask = params_numberOfDigitsToMask params

              -- Manage telephone prefix only in case there is an already specified ported telephone number:
              -- * in this way if there is an error in the logic, it will be displayed with full details
              -- * other matches will be done in the rating phase, after the ported number is matched with this CDR.

              when ((isNothing $ cdr_telephonePrefixId cdr1) && (isJust $ cdr_externalTelephoneNumberWithAppliedPortability cdr1))
                   (do -- Associate telephone operator prefix, using the ported telephone number,
                       -- because it identify the real type of telephone number.
                       let ported1 = fromJust1 "d1" $ cdr_externalTelephoneNumberWithAppliedPortability cdr1
                       let ported = Text.unpack ported1
                       case trie_getMatch trie_getMatch_initial telephonePrefixes ported of
                         Just (_, value)
                           -> do modify (\c -> c { cdr_telephonePrefixId = Just $ telephonePrefix_id value })
                                 return ()
                         Nothing
                           -> let key = List.take (min 4 (List.length ported)) ported
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
                                          maybeDefaultPrefix = params_defaultTelephonePrefixToNotDisplay params

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
                                                  -> case (List.length external2) <= numberOfDigitsToMask of
                                                       True
                                                         -> external2
                                                            -- do not mask very short telephone numbers
                                                       False
                                                         -> let toRemove = min (List.length external2) numberOfDigitsToMask
                                                                toTake = (List.length external2) - toRemove
                                                            in  List.take toTake external2 ++ List.replicate numberOfDigitsToMask 'X'

                                      in fromString external3

                       modify (\c -> c {  cdr_displayedMaskedExternalTelephoneNumber = Just result })
                       return ()
                   )

              -- return nothing, but the state contain the transformed cdr2
              return ()

  createRateError cdr key descr effect solution
    = createError
        Type_Error
        Domain_RATES
        key
        (descr ++ "\n\n" ++ cdr_showDebug cdr ++ "\n\n")
        effect
        solution

-- | Add to the RatingEnv the ExtensionsToExport.
ratingEnv_loadExtensionsToExport :: RatingEnv -> DB.Connection -> IO RatingEnv
ratingEnv_loadExtensionsToExport env selectConn
  = let streamQuery = "SELECT extension FROM ar_voip_extension_to_move"

        readLoop :: DB.Result -> ExtensionsToExport -> IO ExtensionsToExport
        readLoop streamConn r
             = do !maybeR <- DB.fetchRow streamConn
                  case maybeR of
                    [] -> return r
                    [Just extensionCode]
                      -> do let r2 = trie_addExtensionCode r (fromByteStringToString extensionCode) ()
                            readLoop streamConn r2
                    _ -> throwIO $ ErrorCall "Unexpected query row format in code (ERR 125537)."

    in do DB.query selectConn streamQuery
          dbResult <- DB.useResult selectConn
          r <- readLoop dbResult trie_empty
          DB.freeResult dbResult
          return $ env { env_extensionsToExport = r }

--
-- Test Import Data File
--

-- | Test the initial importing and rating of a format of source-cdr.
rateEngine_testImportDataFile
  :: ConfiguredCDRSImporters
  -> FilePath
  -> CDRProviderName
  -> BS.ByteString
  -> BS.ByteString
  -> CurrencyPrecisionDigits
  -> IO SourceDataSummaryInfo

rateEngine_testImportDataFile
  defaultSourceDataImporter
  inputFile
  providerName
  logicalTypeName
  formatTypeName
  precision = do

   (params, sourceImporter, importer)
     <- case configuredCDRSImporters_get defaultSourceDataImporter logicalTypeName formatTypeName of
          Nothing
            -> throwIO $ ErrorCall $ "There is no known/configured source data file importer."
          Just r
            -> return r

   Control.Monad.Catch.handleAll handleImportErrors $ (do
          summaryInfo@(maybeTimeFrame, maybeTimeFrame2, totLines, linesWithErrors)
           <- withFile inputFile ReadMode $! \sourceH -> do
                nowLocalTime1 <- liftIO getZonedTime
                let nowLocalTime = zonedTimeToLocalTime nowLocalTime1
                cdrsChan <- newBoundedChan 50
                (_, r) <- concurrently (sourceImporter sourceH cdrsChan) (processSourceCDR importer cdrsChan (nowLocalTime, (Nothing, Nothing, 0, 0)))
                return r

          putStrLn $ "\n\nParsed " ++ show totLines ++ " lines. Lines with errors: " ++ show linesWithErrors ++ "\nCDRs with errors are prefixed by " ++ errorPrefix ++ "CDRs ignored with " ++ ignorePrefix ++ "CDRs imported with " ++ okPrefix

          return summaryInfo)

 where

   errorPrefix = "!!!ERROR!!!\n"

   ignorePrefix = "???IGNORED???\n"

   okPrefix = "...IMPORTED...\n"

   handleImportErrors :: SomeException -> IO SourceDataSummaryInfo
   handleImportErrors e
     = throwIO $ ErrorCall $ "There is an error during the importing of file \"" ++ inputFile ++ "\", with provider " ++ (Text.unpack providerName) ++ ", format " ++  (Text.unpack $ fromMySQLResultToText $ Just $ logicalTypeName) ++ "__" ++  (Text.unpack $ fromMySQLResultToText $ Just formatTypeName) ++ ". " ++ (show e) ++ ".\nAll the source data files with this format, and/or similar problems, will be not imported and rated. So there can be an high number of CDRs that are not rated. Note that there is only one error message for a file of this type, but this error message implies that all files of the same type are not rated.\nThis is probably an error in the application configuration. Contact the assistance."

   newMaxMinCallDate :: LocalTime -> Bool -> Maybe (LocalTime, LocalTime) -> Maybe (LocalTime, LocalTime)
   newMaxMinCallDate d False v = v
   newMaxMinCallDate d True Nothing = Just (d, d)
   newMaxMinCallDate d True (Just (minDD, maxDD)) = Just (min minDD d, max maxDD d)

   processSourceCDR :: CDRImporter -> BoundedChan (Maybe LBS.ByteString) -> (LocalTime, SourceDataSummaryInfo) -> IO (SourceDataSummaryInfo)
   processSourceCDR importer sourceChan (!lastCallDate, (!maybeMaxMinCallDate, !maybeImportedServiceeMaxMinCallDate, !totLines, !linesWithErrors))
     = do !maybeCdrContent <- readChan sourceChan
          case maybeCdrContent of
            Nothing
              -> -- end of pipe
                 return (maybeMaxMinCallDate, maybeImportedServiceeMaxMinCallDate, totLines, linesWithErrors)

            Just !cdrContent
              -> do let imported@(_, errorOrCDRS) = importer cdrContent providerName
                    case errorOrCDRS of
                      Left err
                        -> putStrLn $ errorPrefix ++ (fromLazyByteStringToString cdrContent) ++ "\n" ++ asterisellError_userShow err
                      Right []
                        -> putStrLn $ ignorePrefix ++ (fromLazyByteStringToString cdrContent)
                      Right cdrsWithDescription
                        -> mapM_ (\(c, s) -> putStrLn $ okPrefix ++ (fromLazyByteStringToString cdrContent) ++ "\n" ++ s ++ "\n" ++ cdr_showDebug c) cdrsWithDescription

                    let (!callDate, !newMaybeMaxMinCallDate, !newMaybeImportedServiceeMaxMinCallDate)
                          = case imported of
                              (Just !d, _)
                                -> force ( d
                                         , newMaxMinCallDate d True maybeMaxMinCallDate
                                         , newMaxMinCallDate d False maybeImportedServiceeMaxMinCallDate)

                              (Nothing, Right [])
                                -> force (lastCallDate, maybeMaxMinCallDate, maybeImportedServiceeMaxMinCallDate)
                                   -- skip CDRS with a recognize NULL parsing

                              (Nothing, _)
                                -> force (lastCallDate, maybeMaxMinCallDate, maybeImportedServiceeMaxMinCallDate)

                    let !nextLinesWithErrors
                          = case imported of
                              (_, Left err)
                                -> linesWithErrors + 1
                              (_, Right _)
                                -> linesWithErrors

                    let !isImportedServiceCDR = False

                    -- process the rest of the pipe using a recursive call, and passing the new processing "state"
                    processSourceCDR importer sourceChan $!! (callDate, (newMaybeMaxMinCallDate, newMaybeImportedServiceeMaxMinCallDate, totLines + 1, nextLinesWithErrors))

--
-- Utils
--

fromByteStringToString :: BS.ByteString -> String
fromByteStringToString s = Text.unpack $ Text.decodeUtf8 s

fromLazyByteStringToString :: LBS.ByteString -> String
fromLazyByteStringToString s = LText.unpack $ LText.decodeUtf8 s

openConnection :: DB.ConnectInfo -> Bool -> IO DB.Connection
openConnection dbParams writeTransaction
     = do db <- DB.connect dbParams
          DB.autocommit db False
          DB.query db "SET NAMES 'utf8' COLLATE 'utf8_bin'"
          case writeTransaction of
            True -> (do DB.query db "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
                        DB.query db "START TRANSACTION")
            False -> (do DB.query db "SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED"
                         -- set this level because in TokuDB it is the only level for SELECT, not involving LOCKS and SNAPSHOOTS
                         DB.query db "START TRANSACTION READ ONLY")
          return db

type DebugChan = BoundedChan (Either () String)

debugFile_append :: Maybe FilePath -> DebugChan -> String -> IO ()
debugFile_append !maybeDebugFile !chan1 ~msg
     = case maybeDebugFile of
         !Nothing
           -> return ()
         Just !f
           -> do writeChan chan1 (Right msg)
                 return ()

debugFileProcess :: Maybe FilePath -> DebugChan -> IO ()
debugFileProcess maybeDebugFile chan1
     = do msg <- readChan chan1
          case msg of
            Left ()
              -> return ()
            Right msg1
              -> do case maybeDebugFile of
                      Just f
                        -> appendFile f ("\n" ++ msg1 ++ "\n")
                      Nothing -> return ()

                    debugFileProcess maybeDebugFile chan1

-- | Export a LocalTime to a MySQL date time stamp.
exportLocalTime :: Maybe LocalTime -> Builder
exportLocalTime Nothing = stringUtf8  "NULL"
exportLocalTime (Just t)
  = mconcat [ charUtf8 '\''
            , intDec0000 $ fromInteger year
            , charUtf8 '-'
            , intDec00 month
            , charUtf8 '-'
            , intDec00 day
            , charUtf8 ' '
            , intDec00 (todHour tod)
            , charUtf8 ':'
            , intDec00 (todMin tod)
            , charUtf8 ':'
            , intDec00 (truncate $ todSec tod)
            , charUtf8 '\''
            ]

 where

   tod = localTimeOfDay t

   (year, month, day) = toGregorian $ localDay t

   intDec0000 i
     = case i < 10 of
         True -> mappend (stringUtf8 "000") (intDec i)
         False -> case i < 100 of
                True -> mappend (stringUtf8 "00") (intDec i)
                False -> case i < 1000 of
                           True -> mappend (charUtf8 '0') (intDec i)
                           False -> intDec i

   intDec00 i
     = case i < 10 of
         True -> mappend (charUtf8 '0') (intDec i)
         False -> intDec i

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

--
-- Unit Tests
--

-- TODO test all type of params
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
