{-# LANGUAGE OverloadedStrings, CPP, TemplateHaskell, ScopedTypeVariables, BangPatterns #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

module Main (
    main
) where


import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.MainRatePlan
import Asterisell.ImportDataFiles
import Asterisell.RateEngine
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.CustomerSpecificImporters
import Asterisell.CustomOrganizationInfoImporters
import Asterisell.CustomerSpecificRates
import Asterisell.DB
import Asterisell.CustomPortedTelephoneNumbers

import System.Environment
import System.Console.GetOpt
import System.Exit
import System.Directory (removeFile)
import Data.List as List
import Control.Monad
import Data.Maybe
import Data.Time.LocalTime
import System.IO (stderr, IOMode( ReadMode ))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Test.HUnit as HUnit
import Debug.Trace
import qualified Database.MySQL.Base as DB
import Control.Exception.Assert.Sugar

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.ByteString.Lazy as Parsec
import qualified Text.Parsec.Char as Parsec

import qualified Data.Map as Map

import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

-----------------
-- EXIT ERRORS --
-----------------
-- NOTE: mantain this values in synchro with the values on PHP world

applicationError :: ExitCode
applicationError = ExitFailure 1

extensionConfigurationError :: ExitCode
extensionConfigurationError = ExitFailure 2

telephonePrefixesConfigurationError :: ExitCode
telephonePrefixesConfigurationError = ExitFailure 3

rateConfigurationError :: ExitCode
rateConfigurationError = ExitFailure 4

------------------
-- MAIN UTITILY --
------------------

type FileName = String
type DirectoryName = String
type MySQLDate = String

data Flag
  = Exec_Help
  | Exec_Version
  | Exec_Test
  | Exec_Debug
  | Exec_TestOrganizations String
  | Exec_ImportDataFile FileName
  | Exec_DeleteInputFile String
  | Exec_Rate
  | Exec_UpdateAllCachedCDRS
  | Exec_Import FileName
  | Exec_ProviderName String
  | Exec_ProviderId String
  | Exec_FileLogicalType String
  | Exec_FileVersionType String
  | Exec_FileVersionTypeId String
  | Exec_FileLogicalTypeId String
  | Exec_FromDate MySQLDate
  | Exec_ToDate MySQLDate
  | Exec_TestToDate MySQLDate
  | Exec_DigitsToMask String
  | Exec_DefaultTelephonePrefix String
  | Exec_CurrencyPrecision String
  | Exec_DBHost String
  | Exec_DBPort String
  | Exec_ResultFile FileName
  | Exec_DebugMode String
  | Exec_DebugFileName FileName
  | Exec_SignalFile FileName
  | Exec_RateUnbilledCalls String
  | Exec_IsStatusFile String
  | Exec_IsImportedService String
  | Exec_ExportCDRS FileName
  | Exec_UseOnlyCDRSToMove String
  | Exec_TestImportDataFile String
  | Exec_IsVoipReseller String
  | Exec_RunLevel String
  | Exec_Cores String
  | Exec_FastGrouping String
  | Exec_CustomersImport String
  | Exec_DataSourceName String
  | Exec_PortedTelephoneNumbers String
  | Exec_OrganizationToIgnore String
  | Exec_Params String

options :: [OptDescr Flag]
options = [ Option [] ["help"] (NoArg Exec_Help) "help"
          , Option [] ["update-all-cached-cdrs"] (NoArg Exec_UpdateAllCachedCDRS) "update ar_cached_grouped_cdrs ar_cached_errors tables"
          , Option [] ["rate"] (NoArg Exec_Rate) "rate"
          , Option [] ["customers-import"] (ReqArg Exec_CustomersImport "ID") "import customers from external data source"
          , Option [] ["ported-telephone-numbers"] (ReqArg Exec_PortedTelephoneNumbers "FORMAT-NAME") "import ported telephone numbers from file"
          , Option [] ["debug"] (NoArg Exec_Debug) "debug"
          , Option ['V'] ["version"] (NoArg Exec_Version) "show version number"
          , Option [] ["run-level"] (ReqArg Exec_RunLevel "0..4") "0 for complete rating, 1 .. 4 for disabling some parts"
          , Option [] ["use-cores"] (ReqArg Exec_Cores "0|1|2|..") "0 for using all cores, 1 for using only one corec, etc.."
          , Option [] ["use-fast-grouping"] (ReqArg Exec_FastGrouping "0|1") "1 for (experimental) fast grouping of CDRS"
          , Option [] ["test"] (NoArg Exec_Test) "execute some tests on the code"
          , Option [] ["test-organizations"] (ReqArg Exec_TestOrganizations "NR") "execute some tests on the organization structure. Data must be set from the PHP world."
          , Option [] ["result-file"] (ReqArg Exec_ResultFile "FILE") ""
          , Option [] ["import-data-file"] (ReqArg Exec_ImportDataFile "FILENAME") "import a data file with source CDRs"
          , Option [] ["test-import-data-file"] (ReqArg Exec_TestImportDataFile "FILENAME") "test the import procedure for a data file with source CDRs"
          , Option [] ["delete-data-file"] (ReqArg Exec_DeleteInputFile "true|false") ""
          , Option [] ["import"] (ReqArg Exec_Import "FILENAME") "import and rate the file"
          , Option [] ["provider"] (ReqArg Exec_ProviderName "NAME") ""
          , Option [] ["provider-id"] (ReqArg Exec_ProviderId "ID") ""
          , Option [] ["file-version-type"] (ReqArg Exec_FileVersionType "FILE TYPE") "version type of file to import"
          , Option [] ["file-version-type-id"] (ReqArg Exec_FileVersionTypeId "ID") ""
          , Option [] ["file-logical-type"] (ReqArg Exec_FileLogicalType "FILE TYPE") "logical type of file to import"
          , Option [] ["file-logical-type-id"] (ReqArg Exec_FileLogicalTypeId "ID") ""
          , Option [] ["signal"] (ReqArg Exec_SignalFile "FILENAME") ""
          , Option [] ["is-status-file"] (ReqArg Exec_IsStatusFile "true|false") ""
          , Option [] ["from-date"] (ReqArg Exec_FromDate "\"YYYY-MM-DD hh:mm:ss\"") "rate starting from the specified date, inclusive"
          , Option [] ["to-date"] (ReqArg Exec_ToDate "\"YYYY-MM-DD hh:mm:ss\"") "rate to the specified date, exclusive"
          , Option [] ["test-to-date"] (ReqArg Exec_TestToDate "\"YYYY-MM-DD hh:mm:ss\"") "null for disabling, or a date to which limit the ar_source_cdr, for unit testing reasons"
          , Option [] ["digits-to-mask"] (ReqArg Exec_DigitsToMask "NUMBER") "The number of digits to mask displaying telephone numbers"
          , Option [] ["debug-mode"] (ReqArg Exec_DebugMode "NUMBER") "0 for rating in production mode, 1 for enabling run-time tests during rating."
          , Option [] ["debug-file"] (ReqArg Exec_DebugFileName "FILENAME") "The file where writing debug info."
          , Option [] ["default-telephone-prefix"] (ReqArg Exec_DefaultTelephonePrefix "TELEPHONE-PREFIX") "the telephone prefix used in local calls, and that can be omitted during display of the calls"
          , Option [] ["currency-precision"] (ReqArg Exec_CurrencyPrecision "NUMBER") "the number of decimal digits to use for cost and incomes"
          , Option [] ["db-host"] (ReqArg Exec_DBHost "HOST") "the database host"
          , Option [] ["db-port"] (ReqArg Exec_DBPort "PORT") "the database port"
          , Option [] ["rate-unbilled-calls"] (ReqArg Exec_RateUnbilledCalls "true|false") ""
          , Option [] ["export-cdrs"] (ReqArg Exec_ExportCDRS "FILENAME") "export cdrs"
          , Option [] ["use-only-cdrs-to-move"] (ReqArg Exec_UseOnlyCDRSToMove "true|false") "export only cdrs in ar_source_cdr_to_move table"
          , Option [] ["is-voip-reseller"] (ReqArg Exec_IsVoipReseller "0|1") "0 for call reporting, 1 for voip reseller (billing)"
          , Option [] ["is-imported-service"] (ReqArg Exec_IsImportedService "0|1") "1 for services imported from a voip reseller"
          , Option [] ["data-source-name"] (ReqArg Exec_DataSourceName "ID") "the name of datasource"
          , Option [] ["organization-to-ignore"] (ReqArg Exec_OrganizationToIgnore "ID") "the internal name of an organization to ignore, or an empty string"
          , Option [] ["params"] (ReqArg Exec_Params "FILENAME") "a CSV file with \"param-name,value\" rows inside"
          ]

header = "Usage: main [OPTION...]"

runTestList l
  = do c <- HUnit.runTestTT $ HUnit.TestList l
       case (HUnit.errors c) > 0 || (HUnit.failures c) > 0 of
         True -> exitFailure
         False -> return ()

cparams_toDBConf :: CmdParams -> DBConf
cparams_toDBConf pp = cparams_toDBConf2 pp BS.empty

cparams_toDBConf2 :: CmdParams -> BS.ByteString -> DBConf
cparams_toDBConf2 pp p
    = DBConf {
        dbConf_user = cparams_get pp (BS.concat [p, "db-user"])
      , dbConf_password = cparams_get pp (BS.concat [p, "db-password"])
      , dbConf_dbName = cparams_get pp (BS.concat [p, "db-name"])
      }

main = do
  catch
        mainRate
        (\(e :: SomeException)
            -> do putStrLn $ "\n" ++ show e
                  exitFailure)

mainRate = do
  args <- getArgs
  opts <- case getOpt RequireOrder options args of
             (flags, [],      [])
               -> return flags
             (_,     nonOpts, [])
               ->   error $ "unrecognized arguments: " ++ unwords nonOpts
             (_,     _,       msgs)
               -> error $ concat msgs ++ usageInfo header options
  case opts of
    [] -> do putStrLn (usageInfo header options)
             exitFailure
    [Exec_Help]
      -> do putStrLn (usageInfo header options)
            return ()

    [ Exec_UpdateAllCachedCDRS
     ,Exec_Params pFileName]
      -> do
            params <- cparams_load pFileName
            let dbConf = cparams_toDBConf params
            rateEngine_openDBAndUpdateCachedGroupedCDRS dbConf

    [Exec_Test]
      -> do putStrLn "\nTest Parsing"
            runTestList tt_parseTests

            putStrLn "\nTest Error Management"
            runTestList test_ruleCaseFor

            putStrLn "\nTest Monad RuleCaseFor"
            runTestList test_ruleCaseFor

            putStrLn "\nTest tries"
            runTestList tt_trie_test

            putStrLn "\nTest Rate Specifications"
            runTestList tt_rateSpecificationsTests
            runTestList organizationHierarchy_test

            putStrLn "\nTest Rate Plan"
            runTestList tt_ratePlanTests
            runTestList tt_specificRates

            putStrLn "\nTest Rate Calculations"
            runTestList tt_mainRateCalcTets
            runTestList tt_servicesTests
            runTestList tt_customerSpecificImporters

    [Exec_TestOrganizations pass,
     Exec_Params pFileName,
     Exec_FromDate fromDateS]
      -> do
            params <- cparams_load pFileName
            let dbConf = cparams_toDBConf params

            conn <- db_openConnection dbConf False

            let fromDate = if fromDateS == "null"
                           then Nothing
                           else Just $ fromJust1 "ERR 0256" $ fromMySQLDateTimeToLocalTime fromDateS

            extensionsInfo <- extensions_load conn True Nothing

            case fromDate of
              Just d -> runTestList $ testWithDataImportedFromPHP extensionsInfo (fromJust1 "ma6" $ fromTextToInt (Text.pack pass)) d
              Nothing -> return ()

    [Exec_ImportDataFile fileName,
     Exec_Params pFileName,
     Exec_DebugFileName debugFileNameS,
     Exec_ProviderName providerName,
     Exec_ProviderId providerIdS,
     Exec_FileLogicalType logicalType,
     Exec_FileLogicalTypeId logicalTypeIdS,
     Exec_FileVersionType versionType,
     Exec_FileVersionTypeId versionTypeIdS,
     Exec_IsImportedService isImportedServiceS,
     Exec_IsStatusFile isStatusS,
     Exec_FromDate fromDateS,
     Exec_ToDate toDateS
     ]
      -> do let pseudoPrecision = 4
            -- NOTE: in calldate importing the precision is not important

            let logicalTypeId = fromJust1 "imp1" $ fromTextToInt (Text.pack logicalTypeIdS)
            let versionTypeId = fromJust1 "imp2" $ fromTextToInt (Text.pack versionTypeIdS)
            let providerId = fromJust1 "imp3" $ fromTextToInt (Text.pack providerIdS)

            params <- cparams_load pFileName
            let dbConf = cparams_toDBConf params

            isImportedService
              <- case isImportedServiceS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized is-imported-service value \"" ++ isImportedServiceS ++ "\"."
                             exitFailure

            debugFileName
              <- case debugFileNameS == "null" of
                   True -> return Nothing
                   False -> return $ Just debugFileNameS

            isStatus
              <- case isStatusS of
                   "true" -> return True
                   "false" -> return False
                   _ -> do putStrLn $ "Error in params"
                           exitFailure

            let maybeTimeFrame
                  = case fromDateS of
                      "null"
                        -> Nothing
                      _ -> Just $ (fromJust1 "ma6" $ fromMySQLDateTimeToLocalTime fromDateS, fromJust1 "imp3" $ fromMySQLDateTimeToLocalTime toDateS)

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

              (maybeDateRange, totLines, totLinesWithErrors)
                <- rateEngine_importDataFile
                     fileName
                     (Text.pack providerName)
                     providerId
                     (Text.pack logicalType)
                     logicalTypeId
                     (Text.pack versionType)
                     versionTypeId
                     isStatus
                     maybeTimeFrame
                     dbConf
                     pseudoPrecision

              -- Return the info in the format expected from the PHP caller:
              -- > min call-date, max call-date, tot lines, tot lines with errors

              let showDateRange
                         = case maybeDateRange of
                             Nothing -> "null,null"
                             Just (v1, v2) -> showLocalTimeUsingMySQLFormat v1 ++ "," ++ showLocalTimeUsingMySQLFormat v2

              putStrLn $ showDateRange ++ "," ++ show totLines ++ "," ++ show totLinesWithErrors
              return ())

    [Exec_ExportCDRS outFileName,
     Exec_Params pFileName,
     Exec_ProviderName providerName,
     Exec_ProviderId providerIdS,
     Exec_FileLogicalType logicalType,
     Exec_FileLogicalTypeId logicalTypeIdS,
     Exec_FileVersionType versionType,
     Exec_FileVersionTypeId versionTypeIdS,
     Exec_FromDate fromDateS,
     Exec_ToDate toDateS,
     Exec_UseOnlyCDRSToMove useOnlyCdrsToMoveS
     -- NOTE: this setting in not used anymore
     ]
      -> do let pseudoPrecision = 4
            -- NOTE: in calldate importing the precision is not important

            let logicalTypeId = fromJust1 "imp1" $ fromTextToInt (Text.pack logicalTypeIdS)
            let versionTypeId = fromJust1 "imp2" $ fromTextToInt (Text.pack versionTypeIdS)
            let providerId = fromJust1 "imp3" $ fromTextToInt (Text.pack providerIdS)

            useOnlyCdrsToMove
              <- case useOnlyCdrsToMoveS of
                   "true" -> return True
                   "false" -> return False
                   _ -> do putStrLn $ "Error in params"
                           exitFailure

            params <- cparams_load pFileName
            let dbConf = cparams_toDBConf params

            let timeFrame
                  = (fromJust1 "ma6" $ fromMySQLDateTimeToLocalTime fromDateS, fromJust1 "imp3" $ fromMySQLDateTimeToLocalTime toDateS)

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

              rateEngine_exportDataFile
                outFileName
                (Text.pack providerName)
                providerId
                (Text.pack logicalType)
                logicalTypeId
                (Text.pack versionType)
                versionTypeId
                timeFrame
                dbConf

              -- Return the info in the format expected from the PHP caller:
              -- > min call-date, max call-date, tot lines, tot lines with errors
              return ())

    [ Exec_Rate
     ,Exec_Params pFileName
     ,Exec_DebugMode debugModeS
     ,Exec_RunLevel runLevelS
     ,Exec_Cores coresS
     ,Exec_FastGrouping fastGroupingS
     ,Exec_IsVoipReseller isVoipResellerS
     ,Exec_DigitsToMask digitsToMaskS
     ,Exec_OrganizationToIgnore organizationToIgnoreS
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefixS
     ,Exec_CurrencyPrecision precision
     ,Exec_DebugFileName debugFileName
     ,Exec_FromDate fromDateS
     ,Exec_RateUnbilledCalls rateUnbilledCallsS
     ,Exec_TestToDate testToDateS
     ]

      -> do

            fromCallDate <- parseTimeFrameOrExit fromDateS

            testToDate <- parseMaybeTimeFrameOrExit testToDateS 

            params <- cparams_load pFileName
            let dbConf = cparams_toDBConf params

            digitsToMask
              <- case fromTextToInt $ Text.pack digitsToMaskS of
                   Just i
                     -> do return i
                   Nothing
                      -> do putStrLn $ "Unrecognized digitsToMask int \"" ++ digitsToMaskS ++ "\"."
                            exitFailure

            runLevelT
              <- case runLevelS of
                   "0" -> return RunLevelFull
                   "1" -> return RunLevelSkipUpdateCachedGroupedCDRS
                   "2" -> return RunLevelSkipDBWrite
                   "3" -> return RunLevelFakePortedTelephoneNunmbers
                   _   -> do putStrLn $ "Unrecognized run-level value \"" ++ runLevelS ++ "\"."
                             exitFailure

            useCores
              <- case fromTextToInt $ Text.pack coresS of
                   Just i
                     -> do return i
                   Nothing
                      -> do putStrLn $ "Unrecognized use-cores int \"" ++ coresS ++ "\"."
                            exitFailure

            useFastGrouping
              <- case fastGroupingS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized use-faste-grouping value \"" ++ fastGroupingS ++ "\"."
                             exitFailure

            let runLevel = RunLevel { runLevel_type = runLevelT, runLevel_cores = useCores, runLevel_fastGroupedCDRS = useFastGrouping }

            maybeDebugFileName
              <- case runLevelT of
                   RunLevelSkipDBWrite
                       -> case (Text.length $ Text.strip $ Text.pack debugFileName) == 0 of
                          True -> return $ Just "/var/tmp/debug-rating.csv"
                          False -> return $ Just debugFileName
                   _ -> return Nothing 

            isDebugMode
              <- case debugModeS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized debugMode value \"" ++ debugModeS ++ "\"."
                             exitFailure

            isVoipReseller
              <- case isVoipResellerS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized isVoipReseller mode value \"" ++ isVoipResellerS ++ "\"."
                             exitFailure

            rateUnbilledCalls
              <- case rateUnbilledCallsS of
                   "false" -> return False
                   "true" -> return True
                   _   -> do putStrLn $ "Unrecognized param value."
                             exitFailure

            organizationToIgnore
              <- case organizationToIgnoreS of
                   "" -> return Nothing
                   _ -> return $ Just $ Text.pack organizationToIgnoreS

            let maybeDefaultTelephonePrefix = if List.null defaultTelephonePrefixS then Nothing else (Just $ Text.pack defaultTelephonePrefixS)

            let currencyPrecision = fromJust1 "ma1" $ fromTextToInt (Text.pack precision)

            let env 
                  = InitialRatingParams {
                        iparams_isDebugMode = isDebugMode
                      , iparams_isVoipReseller = isVoipReseller
                      , iparams_digitsToMask = digitsToMask
                      , iparams_defaultTelephonePrefixToNotDisplay = maybeDefaultTelephonePrefix
                      , iparams_currencyPrecision = currencyPrecision
                      , iparams_organizationToIgnore = organizationToIgnore
                      , iparams_debugFileName = maybeDebugFileName
                      , iparams_fromDate = fromCallDate
                      , iparams_testToDate = testToDate
                      , iparams_isRateUnbilledCallsEvent = rateUnbilledCalls
                      , iparams_dbName = fromByteStringToString $ dbConf_dbName dbConf
                      , iparams_dbUser = fromByteStringToString $  dbConf_user dbConf
                      , iparams_dbPasswd = fromByteStringToString $ dbConf_password dbConf
                      , iparams_configuredRatePlanParsers = configuredRateParsers
                      }

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

                stats <- rateEngine_rate runLevel env
                putStr $ show stats
                return ())

    [ Exec_CustomersImport methodNameS
     ,Exec_Params pFileName
     ,Exec_DataSourceName dataSourceNameS
     ,Exec_OrganizationToIgnore organizationToIgnoreS
     ,Exec_CurrencyPrecision precision
     ,Exec_DBHost remoteDBHostS
     ,Exec_DBPort remoteDBPortS
     ]

      -> do
            params <- cparams_load pFileName
            let localConnectInfo = cparams_toDBConf params 
            let remoteConf = cparams_toDBConf2 params "remote-"

            let currencyPrecision = fromJust1 "ma1" $ fromTextToInt (Text.pack precision)

            let dataSourceName = Text.pack dataSourceNameS

            let organizationToIgnore = Text.pack organizationToIgnoreS

            -- NOTE: use a connection that is compatible with old version of MySQL.
            -- In case the custom method can override this.
            let remoteConnectInfo
                  = DB.defaultConnectInfo {
                      DB.ciHost = remoteDBHostS
                    , DB.ciPort = (read remoteDBPortS)
                    , DB.ciDatabase = dbConf_dbName remoteConf
                    , DB.ciUser =  dbConf_user remoteConf
                    , DB.ciPassword = dbConf_password remoteConf
                    }

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure)
              (case methodNameS of
                 "rolf1_dynamic"
                   -> rolf1_synchro localConnectInfo remoteConnectInfo organizationToIgnore currencyPrecision dataSourceName params
                 "rolf1"
                   -> rolf1_synchro localConnectInfo remoteConnectInfo Text.empty currencyPrecision dataSourceName params
                 _ -> do putStrLn $ "Unsupported method \"" ++ methodNameS ++ "\""
                         exitFailure)

    [  Exec_PortedTelephoneNumbers formatNameS
     , Exec_Params pFileName
     , Exec_ImportDataFile fileName
     , Exec_DeleteInputFile deleteInputFileS
     ]

      -> do
            params <- cparams_load pFileName
            let localConnectInfo = cparams_toDBConf params

            deleteInputFile
              <- case deleteInputFileS of
                   "true" -> return True
                   "false" -> return False
                   _ -> do putStrLn $ "Error in params"
                           exitFailure

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure)
              (case formatNameS of
                 "telcordia-zaf"
                   -> do _ <- rolf1_processTelcordia localConnectInfo fileName
                         return ()
                 _ -> do putStrLn $ "Unsupported format \"" ++ formatNameS ++ "\""
                         exitFailure
              )

            when (deleteInputFile) (removeFile fileName)

    [Exec_Debug]
      -> do -- Test some code, during development of the rate engine
            return ()

    _ -> do putStrLn "\nunrecognized options"
            exitFailure


parseTimeFrameOrExit :: String -> IO LocalTime
parseTimeFrameOrExit s
  = case fromMySQLDateTimeToLocalTime s  of
      Just r
        -> return r
      Nothing
        -> do putStrLn $ "Unrecognized date format " ++ s
              exitFailure

parseMaybeTimeFrameOrExit :: String -> IO (Maybe LocalTime)
parseMaybeTimeFrameOrExit fromDateS 
  = case fromDateS == "null" of
      True
        -> return Nothing
      False
        -> Just <$> parseTimeFrameOrExit fromDateS
