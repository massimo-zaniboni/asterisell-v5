{-# LANGUAGE OverloadedStrings, CPP, TemplateHaskell, ScopedTypeVariables, BangPatterns #-}

{- $LICENSE 2013, 2014, 2015, 2016, 2017, 2018
 * Copyright (C) 2013-2018 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

module Main (
    main
) where


import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.MainRatePlan
import Asterisell.RateEngine
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.CustomerSpecificImporters
import Asterisell.CustomerSpecificRates
import Asterisell.DB

import System.Environment
import System.Console.GetOpt
import System.Exit
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

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.ByteString.Lazy as Parsec
import qualified Text.Parsec.Char as Parsec

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
  | Exec_Import FileName
  | Exec_ProviderName String
  | Exec_ProviderId String
  | Exec_FileLogicalType String
  | Exec_FileVersionType String
  | Exec_FileVersionTypeId String
  | Exec_FileLogicalTypeId String
  | Exec_FromDate MySQLDate
  | Exec_ToDate MySQLDate
  | Exec_DigitsToMask String
  | Exec_DefaultTelephonePrefix String
  | Exec_CurrencyPrecision String
  | Exec_DBUser String
  | Exec_DBPassword String
  | Exec_DBName String
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

options :: [OptDescr Flag]
options = [ Option [] ["help"] (NoArg Exec_Help) "help"
          , Option [] ["rate"] (NoArg Exec_Rate) "rate"
          , Option [] ["debug"] (NoArg Exec_Debug) "debug"
          , Option ['V'] ["version"] (NoArg Exec_Version) "show version number"
          , Option [] ["run-level"] (ReqArg Exec_RunLevel "0..4") "0 for complete rating, 1 .. 4 for disabling some parts"
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
          , Option [] ["digits-to-mask"] (ReqArg Exec_DigitsToMask "NUMBER") "The number of digits to mask displaying telephone numbers"
          , Option [] ["debug-mode"] (ReqArg Exec_DebugMode "NUMBER") "0 for rating in production mode, 1 for enabling run-time tests during rating."
          , Option [] ["debug-file"] (ReqArg Exec_DebugFileName "FILENAME") "The file where writing debug info."
          , Option [] ["default-telephone-prefix"] (ReqArg Exec_DefaultTelephonePrefix "TELEPHONE-PREFIX") "the telephone prefix used in local calls, and that can be omitted during display of the calls"
          , Option [] ["currency-precision"] (ReqArg Exec_CurrencyPrecision "NUMBER") "the number of decimal digits to use for cost and incomes"
          , Option [] ["db-user"] (ReqArg Exec_DBUser "user-name") "the database username"
          , Option [] ["db-password"] (ReqArg Exec_DBPassword "password") "the database password"
          , Option [] ["db-name"] (ReqArg Exec_DBName "database-name") "the database name"
          , Option [] ["rate-unbilled-calls"] (ReqArg Exec_RateUnbilledCalls "true|false") ""
          , Option [] ["export-cdrs"] (ReqArg Exec_ExportCDRS "FILENAME") "export cdrs"
          , Option [] ["use-only-cdrs-to-move"] (ReqArg Exec_UseOnlyCDRSToMove "true|false") "export only cdrs in ar_source_cdr_to_move table"
          , Option [] ["is-voip-reseller"] (ReqArg Exec_IsVoipReseller "0|1") "0 for call reporting, 1 for voip reseller (billing)"
          , Option [] ["is-imported-service"] (ReqArg Exec_IsImportedService "0|1") "1 for services imported from a voip reseller"
          ]

header = "Usage: main [OPTION...]"

runTestList l
  = do c <- HUnit.runTestTT $ HUnit.TestList l
       case (HUnit.errors c) > 0 || (HUnit.failures c) > 0 of
         True -> exitFailure
         False -> return ()

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

            putStrLn "\nTest Rate Plan"
            runTestList tt_ratePlanTests
            runTestList tt_specificRates

            putStrLn "\nTest Rate Calculations"
            runTestList tt_mainRateCalcTets
            runTestList tt_servicesTests
            runTestList tt_customerSpecificImporters

    [Exec_TestOrganizations pass,
     Exec_FromDate fromDateS,
     Exec_DBName dbName,
     Exec_DBUser dbUser,
     Exec_DBPassword dbPassword]
      -> do
            let dbConf = DBConf {
                           dbConf_user = fromStringToByteString dbUser
                         , dbConf_password = fromStringToByteString dbPassword
                         , dbConf_dbName = fromStringToByteString dbName
                         }


            conn <- openDBConnection dbConf False
            let fromDate = fromJust1 "ma100" $ fromMySQLDateTimeToLocalTime fromDateS
            extensionsInfo <- extensions_load conn True 
            runTestList $ testWithDataImportedFromPHP extensionsInfo (fromJust1 "ma6" $ fromTextToInt (Text.pack pass)) fromDate

    [Exec_ImportDataFile fileName,
     Exec_DebugMode debugModeS,
     Exec_DebugFileName debugFileNameS,
     Exec_DeleteInputFile deleteInputFileS,
     Exec_ProviderName providerName,
     Exec_ProviderId providerIdS,
     Exec_FileLogicalType logicalType,
     Exec_FileLogicalTypeId logicalTypeIdS,
     Exec_FileVersionType versionType,
     Exec_FileVersionTypeId versionTypeIdS,
     Exec_IsImportedService isImportedServiceS,
     Exec_IsStatusFile isStatusS,
     Exec_FromDate fromDateS,
     Exec_ToDate toDateS,
     Exec_DBName dbName,
     Exec_DBUser dbUser,
     Exec_DBPassword dbPassword
     ]
      -> do let pseudoPrecision = 4
            -- NOTE: in calldate importing the precision is not important

            let logicalTypeId = fromJust1 "imp1" $ fromTextToInt (Text.pack logicalTypeIdS)
            let versionTypeId = fromJust1 "imp2" $ fromTextToInt (Text.pack versionTypeIdS)
            let providerId = fromJust1 "imp3" $ fromTextToInt (Text.pack providerIdS)

            let dbConf = DBConf {
                           dbConf_user = fromStringToByteString dbUser
                         , dbConf_password = fromStringToByteString dbPassword
                         , dbConf_dbName = fromStringToByteString dbName
                         }

            isDebugMode
              <- case debugModeS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized debug mode value \"" ++ debugModeS ++ "\"."
                             exitFailure

            isImportedService
              <- case isImportedServiceS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized is-imported-service value \"" ++ isImportedServiceS ++ "\"."
                             exitFailure

            debugFileName
              <- case isDebugMode of
                   True -> return $ Just debugFileNameS
                   False -> return Nothing

            isStatus
              <- case isStatusS of
                   "true" -> return True
                   "false" -> return False
                   _ -> do putStrLn $ "Error in params"
                           exitFailure

            deleteInputFile
              <- case deleteInputFileS of
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
                     debugFileName
                     fileName
                     deleteInputFile
                     isImportedService
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
                     True

              -- Return the info in the format expected from the PHP caller:
              -- > min call-date, max call-date, tot lines, tot lines with errors

              let showDateRange
                         = case maybeDateRange of
                             Nothing -> "null,null"
                             Just (v1, v2) -> showLocalTimeUsingMySQLFormat v1 ++ "," ++ showLocalTimeUsingMySQLFormat v2

              putStrLn $ showDateRange ++ "," ++ show totLines ++ "," ++ show totLinesWithErrors
              return ())

    [Exec_TestImportDataFile fileName,
     Exec_ProviderName providerName,
     Exec_FileLogicalType logicalType,
     Exec_FileVersionType versionType]
      -> do let pseudoPrecision = 4
            -- fake

            let logicalTypeId = 1
            -- fake

            let versionTypeId = 1
            -- fake

            let providerId = 1
            -- fake

            let dbConf = DBConf {
                           dbConf_user = fromStringToByteString "fake"
                         , dbConf_password = fromStringToByteString "fake"
                         , dbConf_dbName = fromStringToByteString "fake"
                         }

            let isDebugMode = True

            let debugFileName = Just "debug.log"

            let isStatus = False

            let deleteInputFile = False

            let maybeTimeFrame = Nothing

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

              rateEngine_importDataFile
                 debugFileName
                 fileName
                 deleteInputFile
                 False
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
                 False

              return ())

    [Exec_ExportCDRS outFileName,
     Exec_ProviderName providerName,
     Exec_ProviderId providerIdS,
     Exec_FileLogicalType logicalType,
     Exec_FileLogicalTypeId logicalTypeIdS,
     Exec_FileVersionType versionType,
     Exec_FileVersionTypeId versionTypeIdS,
     Exec_FromDate fromDateS,
     Exec_ToDate toDateS,
     Exec_UseOnlyCDRSToMove useOnlyCdrsToMoveS,
     Exec_DBName dbName,
     Exec_DBUser dbUser,
     Exec_DBPassword dbPassword
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

            let dbConf = DBConf {
                           dbConf_user =  fromStringToByteString dbUser
                         , dbConf_password = fromStringToByteString dbPassword
                         , dbConf_dbName = fromStringToByteString dbName
                         }

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
                useOnlyCdrsToMove
                dbConf

              -- Return the info in the format expected from the PHP caller:
              -- > min call-date, max call-date, tot lines, tot lines with errors
              return ())

    [ Exec_Rate
     ,Exec_DebugMode debugModeS
     ,Exec_RunLevel runLevelS
     ,Exec_IsVoipReseller isVoipResellerS
     ,Exec_DigitsToMask digitsToMaskS
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefixS
     ,Exec_CurrencyPrecision precision
     ,Exec_DebugFileName debugFileName
     ,Exec_FromDate fromDateS
     ,Exec_ToDate toDateS
     ,Exec_FromDate fromServiceDateS
     ,Exec_ToDate toServiceDateS
     ,Exec_RateUnbilledCalls rateUnbilledCallsS
     ,Exec_DBName dbName
     ,Exec_DBUser dbUser
     ,Exec_DBPassword dbPassword
     ]

      -> do

            fromCallDate <- parseTimeFrameOrExit fromDateS
            toCallDate <- parseTimeFrameOrExit toDateS

            serviceTimeFrame <- parseMaybeTimeFrame fromServiceDateS toServiceDateS

            digitsToMask
              <- case fromTextToInt $ Text.pack digitsToMaskS of
                   Just i
                     -> do return i
                   Nothing
                      -> do putStrLn $ "Unrecognized digitsToMask int \"" ++ digitsToMaskS ++ "\"."
                            exitFailure

            runLevel
              <- case runLevelS of
                   "0" -> return RunLevelFull
                   "1" -> return RunLevelUntilRating
                   "2" -> return RunLevelUntilPortedTelephoneNumberConversion
                   "3" -> return RunLevelUntilSourceCDRParsing
                   "4" -> return RunLevelUntilReadingFromDB
                   _   -> do putStrLn $ "Unrecognized run-level value \"" ++ runLevelS ++ "\"."
                             exitFailure

            (isDebugMode, maybeDebugFile)
              <- case debugModeS of
                   "0" -> return (False, Nothing)
                   "1" -> return (True, Just debugFileName)
                   _   -> do putStrLn $ "Unrecognized debug mode value \"" ++ debugModeS ++ "\"."
                             exitFailure

            isVoipReseller
              <- case isVoipResellerS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized isVoipReseller mode value \"" ++ debugModeS ++ "\"."
                             exitFailure


            rateUnbilledCalls
              <- case rateUnbilledCallsS of
                   "false" -> return False
                   "true" -> return True
                   _   -> do putStrLn $ "Unrecognized param value."
                             exitFailure

            let maybeDefaultTelephonePrefix = if List.null defaultTelephonePrefixS then Nothing else (Just $ Text.pack defaultTelephonePrefixS)
            
            let currencyPrecision = fromJust1 "ma1" $ fromTextToInt (Text.pack precision)

            let env
                  = InitialRatingParams {
                        iparams_isDebugMode = isDebugMode
                      , iparams_isVoipReseller = isVoipReseller 
                      , iparams_digitsToMask = digitsToMask
                      , iparams_defaultTelephonePrefixToNotDisplay = maybeDefaultTelephonePrefix
                      , iparams_currencyPrecision = currencyPrecision
                      , iparams_debugFileName = debugFileName
                      , iparams_fromDate = fromCallDate
                      , iparams_toDate = toCallDate
                      , iparams_isRateUnbilledCallsEvent = rateUnbilledCalls
                      , iparams_onlyImportedServices = False
                      , iparams_dbName = dbName
                      , iparams_dbUser = dbUser
                      , iparams_dbPasswd = dbPassword
                      , iparams_configuredRatePlanParsers = configuredRateParsers 
                      }

            handleAny
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

                -- first rate service CDRS imported from other resellers, if there are any
                stats1
                  <- case serviceTimeFrame of
                       Nothing
                         -> return 0
                       Just (d1, d2)
                         -> rateEngine_rate runLevel (env { iparams_fromDate = d1, iparams_toDate = d2, iparams_onlyImportedServices = True })

                -- then rate normal calls
                stats2
                  <- rateEngine_rate runLevel env

                putStr $ show (stats1 + stats2)
                return ())

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

parseMaybeTimeFrame :: String -> String -> IO (Maybe (LocalTime, LocalTime))
parseMaybeTimeFrame fromDateS toDateS
  = case fromDateS == "null" of
      True
        -> return Nothing
      False
        -> do fromDate <- parseTimeFrameOrExit fromDateS
              toDate <- parseTimeFrameOrExit toDateS
              return $ Just (fromDate, toDate)
              return $ Just (fromDate, toDate)


