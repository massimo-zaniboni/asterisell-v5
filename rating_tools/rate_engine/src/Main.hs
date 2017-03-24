{-# LANGUAGE OverloadedStrings, CPP, TemplateHaskell, ScopedTypeVariables #-}

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

module Main (
    main
) where


import Asterisell.Cdr
import Asterisell.Error
import Asterisell.RatePlan
import Asterisell.MainRatePlan
import Asterisell.CSVFileRatePlan
import Asterisell.RateEngine
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.CdrsToRate
import Asterisell.CustomerSpecificImporters
import Asterisell.CustomerSpecificRates
import Asterisell.Services

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.List as List
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Pipes.Safe as PS
import qualified Pipes.Safe.Prelude  as PS
import qualified Pipes.Prelude as PP
import qualified Pipes.Csv as PC
import qualified Data.Csv.Parser as PC
import Control.Exception
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
import qualified Data.Char as Char
import Control.Monad.Catch

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

--------------------
-- CONFIGURATIONS --
--------------------

decodeOptions1 :: SourceCDRParams
decodeOptions1
  = SourceCDRParamsCSVFile Nothing ';' UseUTF8

-- | The default rate parsers to use for importing rates.
--   NOTE: the parsers are configured on a separate module for each type of parser.
configuredRateParsers :: ConfiguredRatePlanParsers
configuredRateParsers
  = Map.fromList $
      [ ("rate-plan-specification", parseRatePlanUsingParsecParser mainRatePlanParser_parse)
      , ("csv-header-3col", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 1 0 True False False False ))
      , ("csv-header-3col-last-descr", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 0 1 True False False False))
      , ("csv-header-4col", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 2 0 True False False False ))
      , ("csv-header-3col-italian", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' ',' 1 0 True False False False ))
      , ("csv-header-4col-costOnCall", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 1 0 True False True False ))
      , ("csv-header-5col-costOnCall", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 2 0 True False True False ))
      , ("csv-header-6col-costOnCall", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',''.' 3 0 True False True False ))
      , ("csv-twt-header-7col", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 1 4 True False False False ))
      , ("csv-twt-header-7col-italian", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ';' ',' 1 4 True False False False ))
      , ("csv-twt-no-header-7col", parseRatePlanUsingAttoParsec (parse_csvStdFormat True ',' '.' 1 4 True False False False ))
      , ("csv-twt-nng-5col", parseRatePlanUsingAttoParsec (parse_twtNngFormat True '.'))
      , ("csv-twt-header-5col", parseRatePlanUsingAttoParsec (parse_twtNngFormat5ColPrefixFirst True '.'))
      , ("csv-twt-no-header-5col", parseRatePlanUsingAttoParsec (parse_twtNngFormat5ColPrefixFirst False '.'))
      , ("csv-gamma-header-9col", parseRatePlanUsingAttoParsec parse_gammaFormat9Col)
      , ("csv-gamma-item-rental-6col", parseRatePlanUsingAttoParsec parse_gammaItemRentalFormat6Col)
      , ("csv-header-3col-pref-descr-rate-it", parseRatePlanUsingAttoParsec (parse_csvWith3ColsPDR True ';' ','))
      , ("csv-header-3col-pref-descr-rate", parseRatePlanUsingAttoParsec (parse_csvWith3ColsPDR True ',' '.'))
      , ("csv-digitel-nng", parseRatePlanUsingAttoParsec (parse_digitelNNGFormat))
      ]

-- | Helper function for creating a source-cdr having a typical/standard format.
createStandardSourceCDRImporter
  :: forall a . (CDRFormat a)
  => (String, String)
  -> Maybe a
  -- ^ used only for forcing the type constraints on the pipe, the value is ignored, so you can pass Nothing
  -> CurrencyPrecisionDigits
  -> SourceCDRParams
  -> ((BS.ByteString, BS.ByteString), (SourceCDRParams, SourceCDRImporter, CDRImporter))

createStandardSourceCDRImporter (formatName, formatVersion) t precision params
  =   ( (fromStringToByteString formatName, fromStringToByteString formatVersion)
      , (params, extractLinesFromCSVFile params, importSourceCDRFromCSVLine t precision params)
      )

-- | The default CDRs parsers to use for importing CDRs.
--   NOTE: the importers are specified in Cdr module.
configuredCDRSImporters :: CurrencyPrecisionDigits -> ConfiguredCDRSImporters
configuredCDRSImporters precision
  = HashMap.fromList $
      [ createStandardSourceCDRImporter ("asterisell-standard", "v1") (Nothing::(Maybe CSVFormat_AsterisellStandard_V1)) precision sourceCDRParams_default
      , createStandardSourceCDRImporter ("import-from-v3", "format1") (Nothing::(Maybe CSVFormat_ImportFromV3_Format1)) precision sourceCDRParams_default
      , createStandardSourceCDRImporter ("asterisell-provider", "v1") (Nothing::(Maybe CSVFormat_asterisell_provider__v1)) precision sourceCDRParams_default
      , createStandardSourceCDRImporter ("asterisell-provider-services", "v1") (Nothing::(Maybe CSVFormat_asterisell_provider_services__v1)) precision sourceCDRParams_default
      , createStandardSourceCDRImporter ("twt-cps","v1") (Nothing::(Maybe CSVFormat_twt_cps__v1)) precision decodeOptions1
      , createStandardSourceCDRImporter ("twt-nng","v1")  (Nothing::(Maybe CSVFormat_twt_nng__v1)) precision decodeOptions1
      , createStandardSourceCDRImporter ("free-radius","v1") (Nothing::(Maybe CSVFormat_freeRadius__v1)) precision sourceCDRParams_default
      , createStandardSourceCDRImporter ("asterisk","generic") (Nothing::(Maybe CSVFormat_asterisk__generic)) precision sourceCDRParams_default
      , createStandardSourceCDRImporter ("asterisk","a_p_v1") (Nothing::(Maybe CSVFormat_asterisk__a_p_v1)) precision sourceCDRParams_default

      , createStandardSourceCDRImporter
          ("plain","1")
          (Nothing::(Maybe CSVFormat_plain1))
          precision
          (SourceCDRParamsCSVFile
             (Just $ "cdrdate;callernumber;callednumber;callername;calledname;duration;uniqueid")
             ';'
             UseUTF8)

      , createStandardSourceCDRImporter
          ("plain","2")
          (Nothing::(Maybe CSVFormat_plain1))
          precision
          sourceCDRParams_default

      , createStandardSourceCDRImporter
          ("digitel","v1")
          (Nothing::(Maybe CSVFormat_digitel))
          precision
          (SourceCDRParamsCSVFile (Just const_digitelHeader) ';' UseUTF8)
          
      , createStandardSourceCDRImporter
          ("digitel-nng","v1")
          (Nothing::(Maybe CSVFormat_digitelNNG__v1))
          precision
          (SourceCDRParamsCSVFile (Just const_digitelHeader) ';' UseUTF8)
 
      , createStandardSourceCDRImporter
           ("gamma","v1")
           (Nothing::(Maybe CSVFormat_gamma__v1))
           precision
           (SourceCDRParamsCSVFile
           (Just $ "\"Call Type\",\"Call Cause Definition Required\",\"Customer Identifier\",\"Non-Charged Party\",\"Call Date\",\"Call Time\",\"Duration\",\"Bytes Transmitted\",\"Bytes Received\",\"Description\",\"Chargecode\",\"Time Band\",\"Salesprice\",\"Salesprice (Pre-Bundle)\",\"Extension\",\"DDI\",\"Grouping ID\",\"Call Class (Feature)\",\"Carrier\",\"Recording\",\"VAT\",\"Country of Origin\",\"Network\",\"Retail Tariff Code\",\"Remote Network\",\"APN\",\"Diverted Number\",\"Ring time\",\"Record ID\",\"Currency\",\"Caller Line Identity\",\"Network Access Reference\",\"NGCS Access Charge\",\"NGCS Service Charge\",\"Total Bytes Transferred\",\"User ID\",\"Onward Billing Reference\",\"Contract Name\",\"Bundle Name\",\"Bundle Allowance\",\"Discount Reference\",\"Routing Code\"")
                                            ','
             UseUTF8)

      , createStandardSourceCDRImporter ("gamma-item-rental","v1")
                                        (Nothing::(Maybe CSVFormat_gamma_ItemRental__v1))
                                        precision
                                        sourceCDRParams_default

      , createStandardSourceCDRImporter
          ("colt","v1")
          (Nothing::(Maybe CSVFormat_colt))
          precision
          (SourceCDRParamsCSVFile Nothing ';' UseUTF8)
       , createStandardSourceCDRImporter
          ("colt43","v1")
          (Nothing::(Maybe CSVFormat_colt43))
          precision
          (SourceCDRParamsCSVFile Nothing ';' UseUTF8)
      ]

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
  | Exec_LoadRateCategories FileName
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
  | Exec_LoadTelephonePrefixes FileName
  | Exec_LoadExtensions FileName
  | Exec_LoadVendors FileName
  | Exec_LoadChannelTypes FileName
  | Exec_LoadChannelDomains FileName
  | Exec_LoadRatePlanChanges FileName
  | Exec_LoadRatePlan FileName
  | Exec_DigitsToMask String
  | Exec_DefaultTelephonePrefix String
  | Exec_CurrencyPrecision String
  | Exec_CompileRate FileName
  | Exec_DBUser String
  | Exec_DBPassword String
  | Exec_DBName String
  | Exec_ResultFile FileName
  | Exec_LoadServices FileName
  | Exec_LoadServicePriceList FileName
  | Exec_LoadAssignedServices FileName
  | Exec_CompileServices
  | Exec_DebugMode String
  | Exec_DebugFileName FileName
  | Exec_NamedPipe FileName
  | Exec_SignalFile FileName
  | Exec_RateUnbilledCalls String
  | Exec_IsStatusFile String
  | Exec_UpdateCachedGroupedCDRS
  | Exec_ExportCDRS FileName
  | Exec_UseOnlyCDRSToMove String
  | Exec_TestImportDataFile String
  | Exec_TestRateParsing String
  | Exec_IsVoipReseller String

options :: [OptDescr Flag]
options = [ Option [] ["help"] (NoArg Exec_Help) "help"
          , Option [] ["rate"] (NoArg Exec_Rate) "rate"
          , Option [] ["debug"] (NoArg Exec_Debug) "debug"
          , Option ['V'] ["version"] (NoArg Exec_Version) "show version number"
          , Option [] ["test"] (NoArg Exec_Test) "execute some tests on the code"
          , Option [] ["test-organizations"] (ReqArg Exec_TestOrganizations "NR") "execute some tests on the organization structure. Data must be set from the PHP world."
          , Option [] ["load-rate-categories"] (ReqArg Exec_LoadRateCategories "FILE") ""
          , Option [] ["load-telephone-prefixes"] (ReqArg Exec_LoadTelephonePrefixes "FILE") "load telephone prefix table"
          , Option [] ["load-extensions"] (ReqArg Exec_LoadExtensions "FILE") "load extensions and organizations info"
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
          , Option [] ["pipe-name"] (ReqArg Exec_NamedPipe "FILENAME") "unique pipe name"
          , Option [] ["signal"] (ReqArg Exec_SignalFile "FILENAME") ""
          , Option [] ["is-status-file"] (ReqArg Exec_IsStatusFile "true|false") ""
          , Option [] ["from-date"] (ReqArg Exec_FromDate "\"YYYY-MM-DD hh:mm:ss\"") "rate starting from the specified date, inclusive"
          , Option [] ["to-date"] (ReqArg Exec_ToDate "\"YYYY-MM-DD hh:mm:ss\"") "rate to the specified date, exclusive"
          , Option [] ["load-vendors"] (ReqArg Exec_LoadVendors "FILENAME") ""
          , Option [] ["load-channels-types"] (ReqArg Exec_LoadChannelTypes "FILENAME") ""
          , Option [] ["load-channel-domains"] (ReqArg Exec_LoadChannelDomains "FILENAME") ""
          , Option [] ["load-rate-plan-changes"] (ReqArg Exec_LoadRatePlanChanges "FILENAME") "A list of rate plan changes."
          , Option [] ["load-rate-plan"] (ReqArg Exec_LoadRatePlan "FILENAME") "The base file name for specific rate planes."
          , Option [] ["load-services"]   (ReqArg Exec_LoadServices "FILENAME") "The filename with service descriptions."
          , Option [] ["load-service-price-list"]   (ReqArg Exec_LoadServicePriceList "FILENAME") "The filename with service price list."
          , Option [] ["load-assigned-services"]   (ReqArg Exec_LoadAssignedServices "FILENAME") "The filename with assigned services."
          , Option [] ["digits-to-mask"] (ReqArg Exec_DigitsToMask "NUMBER") "The number of digits to mask displaying telephone numbers"
          , Option [] ["debug-mode"] (ReqArg Exec_DebugMode "NUMBER") "0 for rating in production mode, 1 for enabling run-time tests during rating."
          , Option [] ["debug-file"] (ReqArg Exec_DebugFileName "FILENAME") "The file where writing debug info."
          , Option [] ["default-telephone-prefix"] (ReqArg Exec_DefaultTelephonePrefix "TELEPHONE-PREFIX") "the telephone prefix used in local calls, and that can be omitted during display of the calls"
          , Option [] ["currency-precision"] (ReqArg Exec_CurrencyPrecision "NUMBER") "the number of decimal digits to use for cost and incomes"
          , Option [] ["compile-rate"] (ReqArg Exec_CompileRate "FILENAME") ""
          , Option [] ["compile-services"] (NoArg Exec_CompileServices) ""
          , Option [] ["db-user"] (ReqArg Exec_DBUser "user-name") "the database username"
          , Option [] ["db-password"] (ReqArg Exec_DBPassword "password") "the database password"
          , Option [] ["db-name"] (ReqArg Exec_DBName "database-name") "the database name"
          , Option [] ["rate-unbilled-calls"] (ReqArg Exec_RateUnbilledCalls "0|1") ""
          , Option [] ["update-cached-grouped-cdrs"] (NoArg Exec_UpdateCachedGroupedCDRS) "refresh the cache with grouped CDRS"
          , Option [] ["export-cdrs"] (ReqArg Exec_ExportCDRS "FILENAME") "export cdrs"
          , Option [] ["use-only-cdrs-to-move"] (ReqArg Exec_UseOnlyCDRSToMove "true|false") "export only cdrs in ar_source_cdr_to_move table"
          , Option [] ["test-rate-parsing"] (ReqArg Exec_TestRateParsing "filename") "test the parsing, calling parse_test function"
          , Option [] ["is-voip-reseller"] (ReqArg Exec_IsVoipReseller "0|1") "0 for call reporting, 1 for voip reseller (billing)"
          ]

header = "Usage: main [OPTION...]"

runTestList l
  = do c <- HUnit.runTestTT $ HUnit.TestList l
       case (HUnit.errors c) > 0 || (HUnit.failures c) > 0 of
         True -> exitFailure
         False -> return ()

main = do
  Control.Exception.catch
        mainRate
        (\(e :: IOException)
            -> do putStrLn ("\n" ++ show e)
                  exitFailure
        )

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
    [Exec_UpdateCachedGroupedCDRS,
     Exec_DBName dbName,
     Exec_DBUser dbUser,
     Exec_DBPassword dbPassword
     ]
      -> do let dbConf = DBConf {
                           dbConf_user = dbUser
                         , dbConf_password = dbPassword
                         , dbConf_dbName = dbName
                         }

            rateEngine_openDBAndUpdateCachedGroupedCDRS dbConf
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

    [ Exec_TestOrganizations pass
     ,Exec_FromDate fromDateS
     ,Exec_LoadRateCategories rateCategories
     ,Exec_LoadVendors vendors
     ,Exec_LoadChannelTypes channelsTypes
     ,Exec_LoadChannelDomains channelsDomains
     ,Exec_LoadTelephonePrefixes telephonePrefixes
     ,Exec_DigitsToMask digitsToMask
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
     ,Exec_CurrencyPrecision precision
     ,Exec_LoadExtensions extensions
     ,Exec_LoadRatePlanChanges ratePlanChanges
     ,Exec_LoadRatePlan ratePlanBaseFileName]

      -> do
            let fromDate = fromJust1 "ma3" $ fromMySQLDateTimeToLocalTime fromDateS

            maybeRateCategoriesInfo <- PS.runSafeT $! rateCategories_load True rateCategories
            rateCategoriesInfo
              <- case maybeRateCategoriesInfo of
                   Left msg
                     -> do putStrLn msg
                           exitFailure
                   Right r
                     -> do return r

            maybeVendorsInfo <- parseFileWithVendors True vendors
            vendorsInfo
              <- case maybeVendorsInfo of
                   Left msg
                     -> do putStrLn msg
                           exitFailure
                   Right r
                     -> do return r

            maybeChannelsTypesInfo <- parseFileWithChannelTypes True channelsTypes
            channelsTypesInfo
              <- case maybeChannelsTypesInfo of
                   Left msg
                     -> do putStrLn msg
                           exitFailure
                   Right r
                     -> do return r

            maybeChannelsDomainsInfo <- parseFileWithChannelDomains True channelsDomains
            channelsDomainsInfo
              <- case maybeChannelsDomainsInfo of
                   Left msg
                     -> do putStrLn msg
                           exitFailure
                   Right r
                     -> do return r

            maybeTelephonePrefixesInfo <- parseFileWithTelephonePrefixes True telephonePrefixes
            telephonePrefixesInfo
              <- case maybeTelephonePrefixesInfo of
                   Left (isApplicationError, msg)
                     -> do putStrLn msg
                           case isApplicationError of
                             True
                               -> exitWith applicationError
                             False
                               -> exitWith telephonePrefixesConfigurationError
                   Right r
                     -> do return r

            let maybeDefaultTelephonePrefix = if List.null defaultTelephonePrefix then Nothing else Just defaultTelephonePrefix

            maybeExtensionsInfo <- parseFileWithExtensions True extensions
            extensionsInfo
              <- case maybeExtensionsInfo of
                   Left (isApplicationError, msg)
                     -> do putStrLn msg
                           case isApplicationError of
                             True -> exitWith applicationError
                             False -> exitWith extensionConfigurationError
                   Right r
                     -> do return r

            let params = EnvParams {
                           params_rateCategories = rateCategoriesInfo
                         , params_channelTypes = channelsTypesInfo
                         , params_vendors = vendorsInfo
                         , params_channelDomains = channelsDomainsInfo
                         , params_telephonePrefixes = telephonePrefixesInfo
                         , params_organizations = extensionsInfo
                         , params_numberOfDigitsToMask = fromJust1 "ma4" $ fromTextToInt (Text.pack digitsToMask)
                         , params_defaultTelephonePrefixToNotDisplay = maybeDefaultTelephonePrefix
                         , params_currencyPrecision = fromJust1 "ma5" $ fromTextToInt (Text.pack precision)
                         }

            maybeRatePlanInfo <- PS.runSafeT $! ratingEnv_load ratePlanChanges ratePlanBaseFileName fromDate Nothing configuredRateParsers params Nothing
            ratePlanInfo
              <- case maybeRatePlanInfo of
                   Left msg
                     -> do putStrLn $ show msg
                           exitFailure
                   Right r
                     -> do return r

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
                           dbConf_user = dbUser
                         , dbConf_password = dbPassword
                         , dbConf_dbName = dbName
                         }

            isDebugMode
              <- case debugModeS of
                   "0" -> return False
                   "1" -> return True
                   _   -> do putStrLn $ "Unrecognized debug mode value \"" ++ debugModeS ++ "\"."
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

            Control.Monad.Catch.handle
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

              (maybeDateRange, maybeImportedServiceDateRange, totLines, totLinesWithErrors)
                <- rateEngine_importDataFile debugFileName (configuredCDRSImporters pseudoPrecision) fileName deleteInputFile (Text.pack providerName) providerId (fromStringToByteString logicalType) logicalTypeId (fromStringToByteString versionType) versionTypeId isStatus maybeTimeFrame dbConf pseudoPrecision
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
            -- NOTE: in calldate importing the precision is not important
            Control.Monad.Catch.handle
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do

              rateEngine_testImportDataFile (configuredCDRSImporters pseudoPrecision) fileName (Text.pack providerName) (fromStringToByteString logicalType) (fromStringToByteString versionType) pseudoPrecision
              return ())

    [Exec_TestRateParsing fileName]
      -> do testParserUsingAttoParsec parse_test fileName

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
                           dbConf_user = dbUser
                         , dbConf_password = dbPassword
                         , dbConf_dbName = dbName
                         }

            let timeFrame
                  = (fromJust1 "ma6" $ fromMySQLDateTimeToLocalTime fromDateS, fromJust1 "imp3" $ fromMySQLDateTimeToLocalTime toDateS)

            Control.Monad.Catch.handle
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do


              rateEngine_exportDataFile (configuredCDRSImporters pseudoPrecision) outFileName (Text.pack providerName) providerId (fromStringToByteString logicalType) logicalTypeId (fromStringToByteString versionType) versionTypeId  timeFrame useOnlyCdrsToMove dbConf
              -- Return the info in the format expected from the PHP caller:
              -- > min call-date, max call-date, tot lines, tot lines with errors
              return ())

    [ Exec_Rate
     ,Exec_DebugMode debugModeS
     ,Exec_IsVoipReseller isVoipResellerS
     ,Exec_LoadRateCategories rateCategories
     ,Exec_LoadVendors vendors
     ,Exec_LoadChannelTypes channelsTypes
     ,Exec_LoadChannelDomains channelsDomains
     ,Exec_LoadTelephonePrefixes telephonePrefixes
     ,Exec_DigitsToMask digitsToMask
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
     ,Exec_CurrencyPrecision precision
     ,Exec_LoadExtensions extensions
     ,Exec_LoadRatePlanChanges ratePlanChanges
     ,Exec_LoadRatePlan ratePlanBaseFileName
     ,Exec_LoadServices servicesFileName
     ,Exec_LoadServicePriceList servicePriceListFileName
     ,Exec_LoadAssignedServices assignedServicesFileName
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

            let dbConf = DBConf {
                           dbConf_user = dbUser
                         , dbConf_password = dbPassword
                         , dbConf_dbName = dbName
                         }

            normalTimeFrame <- parseMaybeTimeFrame fromDateS toDateS
            serviceTimeFrame <- parseMaybeTimeFrame fromServiceDateS toServiceDateS
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

            params
              <- prepareEnvParams isDebugMode
                                  [Exec_LoadRateCategories rateCategories
                                  ,Exec_LoadVendors vendors
                                  ,Exec_LoadChannelTypes channelsTypes
                                  ,Exec_LoadChannelDomains channelsDomains
                                  ,Exec_LoadTelephonePrefixes telephonePrefixes
                                  ,Exec_DigitsToMask digitsToMask
                                  ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
                                  ,Exec_CurrencyPrecision precision
                                  ,Exec_LoadExtensions extensions
                                  ]

            maybeServiceParams
              <- loadServiceParams isDebugMode servicesFileName servicePriceListFileName assignedServicesFileName params

            serviceParams
              <- case maybeServiceParams of
                   Left err
                     -> do putStrLn ("\nError during reading of service configurations: " ++ err)
                           exitWith rateConfigurationError
                   Right r
                     -> do return r

            let currencyPrecision = params_currencyPrecision params

            Control.Monad.Catch.handle
              (\(err :: SomeException)
                            -> do putStrLn $ show err
                                  exitFailure) (do
                stats
                  <- rateEngine_incrementalRate
                       isVoipReseller
                       (configuredCDRSImporters currencyPrecision)
                       dbConf
                       maybeDebugFile
                       normalTimeFrame
                       serviceTimeFrame
                       rateUnbilledCalls
                       configuredRateParsers
                       ratePlanChanges
                       ratePlanBaseFileName
                       params
                       serviceParams

                putStr $ show stats
                return ())


    [ Exec_CompileServices
     ,Exec_LoadRateCategories rateCategories
     ,Exec_LoadVendors vendors
     ,Exec_LoadChannelTypes channelsTypes
     ,Exec_LoadChannelDomains channelsDomains
     ,Exec_LoadTelephonePrefixes telephonePrefixes
     ,Exec_LoadServices servicesFileName
     ,Exec_LoadServicePriceList servicePriceListFileName
     ,Exec_LoadAssignedServices assignedServicesFileName
     ,Exec_DigitsToMask digitsToMask
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
     ,Exec_CurrencyPrecision precision
     ,Exec_LoadExtensions extensions
     ,Exec_ResultFile resultFileName
     ]

      -> do
            params
              <- prepareEnvParams False
                                  [Exec_LoadRateCategories rateCategories
                                  ,Exec_LoadVendors vendors
                                  ,Exec_LoadChannelTypes channelsTypes
                                  ,Exec_LoadChannelDomains channelsDomains
                                  ,Exec_LoadTelephonePrefixes telephonePrefixes
                                  ,Exec_DigitsToMask digitsToMask
                                  ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
                                  ,Exec_CurrencyPrecision precision
                                  ,Exec_LoadExtensions extensions
                                  ]

            maybeServiceParams
              <- loadServiceParams False servicesFileName servicePriceListFileName assignedServicesFileName params

            case maybeServiceParams of
                   Right serviceParams
                     -> do BL.writeFile resultFileName $ service_exportToCSVTheServiceCDRSTelephonePrefixes serviceParams
                           return ()
                   _
                     -> do writeFile resultFileName ""
                           -- do nothing, because errors in rate will be signaled in other phases of the rating process
                           return ()

            return ()


    [ Exec_CompileRate rateFileName
     ,Exec_LoadRateCategories rateCategories
     ,Exec_LoadVendors vendors
     ,Exec_LoadChannelTypes channelsTypes
     ,Exec_LoadChannelDomains channelsDomains
     ,Exec_LoadTelephonePrefixes telephonePrefixes
     ,Exec_LoadServices servicesFileName
     ,Exec_LoadServicePriceList servicePriceListFileName
     ,Exec_LoadAssignedServices assignedServicesFileName
     ,Exec_DigitsToMask digitsToMask
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
     ,Exec_CurrencyPrecision precision
     ,Exec_LoadExtensions extensions
     ,Exec_ResultFile resultFileName
     ]

      -> do
            params
              <- prepareEnvParams False
                                  [Exec_LoadRateCategories rateCategories
                                  ,Exec_LoadVendors vendors
                                  ,Exec_LoadChannelTypes channelsTypes
                                  ,Exec_LoadChannelDomains channelsDomains
                                  ,Exec_LoadTelephonePrefixes telephonePrefixes
                                  ,Exec_DigitsToMask digitsToMask
                                  ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
                                  ,Exec_CurrencyPrecision precision
                                  ,Exec_LoadExtensions extensions
                                  ]

            maybeParsedRate :: Either Parsec.ParseError MainRatePlan
              <- Parsec.parseFromFile (mainRatePlanParser_parse params) rateFileName

            case maybeParsedRate of
                   Right parsedRate
                     -> do BL.writeFile resultFileName $ mainRatePlan_exportToCSVTheServiceCDRSTelephonePrefixes parsedRate
                           return ()
                   _
                     -> do writeFile resultFileName ""
                           -- do nothing, because errors in rate will be signaled in other phases of the rating process
                           return ()

            return ()
    [Exec_Debug]
      -> do -- Test some code, during development of the rate engine

            return ()

    _ -> do putStrLn "\nunrecognized options"
            exitFailure

prepareEnvParams :: Bool -> [Flag] -> IO EnvParams
prepareEnvParams
   isDebugMode
   [ Exec_LoadRateCategories rateCategories
     ,Exec_LoadVendors vendors
     ,Exec_LoadChannelTypes channelsTypes
     ,Exec_LoadChannelDomains channelsDomains
     ,Exec_LoadTelephonePrefixes telephonePrefixes
     ,Exec_DigitsToMask digitsToMask
     ,Exec_DefaultTelephonePrefix defaultTelephonePrefix
     ,Exec_CurrencyPrecision precision
     ,Exec_LoadExtensions extensions
   ] = do

            maybeRateCategoriesInfo <- PS.runSafeT $! rateCategories_load isDebugMode rateCategories
            rateCategoriesInfo
              <- case maybeRateCategoriesInfo of
                   Left msg
                     -> do putStrLn ("\nError parsing \"" ++ rateCategories ++ "\": " ++ msg)
                           exitFailure
                   Right r
                     -> do return r

            maybeVendorsInfo <- parseFileWithVendors isDebugMode vendors
            vendorsInfo
              <- case maybeVendorsInfo of
                   Left msg
                     -> do putStrLn ("\nError parsing \"" ++ vendors ++ "\": " ++ msg)
                           exitFailure
                   Right r
                     -> do return r

            maybeChannelsTypesInfo <- parseFileWithChannelTypes isDebugMode channelsTypes
            channelsTypesInfo
              <- case maybeChannelsTypesInfo of
                   Left msg
                     -> do putStrLn ("\nError parsing \"" ++ channelsTypes ++ "\": " ++ msg)
                           exitFailure
                   Right r
                     -> do return r

            maybeChannelsDomainsInfo <- parseFileWithChannelDomains isDebugMode channelsDomains
            channelsDomainsInfo
              <- case maybeChannelsDomainsInfo of
                   Left msg
                     -> do putStrLn ("\nError parsing \"" ++ channelsDomains ++ "\": " ++ msg)
                           exitFailure
                   Right r
                     -> do return r

            maybeTelephonePrefixesInfo <- parseFileWithTelephonePrefixes isDebugMode telephonePrefixes
            telephonePrefixesInfo
              <- case maybeTelephonePrefixesInfo of
                   Left (isApplicationError, msg)
                     -> do putStrLn ("\nError parsing \"" ++ telephonePrefixes ++ "\": " ++ msg)
                           case isApplicationError of
                             True
                               -> exitWith applicationError
                             False
                               -> exitWith telephonePrefixesConfigurationError

                   Right r
                     -> do return r

            let maybeDefaultTelephonePrefix = if List.null defaultTelephonePrefix then Nothing else Just defaultTelephonePrefix

            maybeExtensionsInfo <- parseFileWithExtensions isDebugMode extensions
            extensionsInfo
              <- case maybeExtensionsInfo of
                   Left (isApplicationError, msg)
                     -> do putStrLn $ "\nError parsing \"" ++ extensions ++ "\": " ++ msg
                           case isApplicationError of
                             True -> exitWith applicationError
                             False -> exitWith extensionConfigurationError
                   Right r
                     -> do return r

            let currencyPrecision = fromJust1 "ma1" $ fromTextToInt (Text.pack precision)

            return $ EnvParams {
                           params_rateCategories = rateCategoriesInfo
                         , params_channelTypes = channelsTypesInfo
                         , params_vendors = vendorsInfo
                         , params_channelDomains = channelsDomainsInfo
                         , params_telephonePrefixes = telephonePrefixesInfo
                         , params_organizations = extensionsInfo
                         , params_numberOfDigitsToMask = fromJust1 "ma2" $ (fromTextToInt (Text.pack digitsToMask))
                         , params_defaultTelephonePrefixToNotDisplay = maybeDefaultTelephonePrefix
                         , params_currencyPrecision = currencyPrecision
                         }

parseMaybeTimeFrame :: String -> String -> IO MaybeTimeFrame
parseMaybeTimeFrame fromDateS toDateS
  = case fromDateS == "null" of
      True
        -> return Nothing
      False
        -> do fromDate
                <- case  fromMySQLDateTimeToLocalTime fromDateS of
                     Just r
                       -> return r
                     Nothing
                       -> do putStrLn $ "Unrecognized date format " ++ fromDateS
                             exitFailure

              toDate
                <- case fromMySQLDateTimeToLocalTime toDateS of
                     Just r
                       -> return r
                     Nothing
                       -> do putStrLn $ "Unrecognized date format " ++ toDateS
                             exitFailure

              return $ Just (fromDate, toDate)

