{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings #-}

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

-- | Manage Call Detail Records (CDRs)
--
module Asterisell.Cdr (
  TelephonePrefixId,
  CDR(..),
  cdr_isDerivedServiceCDR,
  cdr_isServiceCDR,
  cdr_isNormalCDR,
  cdr_empty,
  ServiceCDR,
  LocaleConversion(..),
  serviceCdr_defaultCommunicationChannel,
  serviceCdr_defaultExternalTelephoneNumber,
  CDRDirection(..),
  MonetaryValue,
  monetaryValue_show,
  CurrencyPrecisionDigits,
  cdr_uniqueErrorKey,
  cdrDirection_asterisellCode,
  SourceDataFileHandle,
  cdr_showDebug,
  ConfiguredCDRSImporters,
  configuredCDRSImporters_get,
  cdr_toCSVLine,
  toCDR,
  getCallDate,
  CDRFormat,
  IsThereHeader,
  ParentIdHierarchy,
  ImportedCDR,
  CSVFormat_AsterisellStandard_V1,
  CSVFormat_asterisell_provider__v1,
  CSVFormat_asterisell_provider_services__v1,
  fromIntegerWithFixedPrecisionToMonetaryValue,
  CSVFormat_ImportFromV3_Format1,
  ExportMaybeNull(ExportNull, Export),
  ImportMaybeNull,
  fromExport,
  fromExportOrNothing,
  toMaybe,
  isNullExport,
  isEmptyOrNull,
  isNotEmptyOrNull,
  isNotNullExportWith,
  SourceCDRDescription,
  SourceCDRImporter,
  SourceCDRParams (..),
  sourceCDRParams_default,
  CDRImporter,
  CDRProviderName,
  fromExportOrEmptyString,
  toMonetaryValueWithFixedPrecisionInt,
  nullStr,
  addLine,
  cdrField,
  fieldI,
  importNotNullText,
  importAndConvertNotNullValue,
  importAndConvertNotNullValue2
) where

import Asterisell.Error
import Asterisell.VoIPChannelAndVendor
import Asterisell.OrganizationHierarchy as Organization
import Asterisell.TelephonePrefixes
import Asterisell.Trie
import Asterisell.Utils
import Asterisell.OrganizationHierarchy

import qualified Data.Text as Text

import Data.Time.LocalTime
import Data.Maybe
import Data.List as List
import qualified Data.HashMap.Strict as Map
import qualified Data.Hash.MD5 as MD5
import qualified Data.Csv as Csv
import System.IO (IOMode(WriteMode, ReadMode), openFile, hClose, putStrLn)
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad as M
import Data.Char (ord)
import Data.Vector as V hiding((++))
import Data.Time.LocalTime
import Control.Monad.Loops
import Control.Monad.ST
import Control.Applicative.Lift
import Data.STRef.Strict
import Data.Maybe
import Data.List as List
import System.IO
import System.FilePath.Posix
import Data.Attoparsec.Text as P
import Data.Attoparsec.Combinator
import qualified Data.Hash.MD5 as MD5
import Control.Monad.Except
import qualified Test.HUnit as HUnit
import Control.Concurrent.BoundedChan
import Control.Concurrent.MVar
import Data.Csv
import Control.DeepSeq

data CDRDirection
  = CDR_none
  | CDR_incoming
  | CDR_outgoing
  | CDR_internal
  | CDR_ignored
  | CDR_error
  | CDR_system  
 deriving(Show, Eq)

cdrDirection_asterisellCode :: CDRDirection -> Int
cdrDirection_asterisellCode d
  = case d of
      CDR_none -> 0
      CDR_incoming -> 1
      CDR_outgoing -> 2
      CDR_internal-> 3
      CDR_ignored -> 4
      CDR_error -> 5
      CDR_system -> 6

cdrDirection_fromAsterisellCode :: Int -> CDRDirection
cdrDirection_fromAsterisellCode c
  = case c of
      0 -> CDR_none
      1 -> CDR_incoming
      2 -> CDR_outgoing
      3 -> CDR_internal
      4 -> CDR_ignored
      5 -> CDR_error
      6 -> CDR_system

-- | Use arbitrary precision fractions inside Haskell,
--   for representing monetary values.
--
--   NOTE: it is faster in this way Data.Ratio Integer,
--   than Data.Ratio Int
--
type MonetaryValue = Rational

-- | User readable show.
monetaryValue_show :: MonetaryValue -> String
monetaryValue_show v 
  = let f :: Float = fromRational v
    in  show f

type TelephonePrefixId = Int

-- | How many precision digits to use when exporting currencies.
--
type CurrencyPrecisionDigits = Int

-- | The name of CDRProvider, that can be used also as channel-name for associating the CDRs to the correct provider.
type CDRProviderName = Text.Text

-- | Something like "/1/2/5/" where "/1" is the UnitId of the 
--   root organization, "2" the UnitId of its child organization,
--   and "5" the final extension. 
cachedParentIdHierarchy_toText :: ParentIdHierarchy -> Text.Text
cachedParentIdHierarchy_toText h 
  = Text.pack $ "/" ++ List.intercalate "/" (List.map show h) ++ "/"

maybeCachedParentIdHierarchy_toMaybeText mh 
  = case mh of
      Nothing
        -> Nothing
      Just h
        -> Just $ cachedParentIdHierarchy_toText h

-- | A Call Detail Record that can be rated, and exported to Asterisell database.
--   This is the final/internal Asterisell format, for representing the calls.
--   Some fields are directly exported to the external database, while other fields
--   are used from the Rating procedure for instructing how to derive the important fields.
--
--   NULL values are imported and exported using the string "\\N"
data CDR
  = CDR {
      cdr_precision :: Int
      -- ^ the precision to use for cost and income calcs
   
    , cdr_calldate :: !LocalTime
    , cdr_toCalldate :: !(Maybe LocalTime)
    , cdr_isImportedServiceCDR :: !Bool  

    , cdr_countOfCalls :: !Int
    , cdr_direction :: ! CDRDirection

    , cdr_errorDirection :: !CDRDirection
    -- ^ in case of an error in the rating,
    --   this field specify the expected direction of the CDR,
    --   in order to report to admin the type of errors

    , cdr_isRedirect :: !Bool
    , cdr_duration :: !(Maybe Int)
    , cdr_billsec :: !(Maybe Int)
    -- ^ Nothing if the value must be completed from the generic rating utility 

    , cdr_internalTelephoneNumber :: !Text.Text
    -- ^ identify the internal extension/organization/accountcode responsible of the call.
    -- The content of this field is not displayed, but it is used from CDR processing to associate the customer/accountcode to this call

    , cdr_externalTelephoneNumber :: !Text.Text
    -- ^ identify the external telephone number. 
    --   Other fields will be set with the number display and similar info.
    --   For normal import patterns, only this value must be completed, and other values will be derived.
    --   For special import patterns other fields regarding the number to use for rating and display can be set.  
    
    , cdr_organizationUnitId :: !(Maybe UnitId)
    -- ^ the value is always completed from the generic rating utility

    , cdr_bundleOrganizationUnitId :: !(Maybe UnitId)
    -- ^ the value is completed during rating phase, and it can be Nothing.
    --   If this call was rated applying a BundleRate, this is the organization owning the applied BundleRate.
    --   It can be the same cdr_organizationUnitId or a parent of cdr_organizationUnitId.
    
    , cdr_cachedParentIdHierarchy :: !(Maybe ParentIdHierarchy)
    -- ^ the value is always completed from the generic rating utility.
    
    , cdr_billableOrganizationUnitId :: !(Maybe UnitId)
    -- ^ the value is always completed from the generic rating utility 

    , cdr_priceCategoryId :: !(Maybe PriceCategoryId)
    -- ^ the value is always completed from the generic rating utility 
    
    , cdr_income :: !MonetaryValue

    , cdr_channel :: !(Maybe Text.Text)
    -- ^ if completed, the CDR rating procedure will use the ArVendorDomain table, 
    --   for associating the channel type, and vendor id.

    , cdr_communicationChannelTypeId :: !(Maybe ChannelTypeId)
    -- ^ Nothing if the value must be completed from the generic rating utility 

    , cdr_vendorId :: !(Maybe VendorId)
    -- ^ Nothing if the value must be completed from the generic rating utility 
 
    , cdr_cost :: !MonetaryValue

    , cdr_expectedCost  :: !(Maybe MonetaryValue)

    , cdr_costSaving :: !MonetaryValue

    , cdr_telephonePrefixId :: !(Maybe TelephonePrefixId)
    -- ^ Nothing if the value must be completed from the generic rating utility 
   
    , cdr_externalTelephoneNumberWithAppliedPortability :: !(Maybe Text.Text)
    -- ^ Nothing if the value must be completed from the generic rating utility.
    --   The telephone number to use for deciding wich type of rate to apply.
    --   It can contains special prefixes, and so on, because it is not displayed to the user.

    , cdr_displayedExternalTelephoneNumber :: !(Maybe Text.Text)
    -- ^ Nothing if the value must be completed from the generic rating utility.
    --   The telephone number to display in the call report.
    --   It should not contain special prefixes.  

    , cdr_displayedMaskedExternalTelephoneNumber :: !(Maybe Text.Text)
    -- ^ Nothing if the value must be completed from the generic rating utility 
    --   The telephone number to display in the call report.
    --   It should not contain special prefixes.  

    , cdr_problemDuplicationKey  :: ! (Maybe Text.Text)

    , cdr_debug_cost_rate :: ! (Maybe Text.Text)

    , cdr_debug_income_rate :: ! (Maybe Text.Text)

    , cdr_debug_residual_income_rate :: ! (Maybe Text.Text)

    , cdr_debug_residual_call_duration :: ! (Maybe Int)

    , cdr_debug_bundle_left_calls :: ! (Maybe Int)

    , cdr_debug_bundle_left_duration :: ! (Maybe Int)

    , cdr_debug_bundle_left_cost :: ! (Maybe MonetaryValue)

    , cdr_debug_rating_details :: ! (Maybe Text.Text)
  }
 deriving(Show)

instance NFData CDR where
  rnf cdr = seq cdr ()

cdr_empty :: LocalTime -> CurrencyPrecisionDigits -> CDR
cdr_empty t p
  = CDR {
      cdr_precision = p 
    , cdr_calldate = t
    , cdr_toCalldate = Nothing
    , cdr_countOfCalls = 1
    , cdr_isImportedServiceCDR = False                     
    , cdr_isRedirect = False
    , cdr_errorDirection = CDR_none
    , cdr_direction = CDR_none
    , cdr_duration = Nothing
    , cdr_billsec = Nothing
    , cdr_organizationUnitId = Nothing
    , cdr_cachedParentIdHierarchy = Nothing
    , cdr_billableOrganizationUnitId = Nothing
    , cdr_priceCategoryId = Nothing
    , cdr_bundleOrganizationUnitId = Nothing

    , cdr_income = 0
    , cdr_costSaving = 0
    , cdr_channel = Nothing
    , cdr_vendorId = Nothing
    , cdr_communicationChannelTypeId = Nothing
    , cdr_cost = 0
    , cdr_telephonePrefixId = Nothing
    , cdr_displayedExternalTelephoneNumber = Nothing
    , cdr_externalTelephoneNumberWithAppliedPortability = Nothing
    , cdr_displayedMaskedExternalTelephoneNumber = Nothing

    , cdr_internalTelephoneNumber = ""
    , cdr_externalTelephoneNumber = ""
    
    , cdr_expectedCost = Nothing
    , cdr_problemDuplicationKey = Nothing 

    , cdr_debug_cost_rate = Nothing
    , cdr_debug_income_rate = Nothing
    , cdr_debug_residual_income_rate = Nothing
    , cdr_debug_residual_call_duration = Nothing
    , cdr_debug_bundle_left_calls = Nothing
    , cdr_debug_bundle_left_duration = Nothing
    , cdr_debug_bundle_left_cost = Nothing
    , cdr_debug_rating_details = Nothing
  }


-- | A unique key associated to the CDR content, to use during error generation.
--
cdr_uniqueErrorKey :: CDR -> String
cdr_uniqueErrorKey cdr
  =  MD5.md5s (MD5.Str $ show cdr)

-- | A pseudo CDR representing the billing of a bundle-rate service.
type ServiceCDR = CDR

-- | The default communication channel to associate to servive-cdrs.
--   DEV NOTE: keep updated with `ConfigureCommunicationChannels` on the PHP side.
serviceCdr_defaultCommunicationChannel :: Text.Text
serviceCdr_defaultCommunicationChannel = "system-service-cdr"

-- | The default telephone number associated to a service-cdr.
--   The displayed name is another. This is important only for lookup with the prefix table.
serviceCdr_defaultExternalTelephoneNumber :: Text.Text -> Text.Text -> Text.Text
serviceCdr_defaultExternalTelephoneNumber serviceType serviceName 
  = let s = MD5.Str $ (Text.unpack serviceType) ++ "--" ++ (Text.unpack serviceName) 
    in Text.pack $ "system-md5-" ++ (MD5.md5s s)

-- | ServiceCDRS are CDRS associated to services or bundle-rates associated to a time-frame.
cdr_isServiceCDR :: CDR -> Bool
cdr_isServiceCDR cdr = isJust $ cdr_toCalldate cdr

-- | A CDR that is not a service, but a normal call.
cdr_isNormalCDR :: CDR -> Bool
cdr_isNormalCDR cdr = not $ cdr_isServiceCDR cdr

-- | Derived ServiceCDRS are service CDRS derived from the current instance.
cdr_isDerivedServiceCDR :: CDR -> Bool
cdr_isDerivedServiceCDR cdr = (cdr_isServiceCDR cdr) && (not $ cdr_isImportedServiceCDR cdr)

-----------------------------------
-- DEFAULT SOURCE DATA IMPORTERS --
-----------------------------------

-- | A CSV format used internally from Asterisell.
--   NOTE: the cdr_isImportedServiceCDR is managed during rating, if the CDR is a service CDR.
data CSVFormat_AsterisellStandard_V1
  = CSVFormat_AsterisellStandard_V1 {
      f1v1_callDate :: !Text.Text
    , f1v1_toCallDate :: !(ExportMaybeNull Text.Text)
    , f1v1_direction :: !Text.Text
      -- "incoming", "internal", "outgoing", etc...
    , f1v1_isRedirect :: !Text.Text
      -- "0" or "1"
    , f1v1_disposition :: !Text.Text
      -- "ANSWERED", ...
    , f1v1_billsec :: !Text.Text
    , f1v1_internalExtension :: !Text.Text
    , f1v1_externalTelephoneNumber :: !Text.Text
    , f1v1_portedTelephoneNumber :: !Text.Text
    , f1v1_vendorAndChannelDomain :: !Text.Text
    , f1v1_expectedCost :: !Text.Text
 } deriving(Show)

instance FromRecord CSVFormat_AsterisellStandard_V1 where
     parseRecord v =
         let expectedCols = 11
         in case V.length v == expectedCols of
              True
                -> CSVFormat_AsterisellStandard_V1 <$>
                     v .! 0 <*>
                     v .! 1 <*>
                     v .! 2 <*>
                     v .! 3 <*>
                     v .! 4 <*>
                     v .! 5 <*>
                     v .! 6 <*>
                     v .! 7 <*>
                     v .! 8 <*>
                     v .! 9 <*>
                     v .! 10
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

-- | True for an answered call, False for a call to ignore, Nothing for an unrecognized call.
--
default_recognizeAnsweredCall :: Text.Text -> Maybe Bool
default_recognizeAnsweredCall "ANSWERED" = Just True
default_recognizeAnsweredCall "NOT ANSWERED" = Just False
default_recognizeAnsweredCall "IGNORE" = Just False
default_recognizeAnsweredCall _ = Nothing

default_callDirection :: Text.Text -> Maybe CDRDirection
default_callDirection "outgoing" = Just CDR_outgoing
default_callDirection "incoming" = Just CDR_incoming
default_callDirection "internal" = Just CDR_internal
default_callDirection _ = Nothing

instance CDRFormat CSVFormat_AsterisellStandard_V1 where

  getCallDate cdr 
    = let ds = Text.unpack $ f1v1_callDate cdr
      in case fromMySQLDateTimeAsTextToLocalTime (f1v1_callDate cdr) of
           Nothing
             -> Left $ createError
                         Type_Error
                         Domain_RATES
                         ("unknown date format - " ++ ds)
                         ("\"" ++ ds ++ "\" is an unknown call date format.")
                         ("This CDR and CDRs with similar calldate will not be imported.")
                         ("This is a problem in the input format, or in the specification, or in the application code. Contact the assistance.")

           Just v
             -> Right $ Just v

  toCDR precision provider record
    = do 
        let maybeCallDate = fromMySQLDateTimeAsTextToLocalTime $ f1v1_callDate record
        when (isNothing maybeCallDate)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized calldate - " ++ (Text.unpack $ f1v1_callDate record))
                                        ("The calldate field \"" ++ (Text.unpack $ f1v1_callDate record) ++ "\" uses an unrecognized format.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )   
 
 
        let maybeDisposition = default_recognizeAnsweredCall $ f1v1_disposition record
        when (isNothing maybeDisposition)
             (throwError $ createError Type_Error
                                       Domain_RATES
                                       ("unrecognized disposition - " ++ (Text.unpack $ f1v1_disposition record))
                                       ("The disposition field \"" ++ (Text.unpack $ f1v1_disposition record) ++ "\" has an unrecognized value.")
                                       ("The CDRs with this disposition will not be rated.")
                                       ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )    
        
        let maybeDirection = default_callDirection $ f1v1_direction record
        when (isNothing maybeDirection)
             (throwError $ createError Type_Error
                                       Domain_RATES
                                       ("unrecognized direction - " ++ (Text.unpack $ f1v1_direction record))
                                       ("The call direction field \"" ++ (Text.unpack $ f1v1_direction record) ++ "\" has an unrecognized value.")
                                       ("The CDRs with this call direction will not be rated.")
                                       ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )    
       
        let maybePorted
              = if Text.length (f1v1_portedTelephoneNumber record) > 0 then Just (f1v1_portedTelephoneNumber record) else Nothing           
       
        let maybeDuration = fromTextToInt (f1v1_billsec record)
        when (isNothing maybeDuration)
             (throwError $ createError Type_Error
                                       Domain_RATES
                                       ("unrecognized duration - " ++ (show $ f1v1_billsec record))
                                       ("The call duration field \"" ++ (show $ f1v1_billsec record) ++ "\" has an unrecognized value.")
                                       ("The CDRs with this call duration field will not be rated.")
                                       ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )

        maybeExpectedCost
          <- case Text.length (f1v1_expectedCost record) > 0 of
              False -> return Nothing
              True -> case fromTextToRational (f1v1_expectedCost record) of 
                        Just r -> return $ Just $ toRational r
                        Nothing -> throwError $ createError Type_Error
                                                            Domain_RATES
                                                            ("unrecognized expected cost - " ++ (show $ f1v1_expectedCost record))
                                                            ("The call expected cost field \"" ++ (show $ f1v1_expectedCost record) ++ "\" has an unrecognized value.")
                                                            ("The CDRs with this value will not be rated.")
                                                            ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
        
        let cdr =  cdr_empty (fromJust1 "cdra1" maybeCallDate) precision              
        return [cdr {
                cdr_countOfCalls = 1
              , cdr_direction = fromJust1 "cdra2" maybeDirection
              , cdr_errorDirection = CDR_none
              , cdr_isRedirect = if (f1v1_isRedirect record == "1") then True else False
              , cdr_duration = maybeDuration
              , cdr_billsec = maybeDuration
              , cdr_internalTelephoneNumber = f1v1_internalExtension record
              , cdr_externalTelephoneNumber = f1v1_externalTelephoneNumber record
              , cdr_channel = Just $ f1v1_vendorAndChannelDomain record
              , cdr_expectedCost = maybeExpectedCost
              }]

--------------------
-- CUSTOM FORMATS --
--------------------

-- | A CSV format used for importing data from version 3 of Asterisell
data CSVFormat_ImportFromV3_Format1
  = CSVFormat_ImportFromV3_Format1 {
      f2v1_callDate :: !Text.Text  
    , f2v1_src :: !Text.Text 
    , f2v1_dst :: !Text.Text 
    , f2v1_duration :: !Int        
    , f2v1_billsec :: !Int         
    , f2v1_accountcode :: !Text.Text 
    , f2v1_destination_type :: !Int  
    , f2v1_account_id :: !Int 
    , f2v1_income :: !Int  
    , f2v1_vendor_id :: !Int 
    , f2v1_cost :: !Int   
    , f2v1_telephone_prefix_id :: !Int  
    , f2v1_internal_telephone_number :: !Text.Text 
    , f2v1_external_telephone_number :: !Text.Text  
    , f2v1_external_telephone_number_with_number_portability :: !Text.Text 
    , f2v1_masked_external_telephone_number :: !Text.Text  
    , f2v1_source_cost :: !Text.Text
    -- ^ it can be also "NULL" or an Integer value, but I ignore, so I manage it as a simple text field
    , f2v1_asterisk_account_code :: !Text.Text
 } deriving(Show)

instance FromRecord CSVFormat_ImportFromV3_Format1 where
     parseRecord v = 
         let expectedCols = 18
         in case V.length v == expectedCols of
              True
                -> CSVFormat_ImportFromV3_Format1 <$>
                     v .! 0 <*>
                     v .! 1 <*>
                     v .! 2 <*>
                     v .! 3 <*>
                     v .! 4 <*>
                     v .! 5 <*>
                     v .! 6 <*>
                     v .! 7 <*>
                     v .! 8 <*>
                     v .! 9 <*>
                     v .! 10 <*>
                     v .! 11 <*>
                     v .! 12 <*>
                     v .! 13 <*>
                     v .! 14 <*>
                     v .! 15 <*>
                     v .! 16 <*>
                     v .! 17 

              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

-- | Return a standard vendor name to use as comunication channel name, for importing CDRS.
csvFormat_importFromV3_Format1_getVendorId :: Int -> Text.Text
csvFormat_importFromV3_Format1_getVendorId i
  = Text.pack $ "import-from-v3/" ++ show i

instance CDRFormat CSVFormat_ImportFromV3_Format1 where

  getCallDate cdr 
    = let ds = Text.unpack $ f2v1_callDate cdr
      in case fromMySQLDateTimeAsTextToLocalTime (f2v1_callDate cdr) of
           Nothing
             -> Left $ createError
                         Type_Error
                         Domain_RATES
                         ("unknown date format - " ++ ds)
                         ("\"" ++ ds ++ "\" is an unknown call date format.")
                         ("This CDR and CDRs with similar calldate will not be imported.")
                         ("This is a problem in the input format, or in the specification, or in the application code. Contact the assistance.")

           Just v
             -> Right $ Just v

  toCDR precision provider record
    = do 

        let maybeCallDate = fromMySQLDateTimeAsTextToLocalTime $ f2v1_callDate record
        when (isNothing maybeCallDate)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized calldate - " ++ (Text.unpack $ f2v1_callDate record))
                                        ("The calldate field \"" ++ (Text.unpack $ f2v1_callDate record) ++ "\" uses an unrecognized format.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )   
 
 
        let (callDirection, errorDirection) 
              = case f2v1_destination_type record of
                  0 -> (CDR_error, CDR_outgoing)
                  1 -> (CDR_incoming, CDR_none)
                  2 -> (CDR_outgoing, CDR_none)
                  3 -> (CDR_internal, CDR_none)
                  4 -> (CDR_ignored, CDR_none)
                  5 -> (CDR_error, CDR_outgoing)
                  6 -> (CDR_system, CDR_none)
                  _ -> (CDR_error, CDR_error)

        when (errorDirection == CDR_error)
             (throwError $ createError Type_Error
                                       Domain_RATES
                                       ("unrecognized direction - " ++ (show $ f2v1_destination_type record))
                                       ("The call direction field \"" ++ (show $ f2v1_destination_type record) ++ "\" has an unrecognized value.")
                                       ("The CDRs with this call direction will not be rated.")
                                       ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )    
       
        let cost = fromIntegerWithFixedPrecisionToMonetaryValue 4 (f2v1_cost record)
        -- NOTE: use the fixed precision of the source, not of the dest

        let income = fromIntegerWithFixedPrecisionToMonetaryValue 4 (f2v1_income record)
          
        let cdr =  cdr_empty (fromJust1 "cdra3" maybeCallDate) precision              
        return $ [cdr {
                cdr_countOfCalls = 1
              , cdr_direction = callDirection
              , cdr_errorDirection = errorDirection
              , cdr_isRedirect = False
              , cdr_duration = Just $ f2v1_duration record
              , cdr_billsec = Just $ f2v1_billsec record
              , cdr_cost = cost
              , cdr_income = income
              , cdr_externalTelephoneNumber = Text.strip $ f2v1_external_telephone_number record
              , cdr_externalTelephoneNumberWithAppliedPortability = Just $ Text.strip $ f2v1_external_telephone_number_with_number_portability record
              , cdr_displayedExternalTelephoneNumber = Just $ Text.strip $ f2v1_external_telephone_number record
              , cdr_displayedMaskedExternalTelephoneNumber = Just $ Text.strip $ f2v1_masked_external_telephone_number record
              , cdr_internalTelephoneNumber = Text.strip $ f2v1_asterisk_account_code record
              , cdr_expectedCost = Nothing
              , cdr_channel = Just $ Text.strip $ csvFormat_importFromV3_Format1_getVendorId $ f2v1_vendor_id record
              }]

---------------------------
-- MANAGE STREAM OF CDRs --
---------------------------

-- | Identify some locale conversion to apply to the file.
--   Notes about correct usage of locales in Haskell (at best  my knowledge):
-- * the code like @liftIO $ hSetEncoding handle utf8_bom@ is used for instructing Haskell IO functions, to read UTF8 stream of data
-- * the Cassava library convert to Text in case the destination fields are of type Text, and presume that the source ByteString is in UTF8 format
-- * other conversions must be done in explicit way usi the @Data.Text.Encoding@ functions on the ByteString
data LocaleConversion
  = UseUTF8
 deriving(Eq, Show)   

-- | Record that can be read, converted to a CDR, and rated.
--   NOTE: customize the show instance, because it is used for showing CDRs with problems to users.
class (Show a, FromRecord a) => CDRFormat a where
  
  -- | Return only the calldate of the parsed CSV line.
  --   Return Nothing if the CSV line is without a calldate, and it can be safely ignored.
  --   Usually calls to ignore, but with a calldate, must return the calldate. They will be ignored later, in processing,
  --   but it is important associating to a file all the calldates.
  --   Return also the to_calldate in case of service CDRS. If the to_calldate is Nothing, then it is a normal cass, otherwise it is a service CDR (mandatory).
  getCallDate :: a -> Either AsterisellError (Maybe LocalTime)

  -- | Convert the parsed CSV line, to one or more CDRs.
  --   The cdr_isImportedServiceCDR is set directly from the rating engine, in case the CDR is cdr_isServiceCDR, so it is not needed it is set from this method.
  toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> a -> Either AsterisellError [CDR]
 
  -- | A unique key identiyfying the record, to use during generation of error messages,
  --   if the error must be linked to the specific CDR.
  uniqueErrorKey :: a -> String
  uniqueErrorKey x = MD5.md5s (MD5.Str $ show x)

type SourceDataFileHandle = Handle

type LogicalTypeName = BS.ByteString

type FormatTypeName = BS.ByteString

type IsThereHeader = Bool

-- | A human readable description of the imported source CDR.
type SourceCDRDescription = String

-- | A process sending to the specified channel a stream of ar_source_cdr of a specific format.
--   Usually it returns the line of a CSV file, corresponding to a single CDR.
--   Return Nothing for signaling the end of input.
--   @ensure the CSV line content is always converted to UTF8 format.
type SourceCDRImporter
  =  Handle 
  -> BoundedChan (Maybe BSL.ByteString)
  -> IO ()

-- | Import a single CDR from a single ar_source_cdr content.
--   Usually import a CDR from a CSV line content.
--   @require the CSV content is expected to be in UTF8 content. 
type CDRImporter = BSL.ByteString -> CDRProviderName -> ImportedCDR

-- | A CDR converted from a source CDR.
--   @invariant the returned CDR can not have an error field setted.
type ImportedCDR
       = ( Maybe LocalTime
           -- ^ the calldate of the CDR (if parseable), used also in case of errors
         , Either AsterisellError [(CDR, SourceCDRDescription)]
           -- ^ the error, or the CDR result of the conversion. The imported CDR can contain an optional high-level description of its content,
           --   used only in case of debug in next rating phases.
           --   @require does not contains a description of the CDR, because it will be added from the framework   
         )

-- | Params identifying the file containing the source CDRs.
--   They are used both during importing, and during exporting of CDRs.
data SourceCDRParams
       = SourceCDRParamsCSVFile
           (Maybe Text.Text) -- ^ the header (if presents)
           Char -- ^ the field separator character
           LocaleConversion -- ^ the encoding of the source file. Internally it is saved into UTF8 encoding

sourceCDRParams_default :: SourceCDRParams
sourceCDRParams_default
  = SourceCDRParamsCSVFile Nothing ',' UseUTF8 

-- | Associations between logical and physical formats, and the corresponding rating function.
type ConfiguredCDRSImporters 
  = Map.HashMap (LogicalTypeName, FormatTypeName) (SourceCDRParams, SourceCDRImporter, CDRImporter)

configuredCDRSImporters_get :: ConfiguredCDRSImporters -> LogicalTypeName -> FormatTypeName -> Maybe (SourceCDRParams, SourceCDRImporter, CDRImporter)
configuredCDRSImporters_get conf1 l f = Map.lookup (l, f) conf1

----------------------------------
-- EXPORT A CDR TO A CSV RECORD --
----------------------------------

-- | A value that can be null.
--
data ExportMaybeNull a
  = ExportNull
  | Export a

type ImportMaybeNull a = ExportMaybeNull a

fromExport :: ExportMaybeNull a -> a
fromExport (Export x) = x
fromExport _ = error "unexpected ExportNull"

fromExportOrEmptyString :: ExportMaybeNull Text.Text -> Text.Text
fromExportOrEmptyString (Export x) = x
fromExportOrEmptyString (ExportNull) = Text.empty

fromExportOrNothing :: (a -> b) -> ExportMaybeNull a -> Maybe b
fromExportOrNothing _ ExportNull = Nothing
fromExportOrNothing f (Export a) = Just $ f a 

toMaybe :: ExportMaybeNull a -> Maybe a
toMaybe ExportNull = Nothing
toMaybe (Export a) = Just a

isNullExport :: ExportMaybeNull a -> Bool
isNullExport mx = case mx of
              ExportNull -> True
              _ -> False

isNotNullExportWith :: ExportMaybeNull a -> (a -> Bool) -> Bool
isNotNullExportWith mx f =
  case mx of
    ExportNull -> False
    Export x -> f x

isEmptyOrNull :: ExportMaybeNull Text.Text -> Bool
isEmptyOrNull mt
 = case mt of
      ExportNull -> True
      Export t -> Text.null t

isNotEmptyOrNull :: ExportMaybeNull Text.Text -> Bool
isNotEmptyOrNull mt = not $ isEmptyOrNull mt

instance (Show a) => Show (ExportMaybeNull a) where
  show (ExportNull) = "ExportNull"
  show (Export a) = "Export " ++ show a

instance (Eq a) => Eq (ExportMaybeNull a) where
  (==) x y = case (x,y) of
               (ExportNull, ExportNull)
                 -> True
               (Export x1, Export y1)
                 -> x1 == y1
               _ -> False

instance (ToField a) => ToField (ExportMaybeNull a) where
  toField (ExportNull) = "\\N"
  toField (Export x) = toField x

instance (FromField a) => FromField (ExportMaybeNull a) where
  parseField s1 = do
      s2 :: Text.Text <- parseField s1
      -- NOTE: force UTF8 conversion
      case s2 == Text.pack "\\N" of
        True -> return ExportNull
        False -> do s3 :: a <- parseField s1
                    -- NOTE: parse using the proper type conversion
                    return $ Export s3

toMaybeField :: (ToField a) => Maybe a -> ExportMaybeNull a
toMaybeField Nothing = ExportNull
toMaybeField (Just x) = Export x 

toMySQLBoolField :: Bool -> String
toMySQLBoolField True = "1"
toMySQLBoolField False = "0"

-- | Import from an Integer with fixed precision digits, to a Monetary value.
fromIntegerWithFixedPrecisionToMonetaryValue :: CurrencyPrecisionDigits -> Int -> MonetaryValue
fromIntegerWithFixedPrecisionToMonetaryValue precision value
  = let scale :: MonetaryValue = (toRational 0.1) ^ precision
        value' = toRational value
    in  value' * scale

-- | Export a MonetaryValue to an Integer with the right digits corresponding to the fixed precision digits.
toMonetaryValueWithFixedPrecision :: CurrencyPrecisionDigits -> MonetaryValue -> String
toMonetaryValueWithFixedPrecision precision rationalValue
  = show $ toMonetaryValueWithFixedPrecisionInt precision rationalValue

toMonetaryValueWithFixedPrecisionInt :: CurrencyPrecisionDigits -> MonetaryValue -> Int
toMonetaryValueWithFixedPrecisionInt precision rationalValue
  = let scale = toRational $ 10 ^ precision
        scaledValue = rationalValue * scale
    in  fromIntegral $ mathRound scaledValue


-- | Export a MonetaryValue to an Integer with the right digits corresponding to the fixed precision digits.
--
toMaybeMonetaryValueWithFixedPrecision :: CurrencyPrecisionDigits -> Maybe MonetaryValue -> String
toMaybeMonetaryValueWithFixedPrecision precision maybeRationalValue
  = case maybeRationalValue of
      Just rationalValue
        -> toMonetaryValueWithFixedPrecision precision rationalValue
      Nothing
        -> "\\N"
 
instance Csv.ToRecord CDR where
  toRecord cdr
    = let precision = cdr_precision cdr
      in V.fromList [
              toField $ showLocalTimeUsingMySQLFormat $ cdr_calldate cdr
            , toField (case cdr_toCalldate cdr of
                         Nothing
                           -> "\\N"
                         Just d
                           -> showLocalTimeUsingMySQLFormat d)
            , toField $ toMySQLBoolField $ cdr_isImportedServiceCDR cdr  
            , toField $ cdr_countOfCalls cdr
            , toField $ cdrDirection_asterisellCode $ cdr_direction cdr
            , toField $ toMySQLBoolField $ cdr_isRedirect cdr
            , toField $ cdr_duration cdr
            , toField $ toMaybeField $ cdr_billsec cdr
            , toField $ toMaybeField $ cdr_organizationUnitId cdr
            , toField $ toMaybeField $ maybeCachedParentIdHierarchy_toMaybeText $  cdr_cachedParentIdHierarchy cdr
            , toField $ toMaybeField $ cdr_billableOrganizationUnitId cdr
            , toField $ toMaybeField $ cdr_bundleOrganizationUnitId cdr
            , toField $ toMonetaryValueWithFixedPrecision precision (cdr_income cdr)
            , toField $ toMonetaryValueWithFixedPrecision precision (cdr_costSaving cdr)
            , toField $ toMaybeField $ cdr_vendorId cdr
            , toField $ toMaybeField $ cdr_communicationChannelTypeId cdr
            , toField $ toMonetaryValueWithFixedPrecision precision (cdr_cost cdr)
            , toField $ (case cdr_expectedCost cdr of
                           Nothing
                             -> "\\N"
                           Just v
                             -> toMonetaryValueWithFixedPrecision precision v)
            , toField $ toMaybeField $ cdr_telephonePrefixId cdr
            , toField $ toMaybeField $ cdr_displayedExternalTelephoneNumber cdr
            , toField $ toMaybeField $ cdr_externalTelephoneNumberWithAppliedPortability cdr
            , toField $ toMaybeField $ cdr_displayedMaskedExternalTelephoneNumber cdr
            , toField $ cdrDirection_asterisellCode $  cdr_errorDirection cdr
            , toField $ toMaybeField $ cdr_problemDuplicationKey cdr
            , toField $ toMaybeField $ cdr_debug_cost_rate cdr 
            , toField $ toMaybeField $ cdr_debug_income_rate cdr
            , toField $ toMaybeField $ cdr_debug_residual_income_rate cdr
            , toField $ toMaybeField $ cdr_debug_residual_call_duration cdr
            , toField $ toMaybeField $ cdr_debug_bundle_left_calls cdr
            , toField $ toMaybeField $ cdr_debug_bundle_left_duration cdr
            , toField $ toMaybeMonetaryValueWithFixedPrecision precision (cdr_debug_bundle_left_cost cdr)
      ]

cdr_toCSVLine :: CDR -> BSL.ByteString
cdr_toCSVLine cdr = Csv.encode [cdr]

cdr_showDebug :: CDR ->  String
cdr_showDebug cdr 
  = ""
    ++ addLine "calldate" (showLocalTimeUsingMySQLFormat $ cdr_calldate cdr)
    ++ addLine "to calldate" (case cdr_toCalldate cdr of
                               Nothing
                                 -> nullStr
                               Just d
                                 -> showLocalTimeUsingMySQLFormat d
                            )
    ++ addLine "is imported service CDR" (show $ cdr_isImportedServiceCDR cdr)
    ++ addLine "count of calls" (show $ cdr_countOfCalls cdr)
    ++ addLine "direction" (show $ cdr_direction cdr)
    ++ addLine "is redirect" (show $ cdr_isRedirect cdr)
    ++ addLine "error direction" (show $ cdr_errorDirection cdr)
    ++ addLine "duration" (show $ cdr_duration cdr)
    ++ addLine "billsec" (show $ cdr_billsec cdr)
    ++ addLine "internal telephoneNumber" (Text.unpack $ cdr_internalTelephoneNumber cdr)
    ++ addLine "external telephoneNumber" (Text.unpack $ cdr_externalTelephoneNumber cdr)
    ++ addLine "external telephoneNumber with applied portability" (show $ cdr_externalTelephoneNumberWithAppliedPortability cdr)
    ++ addLine "displayed external telephone number" (show $ cdr_displayedExternalTelephoneNumber cdr)
    ++ addLine "displayed masked external telephone number" (show $ cdr_displayedMaskedExternalTelephoneNumber cdr)
    ++ addLine "communication channel" (maybeUnpack $ cdr_channel cdr)
    
 where

  nullStr
    = "<NULL>"

  addLine :: String -> String -> String
  addLine l v 
    = "\n   " ++ l ++ ": " ++ v
  
  addLineOrNull l Nothing
    = addLine l nullStr

  addLineOrNull l (Just v)
    = addLine l v
    
  maybeUnpack Nothing
    = nullStr
  
  maybeUnpack (Just t)
    = Text.unpack t 

--
-- Common Formats
--

-- | Info received from another Asterisell instance acting like a VoIP CDR provider.
--   It contains only normal calls, and not service-cdrs.  
data CSVFormat_asterisell_provider__v1
  = CSVFormat_asterisell_provider__v1 {
            asterisell_provider__v1__callDate :: !Text.Text
           ,asterisell_provider__v1__toCallDate :: !(ExportMaybeNull Text.Text)
           ,asterisell_provider__v1__countOfCalls :: !Int
           ,asterisell_provider__v1__destinationType :: !Int
           ,asterisell_provider__v1__isRedirect :: !Int
           ,asterisell_provider__v1__duration :: !Int
           ,asterisell_provider__v1__billsec :: !Int
           ,asterisell_provider__v1__income :: !Int
           ,asterisell_provider__v1__account :: !(ExportMaybeNull Text.Text)
           ,asterisell_provider__v1__externalTelephoneNumber :: !Text.Text
           ,asterisell_provider__v1__portedExternalTelephoneNumber :: !Text.Text
           ,asterisell_provider__v1__communicationChannel :: !Text.Text 
           }

instance Show CSVFormat_asterisell_provider__v1 where
  show cdr
    = showLines
        [("calldate", show $ asterisell_provider__v1__callDate cdr)
        ,("to calldate", show $ asterisell_provider__v1__toCallDate cdr)
        ,("count of calls", show $ asterisell_provider__v1__countOfCalls cdr)
        ,("destination type", show $ asterisell_provider__v1__destinationType cdr)
        ,("is redirect", show $ asterisell_provider__v1__isRedirect cdr)
        ,("duration", show $ asterisell_provider__v1__duration cdr)
        ,("billsec", show $ asterisell_provider__v1__billsec cdr)
        ,("expected income", show $ asterisell_provider__v1__income cdr)
        ,("account", show $ asterisell_provider__v1__account cdr)
        ,("external telephone number", Text.unpack $ asterisell_provider__v1__externalTelephoneNumber cdr)
        ,("ported telephone number", Text.unpack $ asterisell_provider__v1__portedExternalTelephoneNumber cdr)
        ,("communication cannel", Text.unpack $ asterisell_provider__v1__communicationChannel cdr) 
        ]

   where

     showLines ls = List.concatMap showLine ls

     showLine :: (String, String) -> String
     showLine (h, v) = h ++ ": " ++ v ++ "\n"
     
instance Csv.FromRecord CSVFormat_asterisell_provider__v1 where
     parseRecord v =
         let expectedCols = 12
         in case V.length v == expectedCols of
              True
                -> CSVFormat_asterisell_provider__v1 <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6<*>
                     v .! 7<*>
                     v .! 8<*>
                     v .! 9<*>
                     v .! 10<*>
                     v .! 11
                                          
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_asterisell_provider__v1 where

  getCallDate cdr 
    = let ds = Text.unpack $ asterisell_provider__v1__callDate cdr
      in case fromMySQLDateTimeAsTextToLocalTime (asterisell_provider__v1__callDate cdr) of
           Nothing
             -> Left $ createError
                         Type_Error
                         Domain_RATES
                         ("unknown date format - " ++ ds)
                         ("\"" ++ ds ++ "\" is an unexpected call date format.")
                         ("This CDR and CDRs with similar calldate will not be imported.")
                         ("This is a problem in the input format, or in the specification, or in the application code. Contact the assistance.")

           Just v
             -> Right $ Just v

  toCDR precision provider record = convert_CSVFormat_asterisell_provider__v1__toCDR precision provider record

-- | Import a CDR from an Asterisell provider.
convert_CSVFormat_asterisell_provider__v1__toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> CSVFormat_asterisell_provider__v1 -> Either AsterisellError [CDR]
convert_CSVFormat_asterisell_provider__v1__toCDR precision provider record = do
  let callDateS = Text.unpack $ asterisell_provider__v1__callDate record
  let maybeCallDate = fromMySQLDateTimeAsTextToLocalTime $ asterisell_provider__v1__callDate record
      
  when (isNothing maybeCallDate)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized calldate - " ++ callDateS)
                                        ("The calldate field \"" ++ callDateS ++ "\" has unexpected format.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )

  toCallDate
    <- case asterisell_provider__v1__toCallDate record of
         ExportNull
           -> return Nothing
         Export d
           -> case fromMySQLDateTimeAsTextToLocalTime d of
                Nothing
                  -> throwError $ createError  Type_Error
                                               Domain_RATES
                                               ("unrecognized calldate - " ++ Text.unpack d)
                                               ("The calldate field \"" ++ Text.unpack d  ++ "\" has unexpected format.")
                                               ("These CDRs will be not rated.")
                                               ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
                Just r
                  -> return $ Just r


  let expectedCost = fromIntegerWithFixedPrecisionToMonetaryValue precision (asterisell_provider__v1__income record)
  when (isNullExport $ asterisell_provider__v1__account record)
             (throwError $ createError  Type_Error
                                        Domain_VOIP_ACCOUNTS
                                        ("empty exported accountcode")
                                        ("This CDR has a NULL (not specified) internal VoIP account code.")
                                        ("The CDRs of this VoIP account code will be not rated.")
                                        ("The Asterisell VoIP provider has not configured some exported/reselled internal VoIP account codes. Contact the provider, and tell him to complete the VoIP account codes.")
             )

  let providerT = provider

  let providerTT = Text.append providerT (Text.pack "-")     
      
  let cdr =  cdr_empty (fromJust1 "cdra4" maybeCallDate) precision
  
  return $ [cdr {
                cdr_countOfCalls = asterisell_provider__v1__countOfCalls record
              , cdr_toCalldate = toCallDate                     
              , cdr_direction = (cdrDirection_fromAsterisellCode $ asterisell_provider__v1__destinationType record) 
              , cdr_errorDirection = CDR_none
              , cdr_isRedirect = if ((asterisell_provider__v1__isRedirect record) == 1) then True else False
              , cdr_duration = Just $ asterisell_provider__v1__duration record
              , cdr_billsec = Just $ asterisell_provider__v1__billsec record
              , cdr_externalTelephoneNumber = asterisell_provider__v1__externalTelephoneNumber record
              , cdr_displayedMaskedExternalTelephoneNumber
                  = case toCallDate of
                      Nothing
                        -> Nothing
                      Just _
                        -> Just $ asterisell_provider__v1__externalTelephoneNumber record
                           -- in case of services, the specified telephone numeber is already the telephone number to display
                           
              , cdr_externalTelephoneNumberWithAppliedPortability = Just $ asterisell_provider__v1__portedExternalTelephoneNumber record
              , cdr_internalTelephoneNumber = fromExport $ asterisell_provider__v1__account record
              , cdr_expectedCost = Just expectedCost
              , cdr_channel = let sourceChannel = asterisell_provider__v1__communicationChannel record
                              in case Text.null sourceChannel of
                                True
                                  -> Just $ providerT
                                False
                                  -> Just $ Text.concat [providerTT, sourceChannel] 
              }]


-- | Info received from another Asterisell instance acting like a VoIP CDR provider.
--   It contains only imported service calls, and not normal calls.
data CSVFormat_asterisell_provider_services__v1
  = CSVFormat_asterisell_provider_services__v1 CSVFormat_asterisell_provider__v1

instance Show CSVFormat_asterisell_provider_services__v1 where
  show (CSVFormat_asterisell_provider_services__v1 cdr) = show cdr
     
instance Csv.FromRecord CSVFormat_asterisell_provider_services__v1 where
     parseRecord v
       = do cdr :: CSVFormat_asterisell_provider__v1 <- parseRecord v
            return $ CSVFormat_asterisell_provider_services__v1 cdr

instance CDRFormat CSVFormat_asterisell_provider_services__v1 where

  getCallDate (CSVFormat_asterisell_provider_services__v1 cdr) 
    = getCallDate cdr
      
  toCDR precision provider (CSVFormat_asterisell_provider_services__v1 record)
    = toCDR precision provider record

--
-- Utility Functions
--

nullStr :: String
nullStr = "<NULL>"

addLine :: String -> String -> String
addLine l v
       = "\n   " ++ l ++ ": " ++ v

cdrField cdr f
       = case f cdr of
           (ExportNull) -> nullStr
           (Export s) -> Text.unpack s

fieldI cdr f
  = case f cdr of
      (ExportNull) -> nullStr
      (Export i) -> show i

--
-- Common Function To Call
--

importNotNullText
  :: ExportMaybeNull Text.Text
  -> String
  -- ^ field name
  -> String
  -- ^ field content type
  -> Either AsterisellError Text.Text
importNotNullText maybeV fieldName fieldType
  = case maybeV of
      ExportNull
        -> Left $ createError  Type_Error
                        Domain_RATES
                        ("not accepted NULL " ++ fieldType)
                        ("The field " ++ fieldName ++ ", containing " ++ fieldType ++ ", has an unexpected NULL/empty value.")
                        ("These CDRs will be not rated.")
                        ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
      Export v
        -> Right v

importAndConvertNotNullValue
  :: ExportMaybeNull Text.Text
  -> (Text.Text -> Maybe a)
  -- ^ conversion function
  -> String
  -- ^ field name
  -> String
  -- ^ field content type
  -> Either AsterisellError a
importAndConvertNotNullValue maybeV convF fieldName fieldType
  = do textValue <- importNotNullText maybeV fieldName fieldType
       let maybeValue =  convF textValue
       case maybeValue  of
         Nothing
           -> throwError $ createError
                             Type_Error
                             Domain_RATES
                             ("unrecognized " ++ fieldType ++ " - " ++ (Text.unpack textValue))
                             ("The field " ++ fieldName ++ ", containing " ++ fieldType ++ ", has the unrecognized content \"" ++ (Text.unpack textValue) ++ "\"")
                             ("These CDRs will be not rated.")
                             ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
         Just value
           -> return value

importAndConvertNotNullValue2
  :: ExportMaybeNull Text.Text
  -- ^ date
  -> ExportMaybeNull Text.Text
  -- ^ time-stamp
  -> (Text.Text -> Text.Text -> Maybe a)
  -- ^ conversion function for date and time stamp
  -> String
  -- ^ call date field name
  -> String
  -- ^ time stamp field name
  -> String
  -- ^ field content type
  -> Either AsterisellError a
importAndConvertNotNullValue2 maybeV1 maybeV2 convF fieldName1 fieldName2 fieldType
  = do textValue1 <- importNotNullText maybeV1 fieldName1 fieldType
       textValue2 <- importNotNullText maybeV2 fieldName2 fieldType
       let maybeValue =  convF textValue1 textValue2
       case maybeValue  of
         Nothing
           -> throwError $ createError
                             Type_Error
                             Domain_RATES
                             ("unrecognized " ++ fieldType ++ " - " ++ (Text.unpack textValue1))
                             ("The fields " ++ fieldName1 ++ ", and " ++ fieldName2 ++ " containing " ++ fieldType ++ ", has the unrecognized content \"" ++ (Text.unpack textValue1) ++ "\", \"" ++ (Text.unpack textValue2) ++ "\"")
                             ("These CDRs will be not rated.")
                             ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
         Just value
           -> return value


