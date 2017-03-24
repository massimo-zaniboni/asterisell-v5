{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings #-}

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


-- | Import specific format for a Customer.
module Asterisell.CustomerSpecificImporters (
    CSVFormat_twt_cps__v1
  , CSVFormat_twt_nng__v1
  , CSVFormat_freeRadius__v1
  , CSVFormat_gamma__v1
  , CSVFormat_gamma_ItemRental__v1
  , CSVFormat_asterisk__generic
  , CSVFormat_asterisk__a_p_v1
  , CSVFormat_plain1
  , CSVFormat_digitel
  , CSVFormat_digitelNNG__v1
  , CSVFormat_colt
  , CSVFormat_colt43
  , const_digitelHeader
  , const_digitelNNGTimeBandPrefix
  , digitel_normalizeCalledNumber
  , gamma_ItemRental_channelName
  , gamma_ItemRental_rental
  , gamma_ItemRental_connection
  , tt_customerSpecificImporters
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

import Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad
import Control.Applicative
import Data.Vector as V hiding((++))
import Data.Hashable
import Data.Maybe
import Control.Monad.Except
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Csv as CSV
import qualified Data.Csv.Streaming as CSV
import qualified Test.HUnit as HUnit

-- | TWT operator CPS format.
--   A line is something like
--   > 11336;8470764;2014/11/05 08.28.54;0376667XXX;003934710XXX;39347;Italy Mobile Vodafone;126;0,0546;0;0;0;39347
--   > 11336;8470764;2014/11/05 08.37.06;0376667XXX;003933837XXX;39338;Italy Mobile TIM;74;0,0321;0;0;0;39338
data CSVFormat_twt_cps__v1
  = CSVFormat_twt_cps__v1 {
            twt_cps__v1__0 :: !Text.Text
          , twt_cps__v1__1 :: !Text.Text
          , twt_cps__v1__2_callDate :: !Text.Text
          , twt_cps__v1__3_caller :: !Text.Text
          , twt_cps__v1__4_calledNr :: !Text.Text
          , twt_cps__v1__5_originalPrefix :: !Text.Text
          , twt_cps__v1__6_operator :: !Text.Text
          , twt_cps__v1__7_duration :: !Text.Text
          , twt_cps__v1__8_cost :: !Text.Text
          , twt_cps__v1__9 :: !Text.Text
          , twt_cps__v1__10 :: !Text.Text
          , twt_cps__v1__12 :: !Text.Text
          , twt_cps__v1__11_portedPrefix :: !Text.Text
  }

instance Show CSVFormat_twt_cps__v1 where
  show cdr
    = showLines
        [("field 1", twt_cps__v1__0)
        ,("field 2", twt_cps__v1__1)
        ,("call date", twt_cps__v1__2_callDate)
        ,("caller", twt_cps__v1__3_caller)
        ,("called", twt_cps__v1__4_calledNr)
        ,("original called prefix", twt_cps__v1__5_originalPrefix)
        ,("operator", twt_cps__v1__6_operator)
        ,("duration", twt_cps__v1__7_duration)
        ,("cost", twt_cps__v1__8_cost)
        ,("field 10", twt_cps__v1__9)
        ,("field 11", twt_cps__v1__10)
        ,("field 12", twt_cps__v1__12)
        ,("ported called prefix", twt_cps__v1__11_portedPrefix)
        ]

   where

     showLines ls = List.concatMap showLine ls

     showLine (h, v) = h ++ ": " ++ (Text.unpack $ v cdr) ++ "\n"

instance CSV.FromRecord CSVFormat_twt_cps__v1 where
     parseRecord v =
         let expectedCols = 13
         in case V.length v == expectedCols of
              True
                -> CSVFormat_twt_cps__v1 <$>
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
                     v .! 11<*>
                     v .! 12

              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_twt_cps__v1 where

  getCallDate cdr
    = let ds = Text.unpack $ twt_cps__v1__2_callDate cdr
      in case fromDateFormat1ToLocalTime (twt_cps__v1__2_callDate cdr) of
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

  toCDR precision provider record = convert_CSVFormat_twt_cps__v1__toCDR precision provider record

convert_CSVFormat_twt_cps__v1__toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> CSVFormat_twt_cps__v1 -> Either AsterisellError [CDR]
convert_CSVFormat_twt_cps__v1__toCDR precision provider record = do
  let callDateS = Text.unpack $ twt_cps__v1__2_callDate record
  let maybeCallDate = fromDateFormat1ToLocalTime (twt_cps__v1__2_callDate record)
  when (isNothing maybeCallDate)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized calldate - " ++ callDateS)
                                        ("The calldate field \"" ++ callDateS ++ "\" has unexpected format.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )

  let costS = Text.map (\c -> if c == ',' then '.' else c) (twt_cps__v1__8_cost record)
  let maybeCost = fromTextToRational costS
  when (isNothing maybeCost)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized cost - " ++ (Text.unpack costS))
                                        ("The expected cost field \"" ++ (Text.unpack costS) ++ "\" is not a valid number.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
             )

  let maybeDuration =  fromTextToInt $ twt_cps__v1__7_duration record
  when (isNothing maybeDuration)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized duration - " ++ (Text.unpack $ twt_cps__v1__7_duration record))
                                        ("The call duration \"" ++ (Text.unpack $ twt_cps__v1__7_duration record) ++ "\" is not a valid number.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
             )

  let calledNr1 = twt_cps__v1__4_calledNr record
  let calledNr = case Text.isPrefixOf "00" calledNr1 of
                   True -> Text.drop 2 calledNr1
                   False -> calledNr1

  let maybePortedTelephoneNumber = replacePrefixAndGetPortedTelephoneNumber (Text.unpack $ calledNr) (Text.unpack $ twt_cps__v1__5_originalPrefix record) (Text.unpack $ twt_cps__v1__11_portedPrefix record)
  when (isNothing maybePortedTelephoneNumber)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized ported telephone number - " ++ (Text.unpack $ twt_cps__v1__4_calledNr record))
                                        ("The called telephone number \"" ++ (Text.unpack $ twt_cps__v1__4_calledNr record) ++ "\" can not have ported/real telephone prefix \"" ++ (Text.unpack $ twt_cps__v1__11_portedPrefix record) ++ "\".")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
             )

  let portedTelephoneNumber = Text.pack $ fromJust1 "csi2" maybePortedTelephoneNumber

  let cdr =  cdr_empty (fromJust1 "csi3" maybeCallDate) precision
  return $ [cdr {
                cdr_countOfCalls = 1
              , cdr_direction = CDR_outgoing
              , cdr_errorDirection = CDR_none
              , cdr_isRedirect = False
              , cdr_duration = maybeDuration
              , cdr_billsec = maybeDuration
              , cdr_externalTelephoneNumber = calledNr
              , cdr_externalTelephoneNumberWithAppliedPortability = Just portedTelephoneNumber
              , cdr_internalTelephoneNumber = twt_cps__v1__3_caller record
              , cdr_expectedCost = maybeCost
              , cdr_channel = Just $ provider
              }]

-- | TWT operator NNG format, used for Free Toll Numbers (numeri verdi).
--   A line is something like
-- > 11592;8673345;2014/11/12 10.32.29;800035XXX;0521710XXX;003980003XXX;39800;Italy NS Servizi TollFree;13;0,0000;0,0026;3900603;Italy OLO Wind urbano;39800;Italy NS Servizi TollFree
-- > 11592;8673345;2014/11/12 10.32.52;800035XXX;0521710XXX;003980003XXX;39800;Italy NS Servizi TollFree;159;0,0000;0,0318;3900603;Italy OLO Wind urbano;39800;Italy NS Servizi TollFree
data CSVFormat_twt_nng__v1
  = CSVFormat_twt_nng__v1 {
            twt_nng__v1__0 :: !Text.Text  -- 0 11592;
          , twt_nng__v1__1 :: !Text.Text  -- 1 8673345;
          , twt_nng__v1__callDate :: !Text.Text  -- 2 2014/11/12 17.14.28;
          , twt_nng__v1__calledNr :: !Text.Text  -- 3 800035427;
          , twt_nng__v1__caller :: !Text.Text  -- 4 0598750172;
          , twt_nng__v1__5 :: !Text.Text -- 5 ? 0039800035427;
          , twt_nng__v1__6 :: !Text.Text  -- 6 ? 39800;
          , twt_nng__v1__7 :: !Text.Text -- 7 ? Italy NS Servizi TollFree;
          , twt_nng__v1__duration :: !Text.Text -- 8 121;
          , twt_nng__v1__9 :: !Text.Text        -- 9 ? 0,0000;
          , twt_nng__v1__cost :: !Text.Text  -- 10 0,0242;
          , twt_nng__v1__11 :: !Text.Text -- 11 ? 3900603;
          , twt_nng__v1__operator :: !Text.Text -- 12 Italy OLO Wind urbano;
          , twt_nng__v1__13 :: !Text.Text  -- 13 39800;
          , twt_nng__v1__14 :: !Text.Text -- 14 ? Italy NS
          }

instance Show CSVFormat_twt_nng__v1 where
  show cdr
    = showLines
        [("field 1", twt_nng__v1__0)
        ,("field 2", twt_nng__v1__1)
        ,("field 3", twt_nng__v1__callDate)
        ,("called nr", twt_nng__v1__calledNr)
        ,("caller", twt_nng__v1__caller)
        ,("field 6", twt_nng__v1__5)
        ,("field 7", twt_nng__v1__6)
        ,("field 8", twt_nng__v1__7)
        ,("duration", twt_nng__v1__duration)
        ,("field 10", twt_nng__v1__9)
        ,("cost", twt_nng__v1__cost)
        ,("field 12", twt_nng__v1__11)
        ,("operator", twt_nng__v1__operator)
        ,("field 14", twt_nng__v1__13)
        ,("field 15",twt_nng__v1__14)
        ]

   where

     showLines ls = List.concatMap showLine ls

     showLine (h, v) = h ++ ": " ++ (Text.unpack $ v cdr) ++ "\n"

instance CSV.FromRecord CSVFormat_twt_nng__v1 where
     parseRecord v =
         let expectedCols = 15
         in case V.length v == expectedCols of
              True
                -> CSVFormat_twt_nng__v1 <$>
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
                     v .! 11<*>
                     v .! 12<*>
                     v .! 13<*>
                     v .! 14


              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_twt_nng__v1 where

  getCallDate cdr
    = let ds = Text.unpack $ twt_nng__v1__callDate cdr
      in case fromDateFormat1ToLocalTime (twt_nng__v1__callDate cdr) of
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

  toCDR precision provider record = convert_CSVFormat_twt_nng__v1__toCDR precision provider record

convert_CSVFormat_twt_nng__v1__toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> CSVFormat_twt_nng__v1 -> Either AsterisellError [CDR]
convert_CSVFormat_twt_nng__v1__toCDR precision provider record = do
  let callDateS = Text.unpack $ twt_nng__v1__callDate record
  let maybeCallDate = fromDateFormat1ToLocalTime (twt_nng__v1__callDate record)
  when (isNothing maybeCallDate)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized calldate - " ++ callDateS)
                                        ("The calldate field \"" ++ callDateS ++ "\" has unexpected format.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the code importing CDRs, or in the configuration of VoIP servers. Contact the assistance.")
             )

  let costS = Text.map (\c -> if c == ',' then '.' else c) (twt_nng__v1__cost record)
  let maybeCost = fromTextToRational costS
  when (isNothing maybeCost)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized cost - " ++ (Text.unpack costS))
                                        ("The expected cost field \"" ++ (Text.unpack costS) ++ "\" is not a valid number.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
             )

  let maybeDuration =  fromTextToInt $ twt_nng__v1__duration record
  when (isNothing maybeDuration)
             (throwError $ createError  Type_Error
                                        Domain_RATES
                                        ("unrecognized duration - " ++ (Text.unpack $ twt_nng__v1__duration record))
                                        ("The call duration \"" ++ (Text.unpack $ twt_nng__v1__duration record) ++ "\" is not a valid number.")
                                        ("These CDRs will be not rated.")
                                        ("This is probably an error in the imported CSV file content. If there are many errors of this type, contact the assistance, or the VoIP provider.")
             )

  let externalTelephoneNumberForRating = Text.concat ["incoming-toll-free-", "TollFree - ", twt_nng__v1__operator record, Text.pack "-", twt_nng__v1__caller record]

  let cdr =  cdr_empty (fromJust1 "csi4" maybeCallDate) precision

  -- Consider them like outgoing calls, so invert the two num
  -- The green numbers is the account code, because it is the known telephone that pays the call.
  return $ [cdr {
                cdr_countOfCalls = 1
              , cdr_direction = CDR_outgoing
              , cdr_errorDirection = CDR_none
              , cdr_isRedirect = False
              , cdr_duration = maybeDuration
              , cdr_billsec = maybeDuration
              , cdr_externalTelephoneNumber = twt_nng__v1__caller record
              , cdr_externalTelephoneNumberWithAppliedPortability = Just externalTelephoneNumberForRating
              , cdr_displayedExternalTelephoneNumber = Just $ twt_nng__v1__caller record
              , cdr_internalTelephoneNumber = twt_nng__v1__calledNr record
              , cdr_expectedCost = maybeCost
              , cdr_channel = Just provider
              }]

--
-- Support Free Radius according notes on #1446
--

-- | FreeRadius format
data CSVFormat_freeRadius__v1
  = CSVFormat_freeRadius__v1 {
       freeRadius__v1_id :: !Text.Text
      ,freeRadius__v1_Unique_Id :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_Calling_Station_Id :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_Called_Station_Id :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_h323_setup_time :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_Acct_Session_Time :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_in_intrfc_desc :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_out_intrfc_desc :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_h323_remote_address_in :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_h323_remote_address_out :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_rerouted :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_h323_disconnect_cause :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_h323_gateway_id :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_h323_conf_id :: !(ExportMaybeNull Text.Text)
      ,freeRadius__v1_Acct_Session_Id :: !(ExportMaybeNull Text.Text)
  } deriving(Show)

instance CSV.FromRecord CSVFormat_freeRadius__v1 where
     parseRecord v =
         let expectedCols = 15
         in case V.length v == expectedCols of
              True
                -> CSVFormat_freeRadius__v1 <$>
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
                     v .! 11<*>
                     v .! 12<*>
                     v .! 13<*>
                     v .! 14


              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_freeRadius__v1 where
  getCallDate record
    = do d <- importAndConvertNotNullValue (freeRadius__v1_h323_setup_time record) fromDateFormat1ToLocalTime "h323_setup_time" "call date"
         return $ Just d

  toCDR precision provider record = convert_CSVFormat_freeRadius__v1__toCDR precision provider record

convert_CSVFormat_freeRadius__v1__toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> CSVFormat_freeRadius__v1 -> Either AsterisellError [CDR]
convert_CSVFormat_freeRadius__v1__toCDR precision provider record = do

  callDate <- importAndConvertNotNullValue (freeRadius__v1_h323_setup_time record) fromDateFormat1ToLocalTime "h323_setup_time" "call date"
  duration <- importAndConvertNotNullValue (freeRadius__v1_Acct_Session_Time record) fromTextToInt  "Acct_Session_Time" "duration"
  let supplier = freeRadius__v1_out_intrfc_desc record
  let isNullSupplier
        = case supplier of
            ExportNull -> True
            Export s -> Text.null s

  case (duration == 0 || isNullSupplier) of
    True
      -> -- according #1446 this calls can be ignored
         return $ [(cdr_empty callDate precision) {
                     cdr_countOfCalls = 1
                   , cdr_direction = CDR_ignored
                   , cdr_errorDirection = CDR_none
                   }]

    False
      -> do v1 <- importNotNullText (freeRadius__v1_Called_Station_Id record) "Called_Station_Id" "called number"
            v3 <- importNotNullText (freeRadius__v1_in_intrfc_desc record) "in_intrfc_desc" "customer id"
            let cdr =  cdr_empty callDate precision
            return $ [cdr { cdr_countOfCalls = 1
                          , cdr_direction = CDR_outgoing
                          , cdr_errorDirection = CDR_none
                          , cdr_isRedirect = False
                          , cdr_duration = Just duration
                          , cdr_billsec = Just duration
                          , cdr_externalTelephoneNumber = v1
                          , cdr_internalTelephoneNumber = v3
                          , cdr_channel = fromExportOrNothing (\x -> x) supplier
                          }]

--
-- Support Gamma according notes on #1621
--

data CSVFormat_gamma__v1
  = CSVFormat_gamma__v1 {
       gamma__v1_callType :: !Text.Text
      ,gamma__v1_field2 :: !Text.Text
      ,gamma__v1_customerIdentifier :: !Text.Text
      ,gamma__v1_nonChargedParty :: !Text.Text
      ,gamma__v1_callDate :: !Text.Text
      ,gamma__v1_callTime :: !Text.Text
      ,gamma__v1_duration :: !Text.Text
      ,gamma__v1_bytesTransmitted :: !Text.Text
      ,gamma__v1_bytesReceived :: !Text.Text
      ,gamma__v1_description :: !Text.Text
      ,gamma__v1_chargeCode :: !Text.Text
      ,gamma__v1_timeBand :: !Text.Text
      ,gamma__v1_salesPrice :: !Text.Text
      ,gamma__v1_salesPricePreBundle :: !Text.Text
      ,gamma__v1_extension :: !Text.Text
      ,gamma__v1_ddi :: !Text.Text
      ,gamma__v1_groupingID :: !Text.Text
      ,gamma__v1_callClass :: !Text.Text
      ,gamma__v1_carrier :: !Text.Text
      ,gamma__v1_recording :: !Text.Text
      ,gamma__v1_vat :: !Text.Text
      ,gamma__v1_countryOfOrigin :: !Text.Text
      ,gamma__v1_network :: !Text.Text
      ,gamma__v1_retailTariffCode :: !Text.Text
      ,gamma__v1_remoteNetwork :: !Text.Text
      ,gamma__v1_apn :: !Text.Text
      ,gamma__v1_divertedNumber :: !Text.Text
      ,gamma__v1_ringTime :: !Text.Text
      ,gamma__v1_recordID :: !Text.Text
      ,gamma__v1_currency :: !Text.Text
      ,gamma__v1_callerLineIdentity :: !Text.Text
      ,gamma__v1_networkAccessReference :: !Text.Text
      ,gamma__v1_ngcsAccessCharge :: !Text.Text
      ,gamma__v1_ngcsServiceChange :: !Text.Text
      ,gamma__v1_totalBytesTransferred :: !Text.Text
      ,gamma__v1_userID :: !Text.Text
      ,gamma__v1_onwardBillingReference :: !Text.Text
      ,gamma__v1_contractName :: !Text.Text
      ,gamma__v1_bundleName :: !Text.Text
      ,gamma__v1_bundleAllowance :: !Text.Text
      ,gamma__v1_discounteReference :: !Text.Text
      ,gamma__v1_routingCode :: !Text.Text
  } deriving(Show)

instance CSV.FromRecord CSVFormat_gamma__v1 where
     parseRecord v =
         let expectedCols = 42
         in case V.length v == expectedCols of
              True
                -> CSVFormat_gamma__v1 <$>
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
                     v .! 11<*>
                     v .! 12<*>
                     v .! 13<*>
                     v .! 14<*>
                     v .! 15<*>
                     v .! 16<*>
                     v .! 17<*>
                     v .! 18<*>
                     v .! 19<*>
                     v .! 20<*>
                     v .! 21<*>
                     v .! 22<*>
                     v .! 23<*>
                     v .! 24<*>
                     v .! 25<*>
                     v .! 26<*>
                     v .! 27<*>
                     v .! 28<*>
                     v .! 29<*>
                     v .! 30<*>
                     v .! 31<*>
                     v .! 32<*>
                     v .! 33<*>
                     v .! 34<*>
                     v .! 35<*>
                     v .! 36<*>
                     v .! 37<*>
                     v .! 38<*>
                     v .! 39<*>
                     v .! 40<*>
                     v .! 41

              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_gamma__v1 where
  getCallDate record
    = do d <- importAndConvertNotNullValue2
                (Export $ gamma__v1_callDate record)
                (Export $ gamma__v1_callTime record)
                fromGammaCallDateAndTimeStampToLocalTime
                "callDate"
                "callTime"
                "call date"
         return $ Just d

  toCDR precision provider record = convert_CSVFormat_gamma__v1__toCDR precision provider record

convert_CSVFormat_gamma__v1__toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> CSVFormat_gamma__v1 -> Either AsterisellError [CDR]
convert_CSVFormat_gamma__v1__toCDR precision provider record = do

  let callClass = gamma__v1_callClass record
  -- "SIP", "IDA", and other codes identifying the global type of the call.
  -- The details of how to manage other fields, depend in part from the content of this field.

  callDate <- importAndConvertNotNullValue2
                (Export $ gamma__v1_callDate record)
                (Export $ gamma__v1_callTime record)
                fromGammaCallDateAndTimeStampToLocalTime
                "callDate"
                "callTime"
                "call date"

  duration <- importAndConvertNotNullValue (Export $ gamma__v1_duration record) fromTextToInt  "duration" "duration"

  let originNumber = gamma__v1_customerIdentifier record
  let destinationNumber
        = case Text.head $ gamma__v1_nonChargedParty record of
            '+' -> Text.tail $ gamma__v1_nonChargedParty record
            _ -> gamma__v1_nonChargedParty record

  let expectedCost = fromTextToRational $ gamma__v1_salesPrice record

  let callType = gamma__v1_callType record
  -- this can be "V" (standard voice call), "Z" (zero rated call), "G" (?),
  -- but it seems that the field callCharge suffices for identifying the type of the call.

  let chargeCode = gamma__v1_chargeCode record
  -- "UKN" and so on.
  -- Identifies completely the type of called telephone number.

  let timeBand = gamma__v1_timeBand record
  -- 1: peak
  -- 2: off-peak
  -- 3: weekend

  let timeBandAndChargeCode = Text.concat [chargeCode, "----time-band-", timeBand, "-", destinationNumber]
  -- NOTE: the imported rates must be imported using the same schema
  -- NOTE: use first chargeCode, in order to use a more compact telephone prefix table.

  case callClass of
    "IDA"
      -> let cdr =  cdr_empty callDate precision
         in  return $ [cdr { cdr_countOfCalls = 1
                           , cdr_direction = CDR_outgoing
                           , cdr_errorDirection = CDR_none
                           , cdr_isRedirect = False
                           , cdr_duration = Just duration
                           , cdr_billsec = Just duration
                           , cdr_externalTelephoneNumber = destinationNumber
                           , cdr_externalTelephoneNumberWithAppliedPortability = Just timeBandAndChargeCode
                           , cdr_internalTelephoneNumber = originNumber
                           , cdr_channel = Just callClass
                           , cdr_expectedCost = expectedCost
                           }]

    "SIPD"
      -> let cdr =  cdr_empty callDate precision
         in  return $ [cdr { cdr_countOfCalls = 1
                           , cdr_direction = CDR_outgoing
                           , cdr_errorDirection = CDR_none
                           , cdr_isRedirect = False
                           , cdr_duration = Just duration
                           , cdr_billsec = Just duration
                           , cdr_externalTelephoneNumber = destinationNumber
                           , cdr_externalTelephoneNumberWithAppliedPortability = Just timeBandAndChargeCode
                           , cdr_internalTelephoneNumber = originNumber
                           , cdr_channel = Just callClass
                           , cdr_expectedCost = expectedCost
                           }]

    "CPS"
      -> let cdr =  cdr_empty callDate precision
         in  return $ [cdr { cdr_countOfCalls = 1
                           , cdr_direction = CDR_outgoing
                           , cdr_errorDirection = CDR_none
                           , cdr_isRedirect = False
                           , cdr_duration = Just duration
                           , cdr_billsec = Just duration
                           , cdr_externalTelephoneNumber = destinationNumber
                           , cdr_externalTelephoneNumberWithAppliedPortability = Just timeBandAndChargeCode
                           , cdr_internalTelephoneNumber = originNumber
                           , cdr_channel = Just callClass
                           , cdr_expectedCost = expectedCost
                           }]

    _ -> throwError $ createError
                             Type_Error
                             Domain_RATES
                             ("unrecognized callClass - " ++ (Text.unpack callClass))
                             ("The field call class has an unrecognized content \"" ++ (Text.unpack callClass) ++ "\"")
                             ("The CDRs with this type will be not rated.")
                             ("If there are many errors of this type, contact the assistance, or the VoIP provider, because the code must support also this new type of CDR.")

--
-- Support Gamma Item Rental
--

data CSVFormat_gamma_ItemRental__v1
  = CSVFormat_gamma_ItemRental__v1 {
     gammaIR__v1_serviceActivatioDate :: !Text.Text
   , gammaIR__v1_billingMonthAndYear :: !Text.Text
   , gammaIR__v1_cli :: !Text.Text
   , gammaIR__v1_billingDescription :: !Text.Text
   , gammaIR__v1_eventType :: !Text.Text
   , gammaIR__v1_totalCost :: !Text.Text
   , gammaIR__v1_brokenPartCost :: !Text.Text
   , gammaIR__v1_quantity :: !Text.Text
  } deriving(Show)

instance CSV.FromRecord CSVFormat_gamma_ItemRental__v1 where
     parseRecord v =
         let expectedCols = 8
         in case V.length v == expectedCols of
              True
                -> CSVFormat_gamma_ItemRental__v1 <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6<*>
                     v .! 7
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_gamma_ItemRental__v1 where
  getCallDate record
    = do d <- importAndConvertNotNullValue
                (Export $ gammaIR__v1_billingMonthAndYear record)
                fromGammaItemRentalCallDateToLocalTime
                "month and year"
                "something like \"August 2015\""
         return $ Just d

  toCDR precision provider record = convert_CSVFormat_gamma_ItemRental__v1__toCDR precision provider record

gamma_ItemRental_channelName :: Text.Text
gamma_ItemRental_channelName = "item-rental"

gamma_ItemRental_rental :: Text.Text
gamma_ItemRental_rental = "--item-rental--"

gamma_ItemRental_connection :: Text.Text
gamma_ItemRental_connection = "--item-connection--"

-- | Process a CSV content like this:
--
--    Fields - value example:
--    Field 1 - Effective Date.  The effective date of the billing event day/month/year.
--              For cease events, this is the date the service was ceased.
--              - "02/06/2011"
--    Field 2 - Month and year, shows the month the charge applies to.
--              All one off and pro-rata charges will be for the month in question,
--              where advance charges will be for the following month.
--              The month described for pro-rata credits will be the month being credited for.
--              - "June 2011"
--    Field 3 - CLI - "2075156951"
--    Field 4 - Billing Description - "Prem SL line rental - WLR 3"
--    Field 5 - Event Type = Connection / Rental / Cease - "Rental"
--    Field 6 - Total Cost.  Total Cost = Unit price X quantity (field 8) - 0.0000
--    Field 7 - Broken Part Cost of the monthly rental.
--              2This figure may be negative for pro-rata credits given as a result of services ceasing outside
--              of their minimum billing term or a charge if service is ceased within minimum contract term.
--              - 9.7537
--    Field 8 - Quantity.  Up to 3 digits showing the unit quantity of the product being billed for.
--              - 1
convert_CSVFormat_gamma_ItemRental__v1__toCDR :: CurrencyPrecisionDigits -> CDRProviderName -> CSVFormat_gamma_ItemRental__v1 -> Either AsterisellError [CDR]
convert_CSVFormat_gamma_ItemRental__v1__toCDR precision provider record = do

  callDate <- importAndConvertNotNullValue
                (Export $ gammaIR__v1_billingMonthAndYear record)
                fromGammaItemRentalCallDateToLocalTime
                "month and year"
                "something like \"August 2015\""

  let account = gammaIR__v1_cli record

  let chargeCode1 = gammaIR__v1_billingDescription record

  let eventType = gammaIR__v1_eventType record

  let totalCostM = fromTextToRational $ gammaIR__v1_totalCost record

  let brokenPartCostM = fromTextToRational $ gammaIR__v1_brokenPartCost record

  let quantityM = fromTextToInt $ gammaIR__v1_quantity record

  (totalCost, brokenPartCost, quantity)
    <- case (totalCostM, brokenPartCostM, quantityM) of
         (Just a, Just b, Just c)
           -> return (a, b, c)
         _ -> throwError $ createError
                             Type_Error
                             Domain_RATES
                             ("expected number")
                             ("The CSV line should have numbers in totalCost, brokenPartCost and quantity, but one of them is not a recognizable number.")
                             ("The CDRs with this type will be not rated.")
                             ("If there are many errors of this type, contact the assistance, or the VoIP provider, because the code must support also this type of CDR.")

  chargeCode
    <- case eventType of
         "Rental"
           -> return $ Text.append gamma_ItemRental_rental chargeCode1
         "Connection"
           -> return $ Text.append gamma_ItemRental_connection chargeCode1
              -- one time activation cost

         _ -> throwError $ createError
                             Type_Error
                             Domain_RATES
                             ("unrecognized event type  - " ++ (Text.unpack eventType))
                             ("The field call class has an unrecognized event type \"" ++ (Text.unpack eventType) ++ "\"")
                             ("The CDRs with this type will be not rated.")
                             ("If there are many errors of this type, contact the assistance, or the VoIP provider, because the code must support also this type of CDR.")


  -- NOTE: I have no example of "Cease" line item, so I don't manage this value.
  -- In particular I don't know if:
  -- * a "Cease" line, is equivalent to "Rental", and then I subtract the broken part
  -- * a "Cease" line has negative broken part
  -- * a "Cease" line has no corresponding "Rental" part
  -- * a "Cease" line can never be used for a "Connection" item, because it is a one time cost
  -- In any case the income of a Cease, must be managed manually.
  -- Maybe in case of cease I must generate an additional CDR with only the broken cost (negative)

  let cdr =  cdr_empty callDate precision
  return [ (cdr_empty callDate precision) {
                cdr_countOfCalls = quantity
              , cdr_direction = CDR_outgoing
              , cdr_errorDirection = CDR_none
              , cdr_isRedirect = False
              , cdr_duration = Just 0
              , cdr_billsec = Just 0
              , cdr_externalTelephoneNumber = chargeCode1
              , cdr_displayedMaskedExternalTelephoneNumber = Just chargeCode1
              , cdr_displayedExternalTelephoneNumber = Just chargeCode1
              , cdr_externalTelephoneNumberWithAppliedPortability = Just chargeCode
              , cdr_internalTelephoneNumber = account
              , cdr_channel = Just gamma_ItemRental_channelName
              , cdr_expectedCost = Just $ totalCost + brokenPartCost
              }
         ]

--
-- Support Asterisk
--
-- NOTE: there can be many different versions of Asterisk format, so number with different versions,
-- and reuse whenever possible the code.
-- NOTE: use different versions also according the logical meaning of each field, so the version number identifies
-- also the CDR import logic.
-- NOTE: the verson number is a symbolic-code with the project name

-- | Generic Asterisk format, used as base for custom importers.
data CSVFormat_asterisk__generic
  = CSVFormat_asterisk__generic {
      -- these are standard fields
      asterisk__generic_calldate :: !(ExportMaybeNull Text.Text),
      asterisk__generic_clid :: !(ExportMaybeNull Text.Text),
      asterisk__generic_src :: !(ExportMaybeNull Text.Text),
      asterisk__generic_dst :: !(ExportMaybeNull Text.Text),
      asterisk__generic_dcontext :: !(ExportMaybeNull Text.Text),
      asterisk__generic_channel :: !(ExportMaybeNull Text.Text),
      asterisk__generic_dstchannel :: !(ExportMaybeNull Text.Text),
      asterisk__generic_lastapp :: !(ExportMaybeNull Text.Text),
      asterisk__generic_lastdata :: !(ExportMaybeNull Text.Text),
      asterisk__generic_duration :: !(ExportMaybeNull Text.Text),
      asterisk__generic_billsec :: !(ExportMaybeNull Text.Text),
      asterisk__generic_disposition :: !(ExportMaybeNull Text.Text),
      asterisk__generic_amaflags :: !(ExportMaybeNull Text.Text),
      asterisk__generic_accountcode :: !(ExportMaybeNull Text.Text),
      asterisk__generic_uniqueid :: !(ExportMaybeNull Text.Text),
      asterisk__generic_userfield :: !(ExportMaybeNull Text.Text),
      asterisk__generic_did :: !(ExportMaybeNull Text.Text)
    }

instance CSV.FromRecord CSVFormat_asterisk__generic where
     parseRecord v =
         let expectedCols = 17
         in case V.length v == expectedCols of
              True
                -> CSVFormat_asterisk__generic <$>
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
                     v .! 11<*>
                     v .! 12<*>
                     v .! 13<*>
                     v .! 14<*>
                     v .! 15<*>
                     v .! 16

              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance Show CSVFormat_asterisk__generic where
  show cdr
    = ""
      ++ (addLine "asterisk__generic_calldate" $ field asterisk__generic_calldate)
      ++ (addLine "asterisk__generic_clid" $ field asterisk__generic_clid)
      ++ (addLine "asterisk__generic_src" $ field asterisk__generic_src)
      ++ (addLine "asterisk__generic_dst" $ field asterisk__generic_dst)
      ++ (addLine "asterisk__generic_dcontext" $ field asterisk__generic_dcontext)
      ++ (addLine "asterisk__generic_channel" $ field asterisk__generic_channel)
      ++ (addLine "asterisk__generic_dstchannel" $ field asterisk__generic_dstchannel)
      ++ (addLine "asterisk__generic_lastapp" $ field asterisk__generic_lastapp)
      ++ (addLine "asterisk__generic_lastdata" $ field asterisk__generic_lastdata)
      ++ (addLine "asterisk__generic_duration" $ field asterisk__generic_duration)
      ++ (addLine "asterisk__generic_billsec" $ field asterisk__generic_billsec)
      ++ (addLine "asterisk__generic_disposition" $ field asterisk__generic_disposition)
      ++ (addLine "asterisk__generic_amaflags" $ field asterisk__generic_amaflags)
      ++ (addLine "asterisk__generic_accountcode" $ field asterisk__generic_accountcode)
      ++ (addLine "asterisk__generic_uniqueid" $ field asterisk__generic_uniqueid)
      ++ (addLine "asterisk__generic_userfield" $ field asterisk__generic_userfield)
      ++ (addLine "asterisk__generic_did" $ field asterisk__generic_did)
   where
     field = cdrField cdr

instance CDRFormat CSVFormat_asterisk__generic where
  getCallDate record
    = do d <- importAndConvertNotNullValue (asterisk__generic_calldate record) fromMySQLDateTimeAsTextToLocalTime "calldate" "call date"
         return $ Just d

  toCDR precision provider record
    = do cdr <- asterisk__generic_toCDR record precision provider
         return [cdr]

asterisk__generic_toCDR :: CSVFormat_asterisk__generic -> CurrencyPrecisionDigits -> CDRProviderName -> Either AsterisellError CDR
asterisk__generic_toCDR record precision provider
    = do callDate <- importAndConvertNotNullValue (asterisk__generic_calldate record) fromMySQLDateTimeAsTextToLocalTime "calldate" "call date"
         case (isEmptyOrNull $ asterisk__generic_dstchannel record)
              || (not $ (asterisk__generic_disposition record == Export "ANSWERED"))
              || (not $ (asterisk__generic_amaflags record == Export "3"    -- default
                         || asterisk__generic_amaflags record == Export "2" -- bill, documentation
                        )
                 ) of
           True
             -> -- the CDR can be ignored
                return $ (cdr_empty callDate precision) {
                                cdr_countOfCalls = 1
                              , cdr_direction = CDR_ignored
                              , cdr_errorDirection = CDR_none
                              }
           False
             -> do duration <- importAndConvertNotNullValue (asterisk__generic_duration record) fromTextToInt  "duration" "duration"
                   billsec <- importAndConvertNotNullValue (asterisk__generic_billsec record) fromTextToInt  "billsec" "billsec"
                   let cdr =  cdr_empty callDate precision
                   return $ cdr { cdr_duration = Just duration
                                , cdr_billsec = Just billsec
                                }
                   -- return a semi-classified CDR. The other more specific versions will use a better classification method.


-- | anton_panferov project version.
data CSVFormat_asterisk__a_p_v1
  = CSVFormat_asterisk__a_p_v1 {
      asterisk__a_p_v1_generic :: CSVFormat_asterisk__generic,

      -- these are custom fields
      asterisk__a_p_v1_recordingfile :: !(ExportMaybeNull Text.Text),
      asterisk__a_p_v1_cnum :: !(ExportMaybeNull Text.Text),
      asterisk__a_p_v1_cnam :: !(ExportMaybeNull Text.Text),
      asterisk__a_p_v1_outbound_cnum :: !(ExportMaybeNull Text.Text),
      asterisk__a_p_v1_outbound_cnam :: !(ExportMaybeNull Text.Text),
      asterisk__a_p_v1_dst_cnam :: !(ExportMaybeNull Text.Text)
    }

instance Show CSVFormat_asterisk__a_p_v1 where
  show cdr
    = (show (asterisk__a_p_v1_generic cdr))
        ++ (addLine "asterisk__a_p_v1_recordingfile" $ field asterisk__a_p_v1_recordingfile)
        ++ (addLine "asterisk__a_p_v1_cnum" $ field asterisk__a_p_v1_cnum)
        ++ (addLine "asterisk__a_p_v1_cnam" $ field asterisk__a_p_v1_cnam)
        ++ (addLine "asterisk__a_p_v1_outbound_cnum" $ field asterisk__a_p_v1_outbound_cnum)
        ++ (addLine "asterisk__a_p_v1_outbound_cnam" $ field asterisk__a_p_v1_outbound_cnam)
        ++ (addLine "asterisk__a_p_v1_dst_cnam" $ field asterisk__a_p_v1_dst_cnam)
   where
     field = cdrField cdr

instance CSV.FromRecord CSVFormat_asterisk__a_p_v1 where
     parseRecord v =
         let expectedCols = 23
         in case V.length v == expectedCols of
              True
                -> let gr = CSVFormat_asterisk__generic <$>
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
                              v .! 11<*>
                              v .! 12<*>
                              v .! 13<*>
                              v .! 14<*>
                              v .! 15<*>
                              v .! 16
                   in CSVFormat_asterisk__a_p_v1 <$>
                        gr <*>
                        v .! 17<*>
                        v .! 18<*>
                        v .! 19<*>
                        v .! 20<*>
                        v .! 21<*>
                        v .! 22
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)


instance CDRFormat CSVFormat_asterisk__a_p_v1 where
  getCallDate record = getCallDate $ asterisk__a_p_v1_generic record

  toCDR precision provider record
    = do let gr = asterisk__a_p_v1_generic record
         cdr1 <- asterisk__generic_toCDR gr precision provider
         let ignoredCDR = cdr1 { cdr_direction = CDR_ignored }

         case cdr_direction cdr1 of
           CDR_ignored
             -> return [ignoredCDR]
           _
             -> case asterisk__generic_dstchannel gr of
                  ExportNull
                    -> return [ignoredCDR]
                  Export dstChannel
                    -> do -- apply the #1730
                          -- You must proccess calls with dstchannel begins with: "SIP/ext-".
                          -- And also "DAHDI/" dstchannel with called number begins with "98"
                          (isOutgoing, check98)
                            <- case Text.isPrefixOf "SIP/ext-" dstChannel of
                                 True
                                   -> return (True, False)
                                 False
                                   -> case Text.isPrefixOf "DAHDI/" dstChannel of
                                        True
                                          -> return (True, True)
                                        False
                                          -> return (False, False)
                          case isOutgoing of
                            False
                              -> return [ignoredCDR]
                            True
                              -> do externalTelephoneNumber1 <- importAndConvertNotNullValue (asterisk__generic_lastdata gr) (\x -> Just $ fromAsteriskLastDataToExtension x) "lastdata" "external telephone number"
                                    internalTelephoneNumber <- importNotNullText (asterisk__generic_src gr) "src" "internal extension/account"
                                    let check98Passed
                                          = case check98 of
                                              False -> True
                                              True -> Text.isPrefixOf "98" externalTelephoneNumber1
                                    let externalTelephoneNumber2
                                          = case Text.isPrefixOf "00" externalTelephoneNumber1 of
                                              True -> Text.drop 2 externalTelephoneNumber1
                                              False -> Text.cons '7' externalTelephoneNumber1
                                    case check98Passed of
                                      False
                                        -> return [ignoredCDR]
                                      True
                                        -> return $ [cdr1 {
                                                            cdr_direction = CDR_outgoing
                                                          , cdr_externalTelephoneNumber = externalTelephoneNumber2
                                                          , cdr_internalTelephoneNumber = internalTelephoneNumber
                                                          , cdr_channel = Just dstChannel
                                                          }]

--
-- Plain Formats
--

-- Here is the tags and example cdr:
--
-- cdrdate;callernumber;callednumber;callername;calledname;duration;uniqueid
-- 2015-06-21 02:43:30;4915163544XXX;905069460XXX;SIP_CUSTONE;GSMGWS;368;194358247-16792-84452
-- I tried to keep cdr format simple.
--
-- Callername: this is the name of the customer. This is the key to match the cdrs with the correct customer at asterisell5
-- Calledname: this is the name of the supplier. This is the key to match the cdrs with the correct supplier/carrier at asterisell5
-- The cdr files will be in the /cdr folder of the asterisell5.
--
-- I hope you have enough time to start working on this process.

-- | A plain format with
--   > cdrdate;callernumber;callednumber;callername;calledname;duration;uniqueid
--   where
--   * calldate is in MySQL format
--   * Callername: this is the name of the customer. This is the key to match the cdrs with the correct customer at asterisell5
--   * Calledname: this is the name of the supplier. This is the key to match the cdrs with the correct supplier/carrier at asterisell5
--  The CSV fields are separated from ";" character.
data CSVFormat_plain1
  = CSVFormat_plain1 {
      plain1__calldate :: !(ExportMaybeNull Text.Text),
      plain1__callerNumber :: !(ExportMaybeNull Text.Text),
      plain1__calledNumber :: !(ExportMaybeNull Text.Text),
      plain1__account :: !(ExportMaybeNull Text.Text),
      plain1__channel :: !(ExportMaybeNull Text.Text),
      plain1__duration :: !(ExportMaybeNull Text.Text),
      plain1__id :: !(ExportMaybeNull Text.Text)
   } deriving(Show)

instance CSV.FromRecord CSVFormat_plain1 where
     parseRecord v =
         let expectedCols = 7
         in case V.length v == expectedCols of
              True
                -> CSVFormat_plain1 <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6

              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_plain1 where
  getCallDate record
    = do d <- importAndConvertNotNullValue (plain1__calldate record) fromMySQLDateTimeAsTextToLocalTime "calldate" "call date"
         return $ Just d

  toCDR precision provider record
    = do cdr <- plain1_toCDR record precision provider
         return [cdr]

plain1_toCDR :: CSVFormat_plain1 -> CurrencyPrecisionDigits -> CDRProviderName -> Either AsterisellError CDR
plain1_toCDR record precision provider
    = do callDate <- importAndConvertNotNullValue (plain1__calldate record) fromMySQLDateTimeAsTextToLocalTime "calldate" "call date"
         case plain1__duration record of
           ExportNull
             -> return (cdr_empty callDate precision) {
                            cdr_countOfCalls = 1
                          , cdr_direction = CDR_ignored
                          , cdr_errorDirection = CDR_none
                          }
           Export ""
             -> return (cdr_empty callDate precision) {
                            cdr_countOfCalls = 1
                          , cdr_direction = CDR_ignored
                          , cdr_errorDirection = CDR_none
                          }
           Export "0"
             -> return (cdr_empty callDate precision) {
                            cdr_countOfCalls = 1
                          , cdr_direction = CDR_ignored
                          , cdr_errorDirection = CDR_none
                          }
           _ -> do
                   billsec <- importAndConvertNotNullValue (plain1__duration record) fromTextToInt  "duration" "duration"
                   callerNumber <- importNotNullText (plain1__callerNumber record) "callerNumber" "caller number"
                   calledNumber <- importNotNullText (plain1__calledNumber record) "calledNumber" "called number"
                   account  <- importNotNullText (plain1__account record) "account" "customer account"
                   channel  <- importNotNullText (plain1__channel record) "channel" "channe/provider/vendor"

                   let cdr =  cdr_empty callDate precision
                   return $ cdr { cdr_duration = Just billsec
                                , cdr_billsec = Just billsec
                                , cdr_direction = CDR_outgoing
                                , cdr_channel = Just channel
                                , cdr_externalTelephoneNumber = calledNumber
                                , cdr_internalTelephoneNumber = account
                                }

-- | A format like
--
-- > N.;DataOra;Start;End;Durata Secondi;Prezzo in euro;Descrizione
-- > 00001;07/04/2016 09.12.00;05221503222     ;+393487247930       ;0000011;0,003483;Cellulare VODAFONE
--
-- The format is documented on https://support.asterisell.com/issues/1957
data CSVFormat_digitel
  = CSVFormat_digitel {
      digitel__nr :: !(ExportMaybeNull Text.Text),
      digitel__calldate :: !(ExportMaybeNull Text.Text),
      digitel__callerNumber :: !(ExportMaybeNull Text.Text),
      digitel__calledNumber :: !(ExportMaybeNull Text.Text),
      digitel__billsec :: !(ExportMaybeNull Text.Text),
      digitel__cost :: !(ExportMaybeNull Text.Text),
      digitel__description :: !(ExportMaybeNull Text.Text)
   } deriving(Show)

instance CSV.FromRecord CSVFormat_digitel where
     parseRecord v =
         case V.length v of
              7    -- NOTE: important to use a number instead of an identifier
                   -- otherwise it is assigned instead of selected as pattern matching.
                -> CSVFormat_digitel <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6 
              0 -> return $ CSVFormat_digitel ExportNull (Export "ignore") ExportNull ExportNull ExportNull ExportNull ExportNull
              l -> fail $ "There are " ++ show l ++ " columns instead of the expected " ++ (show 7)

instance CDRFormat CSVFormat_digitel where
  getCallDate record = digitel_convertCallDate (digitel__calldate record)

  toCDR precision provider record
    = do cdr <- digitel_toCDR False record precision provider
         return [cdr]

digitel_convertCallDate :: ExportMaybeNull Text.Text -> Either AsterisellError (Maybe LocalTime)
digitel_convertCallDate t1
    = do t2 <- importAndConvertNotNullValue t1 Just "calldate" "call date"
         case t2 of
           "DataOra"
             -> return Nothing
                -- NOTE: this is the initial header
           "ignore"
             -> return Nothing
                -- NOTE: this is the final empty line
           _ -> do d <- importAndConvertNotNullValue t1 (fromDateFormat2ToLocalTime '/' ' ' '.') "calldate" "call date"
                   return $ Just d

digitel_toCDR :: Bool -> CSVFormat_digitel -> CurrencyPrecisionDigits -> CDRProviderName -> Either AsterisellError CDR
digitel_toCDR isNNG record precision provider
    = do callDate <- importAndConvertNotNullValue (digitel__calldate record) (fromDateFormat2ToLocalTime '/' ' ' '.') "calldate" "call date"
         billsec <- importAndConvertNotNullValue (digitel__billsec record) fromTextToInt  "duration" "duration"
         callerNumber <- Text.strip <$> importNotNullText (digitel__callerNumber record) "callerNumber" "caller number"
         (calledNumber, calledNumberToDisplay)
           <- importAndConvertNotNullValue (digitel__calledNumber record) ((normalizeCalledNumber callDate) . Text.strip) "calledNumber" "called number"
         vendorCost <- importAndConvertNotNullValue (digitel__cost record) (fromTextToRational2 ',') "cost" "cost"

         let cdr =  cdr_empty callDate precision
         return $ cdr { cdr_duration = Just billsec
                      , cdr_billsec = Just billsec
                      , cdr_direction = CDR_outgoing
                      , cdr_channel = Just provider
                      , cdr_externalTelephoneNumber = calledNumber
                      , cdr_displayedExternalTelephoneNumber = Just calledNumberToDisplay
                      , cdr_internalTelephoneNumber = callerNumber
                      , cdr_expectedCost = Just vendorCost
                      }
 where

   normalizeCalledNumber  = digitel_normalizeCalledNumber isNNG

const_digitelHeader :: Text.Text
const_digitelHeader = "N.;DataOra;Start;End;Durata Secondi;Prezzo in euro;Descrizione"

-- | True for peak time, False for Off-Peakt time.
const_digitelNNGTimeBandPrefix :: Bool -> Text.Text
const_digitelNNGTimeBandPrefix isPeak = Text.append "digitel-nng-" (if isPeak then "peak--" else "off-peak--")

-- | Normalize a telephone number in Digitel format, adding (optionally) info about the
--   peakd and off peak time band.
--   Return the number to use for rating, and the number to show in the call report.
digitel_normalizeCalledNumber :: Bool -> LocalTime -> Text.Text -> Maybe (Text.Text, Text.Text)
digitel_normalizeCalledNumber  useTimeBand callDate number
     = let (_, _, dc) = toWeekDate (localDay callDate)
           isSunday = dc == 7
           isSaturday = dc == 6
           tc = localTimeOfDay callDate
           (t1, t2)
             = if isSaturday
               then (TimeOfDay 8 0 0, TimeOfDay 13 0 0)
               else (TimeOfDay 8 0 0, TimeOfDay 18 30 0)
           isPeakHour = (not isSunday) && tc >= t1 && tc < t2

           sanitizeCalledNumber :: Text.Text -> Maybe Text.Text
           sanitizeCalledNumber n
             = case Text.head n of
                 '+' -> Just $ Text.tail n
                 _ -> Just n

       in case sanitizeCalledNumber number of
            Nothing -> Nothing
            Just n -> case useTimeBand of
                        True -> Just $ (Text.append (const_digitelNNGTimeBandPrefix isPeakHour) n, n)
                        False -> Just (n, n)

-- | Support Digitel NNG calls according notes on #1972
--
-- A format like:
--
-- > N.;DataOra;Start;End;Durata Secondi;Prezzo in euro;Descrizione
-- > 00001;21/09/2016 00.00.22;0445325362      ;+39800016946        ;0000024;0,000000;Numero Verde
-- > 00002;21/09/2016 01.31.43;0445325362      ;+39800923361        ;0000017;0,000000;Numero Verde
--
newtype CSVFormat_digitelNNG__v1 = CSVFormat_digitelNNG__v1 CSVFormat_digitel
 deriving (Show)

instance CSV.FromRecord CSVFormat_digitelNNG__v1 where
     parseRecord v = CSVFormat_digitelNNG__v1 <$> parseRecord v

instance CDRFormat CSVFormat_digitelNNG__v1 where
  getCallDate (CSVFormat_digitelNNG__v1 record)
    = Just <$> importAndConvertNotNullValue
                 (digitel__calldate record)
                 (fromDateFormat2ToLocalTime '/' ' ' '.')
                 "callDate"
                 "call date in dd/mm/yyyy hh.mm.ss format"

  toCDR precision provider (CSVFormat_digitelNNG__v1 record) = do
    cdr <- digitel_toCDR True record precision provider
    return [cdr]

-- | A format like
--
-- > 150194;2016420000000;DAD;DAD;4315132***;01.08.2016;11:26:54;St.Pölten;Colt LOCAL;0274290136***;22;0.01019;0;02.09.2016;EUR;;VO15;P
data CSVFormat_colt
  = CSVFormat_colt {
        colt__providerId :: !(ExportMaybeNull Text.Text)
      , colt__2 :: !(ExportMaybeNull Text.Text)
      , colt__3 :: !(ExportMaybeNull Text.Text)
      , colt__4 :: !(ExportMaybeNull Text.Text)
      , colt__caller :: !(ExportMaybeNull Text.Text)
      , colt__date :: !(ExportMaybeNull Text.Text)
        -- ^ format:  01.08.2016
      , colt__time :: !(ExportMaybeNull Text.Text)
        -- ^ format: 11:26:54;
      , colt__8 :: !(ExportMaybeNull Text.Text)
      , colt__9 :: !(ExportMaybeNull Text.Text)
      , colt__called :: !(ExportMaybeNull Text.Text)
      , colt__billsec :: !(ExportMaybeNull Text.Text)
      , colt__cost :: !(ExportMaybeNull Text.Text)
        -- ^ format: 0.01019
      , colt__13:: !(ExportMaybeNull Text.Text)
      , colt__14:: !(ExportMaybeNull Text.Text)
      , colt__15:: !(ExportMaybeNull Text.Text)
      , colt__16:: !(ExportMaybeNull Text.Text)
      , colt__17:: !(ExportMaybeNull Text.Text)
      , colt__18:: !(ExportMaybeNull Text.Text)
      , colt__19:: !(ExportMaybeNull Text.Text)
      , colt__20:: !(ExportMaybeNull Text.Text)
   } deriving(Show)

instance CSV.FromRecord CSVFormat_colt where
     parseRecord v =
         let expectedCols = 20
         in case V.length v == expectedCols of
              True
                -> CSVFormat_colt <$>
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
                     v .! 11<*>
                     v .! 12<*>
                     v .! 13<*>
                     v .! 14<*>
                     v .! 15<*>
                     v .! 16<*>
                     v .! 17<*>
                     v .! 18<*>
                     v .! 19
              False -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance CDRFormat CSVFormat_colt where
  getCallDate rc = Just <$> colt_convertCallDate (colt__date rc) (colt__time rc)

  toCDR precision provider rc
    = do cdr <- colt_toCDR Nothing rc precision provider
         return [cdr]

colt_convertCallDate :: ExportMaybeNull Text.Text -> ExportMaybeNull Text.Text -> Either AsterisellError LocalTime
colt_convertCallDate d1 t1 = do
  let parseDT d t = fromDateFormat2ToLocalTime '.' ' ' ':' (Text.concat [d, " ", t])
  importAndConvertNotNullValue2 d1 t1 parseDT "callDate" "callTime" "call date"

colt_toCDR :: Maybe Text.Text -> CSVFormat_colt -> CurrencyPrecisionDigits -> CDRProviderName -> Either AsterisellError CDR
colt_toCDR maybeLocalPrefix rc precision provider = do 
  callDate <- colt_convertCallDate (colt__date rc) (colt__time rc)
  billsec <- importAndConvertNotNullValue (colt__billsec rc) fromTextToInt  "duration" "duration"
  callerNumber <- Text.strip <$> importNotNullText (colt__caller rc) "callerNumber" "caller number"
  calledNumber1 <- Text.strip <$> importNotNullText (colt__called rc) "calledNumber" "called number"
  vendorCost <- importAndConvertNotNullValue (colt__cost rc) (fromTextToRational2 ',') "cost" "cost"

  let calledNumber2
          = case maybeLocalPrefix of
              Nothing
                  -> calledNumber1
              Just localPrefix
                   -> case Text.isPrefixOf "00" calledNumber1 of
                        True -> Text.drop 2 calledNumber1
                                -- this is an international calls, so normalize the number
                        False -> case Text.isPrefixOf "0" calledNumber1 of
                                   True -> Text.concat [localPrefix, Text.drop 1 calledNumber1]
                                           -- this a local call, and normalize to a call using the default prefix
                                   False -> calledNumber1

  let cdr =  cdr_empty callDate precision
  return $ cdr { cdr_duration = Just billsec
               , cdr_billsec = Just billsec
               , cdr_direction = CDR_outgoing
               , cdr_channel = Just provider
               , cdr_externalTelephoneNumber = calledNumber2
               , cdr_internalTelephoneNumber = callerNumber
               , cdr_expectedCost = Just vendorCost
               }

-- | CDRs calls where the international calls starts with 00,
--   and local calls start with 0 and they are assigned to
--   the international prefix 43 (Austria)
newtype CSVFormat_colt43 = CSVFormat_colt43 CSVFormat_colt
 deriving Show

instance CSV.FromRecord CSVFormat_colt43 where
     parseRecord v = do
       rc :: CSVFormat_colt <- parseRecord v
       return $ CSVFormat_colt43 rc 
 
instance CDRFormat CSVFormat_colt43 where
  getCallDate (CSVFormat_colt43 rc) = Just <$> colt_convertCallDate (colt__date rc) (colt__time rc)

  toCDR precision provider (CSVFormat_colt43 rc)
    = do cdr <- colt_toCDR (Just "43") rc precision provider
         return [cdr]

-- ---------------------------------------------
-- TESTS

tt_customerSpecificImporters
  = test_digitelNumbers True [peak1, peak2, peak3, saturdayPeak1, saturdayPeak2]
  ++ test_digitelNumbers False [sunday1, sunday2, saturdayOff1, saturdayOff2, off1, off2, off3, off4] 
 where

  sunday1 = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-30 01:00:00"  
  sunday2 = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-30 00:00:00"
  saturdayOff1 = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-29 14:00:00"
  saturdayOff2 = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-29 13:00:00"
  saturdayPeak1 = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-29 08:00:00"
  saturdayPeak2 = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-29 09:00:00"
  peak1  = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 13:00:00"
  peak2  = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 08:00:00"
  peak3  = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 18:29:00"
  off1   = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 19:00:00"
  off2  = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 05:00:00"
  off3  = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 18:30:00"
  off4  = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2016-10-28 00:00:00"

  test_digitelNumbers :: Bool -> [LocalTime] -> [HUnit.Test]
  test_digitelNumbers isPeak times
    = List.map
        (\time ->
             let testFun = test_digitelNumber time "+390595555" (Text.append (const_digitelNNGTimeBandPrefix isPeak) "390595555")
                 testName = "Digitel peak and off peak telephone number conversion at  " ++ fromLocalTimeToMySQLDateTime time
             in  HUnit.TestCase $ HUnit.assertBool testName testFun) times

  test_digitelNumber time n1 n2
    = case digitel_normalizeCalledNumber True time n1 of
        Just (nn1, nn2) -> Text.isPrefixOf n2 nn1
        Nothing -> False
