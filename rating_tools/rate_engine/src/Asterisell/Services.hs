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


-- | Manage Rent Services associated to customers.
--   They are different from Bundle Service CDRs, because they are explicitely associated to a customer though Service related tables,
--   and they are not associated to BundleRate and Price Category.
module Asterisell.Services (
  service_exportToCSVTheServiceCDRSTelephonePrefixes
 ,service_defaultExternalTelephoneNumber
 ,service_generate
 ,loadServiceParams
 ,CalcService(..)
 ,ServiceId
 ,ServiceIdMap
 ,ServiceParams
 ,tt_servicesTests 
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

import qualified Data.Map as Map
import qualified Pipes.Csv as PC
import Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad
import Control.Applicative
import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Safe as PS 
import qualified Pipes.Safe.Prelude  as PS
import qualified Pipes.ByteString    as PB
import qualified Data.ByteString.Lazy as BS
import Pipes.Csv
import qualified Data.Csv as CSV
import Data.Vector as V hiding((++))
import Data.Hashable
import Data.Maybe
import Data.IntMap as IMap
import System.IO
import Debug.Trace
import Control.Monad.Except

import qualified Test.HUnit as HUnit


--
-- Import CSV Files
--

type MySQLBool = Int

fromMySQLBool :: MySQLBool -> Bool
fromMySQLBool 0 = False
fromMySQLBool 1 = True

data CSVFormat_services
  = CSVFormat_services {
      services_id :: !Int
    , services_name :: !(ExportMaybeNull Text.Text)
    , services_description :: !(ExportMaybeNull Text.Text)  
    , services_priceIsProportionalToActivationDate :: !MySQLBool
    , services_priceChangeWithPriceList :: !MySQLBool
    , services_isAppliedOnlyOneTime :: !MySQLBool
    , services_scheduleTimeFrame :: !(ExportMaybeNull Text.Text)
    , services_scheduleFrom :: !(ExportMaybeNull Text.Text)
  } deriving(Show)

data Service
  = Service {
      service_id :: !Int
    , service_name :: !Text.Text
    , service_description :: !Text.Text  
    , service_priceIsProportionalToActivationDate :: !Bool
    , service_priceChangeWithPriceList :: !Bool
    , service_isAppliedOnlyOneTime :: !Bool
    , service_schedule :: BundleRateTimeFrame
  } deriving(Show)

toService :: CSVFormat_services -> Either String Service
toService csv
  = do s <- case (fromExportOrEmptyString $ services_scheduleTimeFrame csv) of
              "monthly"
                -> case fromTextToInt $ fromExportOrEmptyString $ services_scheduleFrom csv of
                     Nothing
                       -> throwError $ "Expected a number, instead of " ++ (show $ fromExportOrEmptyString $ services_scheduleFrom csv)
                     Just i
                       -> return $ MonthlyTimeFrame i  
                        
              "weekly"
                -> case (fromExportOrEmptyString $ services_scheduleFrom csv) of
                     "Monday" -> return $ WeeklyTimeFrame 1
                     "Tuesday" -> return $ WeeklyTimeFrame 2
                     "Wednesday" -> return $ WeeklyTimeFrame 3
                     "Thursday" -> return $ WeeklyTimeFrame 4
                     "Friday" -> return $ WeeklyTimeFrame 5
                     "Saturday" -> return $ WeeklyTimeFrame 6
                     "Sunday" -> return $ WeeklyTimeFrame 7
                     _ -> throwError $ "Expected a day of week like Monday, Tuesday, and so on, instead of " ++ (show $ fromExportOrEmptyString $ services_scheduleFrom csv)
    
       return Service {
                service_id = services_id csv
              , service_name = fromExportOrEmptyString $ services_name csv
              , service_description = fromExportOrEmptyString $ services_description csv       
              , service_priceIsProportionalToActivationDate = fromMySQLBool $ services_priceIsProportionalToActivationDate csv
              , service_priceChangeWithPriceList = fromMySQLBool $ services_priceChangeWithPriceList csv
              , service_isAppliedOnlyOneTime = fromMySQLBool $ services_isAppliedOnlyOneTime csv
              , service_schedule = s 
              }

instance PC.FromRecord CSVFormat_services where
     parseRecord v =
         let expectedCols = 8
         in case V.length v == expectedCols of
              True
                -> CSVFormat_services <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6<*>
                     v .! 7
                     
              False
                -> fail $ "There are " ++ (show $ V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

data CSVFormat_servicePriceList
  = CSVFormat_servicePriceList {
      servicePriceList_id :: !Int
    , servicePriceList_serviceId :: !(ExportMaybeNull Int)
    , servicePriceList_fromDate :: !(ExportMaybeNull Text.Text)
    , servicePriceList_price :: !Int
    } deriving (Show)

data ServicePrice
  = ServicePrice {
      servicePrice_id :: !Int
    , servicePrice_serviceId :: !Int
    , servicePrice_fromDate :: !CallDate
    , servicePrice_price :: !MonetaryValue
    } deriving (Show)

toServicePrice :: EnvParams -> CSVFormat_servicePriceList -> Either String ServicePrice
toServicePrice env csv
  = do
       let precision = params_currencyPrecision env
       let price = fromIntegerWithFixedPrecisionToMonetaryValue precision (servicePriceList_price csv)
       let ds = Text.unpack $ fromExportOrEmptyString $ servicePriceList_fromDate csv

       d <- case fromMySQLDateTimeToLocalTime ds of
              Nothing
                -> throwError $ "Expected date, instead of " ++ ds
              Just r
                -> return r
                   
       sid <- case servicePriceList_serviceId csv of
                ExportNull -> throwError $ "Expected service id"
                Export i -> return i
                
       return ServicePrice {
                servicePrice_id = servicePriceList_id csv
              , servicePrice_serviceId  = sid
              , servicePrice_fromDate = d
              , servicePrice_price = price
              }

instance PC.FromRecord CSVFormat_servicePriceList where
     parseRecord v =
         let expectedCols = 4
         in case V.length v == expectedCols of
              True
                -> CSVFormat_servicePriceList <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3
                     
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

data CSVFormat_assignedServices
  = CSVFormat_assignedServices {
      assignedServices_id :: !Int
    , assignedServices_serviceId :: !(ExportMaybeNull Int)
    , assignedServices_unitId :: !(ExportMaybeNull Int)
    , assignedServices_nrOfItems :: !Int
    , assignedServices_fromDate :: !(ExportMaybeNull Text.Text)
    , assignedServices_discount :: !Int
    } deriving (Show)

instance PC.FromRecord CSVFormat_assignedServices where
     parseRecord v =
         let expectedCols = 6
         in case V.length v == expectedCols of
              True
                -> CSVFormat_assignedServices <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5
                     
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

data AssignedService
  = AssignedService {
      assignedService_id :: !Int
    , assignedService_serviceId :: !Int
    , assignedService_unitId :: !Int
    , assignedService_nrOfItems :: !Int
    , assignedService_fromDate :: !CallDate
    , assignedService_discount :: !Rational
    } deriving (Show)

toAssignedService :: CSVFormat_assignedServices -> Either String AssignedService
toAssignedService csv
  = do sid <- case assignedServices_serviceId csv of
                ExportNull -> throwError $ "service id must be specified"
                Export i -> return i
                
       uid <- case assignedServices_unitId csv of
                ExportNull -> throwError $ "unit id must be specified"
                Export i -> return i
                
       let ds = Text.unpack $ fromExportOrEmptyString $ assignedServices_fromDate csv
       d <- case fromMySQLDateTimeToLocalTime ds of
              Nothing
                -> throwError $ "Expected date, instead of " ++ ds
              Just r
                -> return r

       let discount :: Rational = (toRational $ assignedServices_discount csv) / (toRational 100)

       return AssignedService {
                assignedService_id = assignedServices_id csv
              , assignedService_serviceId = sid
              , assignedService_unitId = uid
              , assignedService_nrOfItems = assignedServices_nrOfItems csv
              , assignedService_fromDate= d
              , assignedService_discount = discount
              }     

-- | Load services from files generated in the PHP world.
loadServiceParams :: Bool -> String -> String -> String -> EnvParams -> IO (Either String ServiceParams)
loadServiceParams isDebugMode servicesFileName servicePriceListFileName assignedServicesFileName envParams = runSafeT $ do 

     services <- PS.withFile servicesFileName ReadMode $ \handle -> do
         
         -- this is a pipe that convert a file to a stream of bytes
         let sourceFile = PB.fromHandle handle

         -- this is a pipe that convert the stream of bytes to a stream of records.
         let sourceRecords :: Producer (Either String CSVFormat_services) (PS.SafeT IO) ()
             sourceRecords = PC.decodeWith csvOptions PC.NoHeader sourceFile
    
         PP.fold (importServices servicesFileName) (Right IMap.empty, 1) makeResult sourceRecords 

     prices <- PS.withFile servicePriceListFileName ReadMode $ \handle -> do
         
         -- this is a pipe that convert a file to a stream of bytes
         let sourceFile = PB.fromHandle handle

         -- this is a pipe that convert the stream of bytes to a stream of records.
         let sourceRecords :: Producer (Either String CSVFormat_servicePriceList) (PS.SafeT IO) ()
             sourceRecords = PC.decodeWith csvOptions PC.NoHeader sourceFile
    
         PP.fold (importPrices servicePriceListFileName) (Right IMap.empty, 1) makeResult sourceRecords 

     assigned <- PS.withFile assignedServicesFileName ReadMode $ \handle -> do
         
         -- this is a pipe that convert a file to a stream of bytes
         let sourceFile = PB.fromHandle handle

         -- this is a pipe that convert the stream of bytes to a stream of records.
         let sourceRecords :: Producer (Either String CSVFormat_assignedServices) (PS.SafeT IO) ()
             sourceRecords = PC.decodeWith csvOptions PC.NoHeader sourceFile
    
         PP.fold (importAssigned servicePriceListFileName) (Right IMap.empty, 1) makeResult sourceRecords 

     case (services, prices, assigned) of
       (Right s, Right p, Right a)
         -> do let r = ServiceParams {
                                 serviceParams_services = s
                               , serviceParams_prices = p
                               , serviceParams_assigned = a
                               }
               case isDebugMode of
                 False
                   -> return $ Right r
                 True
                   -> case serviceParams_respectCodeContracts r of
                        Right ()
                          -> return $ Right r
                        Left err
                          -> return $ Left $ "Error in application code. Failed code contracts. " ++ err
                     
       (Left err, _, _) -> return $ Left err
       (_, Left err, _) -> return $ Left err
       (_, _, Left err) -> return $ Left err
 
 where

   makeResult (Left err, _) 
     = Left err

   makeResult (Right r, _)
     = Right r

   csvOptions = PC.defaultDecodeOptions

   importServices fileName (Left err, i) _ 
     = (Left err, i)

   importServices fileName (Right map1, i) maybeRecord 
     = case maybeRecord of 
         Left err 
           -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
         Right record
           -> case toService record of
                Left err
                  -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
                Right record2     
                  -> (Right $ IMap.insert (service_id record2) record2 map1, i + 1)


   importPrices fileName (Left err, i) _ 
     = (Left err, i)

   importPrices fileName (Right map1, i) maybeRecord 
     = case maybeRecord of 
         Left err 
           -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
         Right record
           -> case toServicePrice envParams record of
                Left err 
                  -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
                Right record2
                  -> (Right $ IMap.insertWith (addToHead) (servicePrice_serviceId record2) [record2] map1, i + 1)

   importAssigned fileName (Left err, i) _ 
     = (Left err, i)

   importAssigned fileName (Right map1, i) maybeRecord 
     = case maybeRecord of 
         Left err 
           -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
         Right record
           -> case toAssignedService record of
                Left err
                  -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
                Right record2
                  -> (Right $ serviceParams_insertAssignment map1 record2, i + 1)

   addToHead [newValue] oldList = newValue:oldList

--
-- Service Params Access
--

type ServiceId = Int

type ServiceIdMap a = RatePlanIdMap a 

-- | Indexed services. All info are in reverse order of activation date.
data ServiceParams
       = ServiceParams {
           serviceParams_services :: ServiceIdMap Service
         , serviceParams_prices :: ServiceIdMap [ServicePrice]
           -- ^ @ensure prices are ordered by date in reverse order
         , serviceParams_assigned :: ServiceIdMap (UnitIdMap [AssignedService])
           -- @ensure assignments are in reverse order of assignment 
         } deriving(Show)

serviceParams_insertAssignment :: ServiceIdMap (UnitIdMap [AssignedService]) -> AssignedService -> ServiceIdMap (UnitIdMap [AssignedService])
serviceParams_insertAssignment map1 assignment
  = let serviceId = assignedService_serviceId assignment
        unitId = assignedService_unitId assignment
        
        addToHead [newValue] oldList = newValue:oldList

    in case IMap.lookup serviceId map1 of
         Nothing
           -> IMap.insert serviceId (IMap.singleton unitId [assignment]) map1
         Just map2
           -> IMap.insert serviceId (IMap.insertWith (addToHead) unitId [assignment] map2) map1
 
-- | Return an error in case ServiceParams does not respect code contracts.
serviceParams_respectCodeContracts :: ServiceParams -> Either String ()
serviceParams_respectCodeContracts s
  = do              
       extractAndCheckReverseOrder "(err 1075) prices are not in reverse order of date" (servicePrice_fromDate) (serviceParams_prices s)
       extractAndCheckReverseOrder2 "(err 1076) service assignmentes are not in reverse order of date" (assignedService_fromDate) (serviceParams_assigned s)

       return ()

 where

  extractAndCheckReverseOrder msgError extract map1
    = let l1 = List.map snd $ IMap.toAscList map1
          l2 = List.map (List.map extract) l1 
          l3 = List.map isDescendingOrder l2
      in  unless (List.all id l3) (fail msgError) 
  
  extractAndCheckReverseOrder2 msgError extract map1
    = let l1 = List.map snd $ IMap.toAscList map1
      in  Control.Monad.mapM_ (extractAndCheckReverseOrder msgError extract) l1

--
-- Generate Service CDRs
--

data CalcService
  = CalcService {
      calcService_price :: MonetaryValue
    , calcService_nrOfItems :: Int
    , calcService_timeFrame :: TimeFrame
    , calcService_discount :: Rational
    } deriving(Show)

-- | Generate all ServiceCDRs until the calldate is not reached.
--   Return also the ServiceCDRs in open timeframes.
--
--   DEV-NOTE: there are not so many CDRs of this type (proportional to customers), so they are generated using a list, and not pipes.   
service_generate
  :: Bool
  -> RatingEnv 
  -> ServiceParams
  -> CallDate
  -- ^ the initial rating call date (it can be not exactly the beginning of a time-frame)
  -> CallDate
  -- ^ the final rating calldate (it can be not exactly the beginning of a time-frame)
  -> Bool
  -- ^ True for generating also ServiceCDRs of open time frames
  -> Either String [ServiceCDR]

service_generate isDebugMode env serviceParams rateFromDate rateToDate generateAlsoForOpenTimeFrames
  = cdrsOrError
     
 where

  precision = params_currencyPrecision $ env_params env
   
  cdrsOrError = IMap.foldlWithKey' processServiceInAllTimeFrames (Right []) (serviceParams_assigned serviceParams)

  processServiceInAllTimeFrames :: Either String [CDR] -> Int -> UnitIdMap [AssignedService] -> Either String [CDR]
  processServiceInAllTimeFrames (Left err) _ _  = Left err
  processServiceInAllTimeFrames (Right cdrs1) serviceId assignedServices 
    = let service = fromJust1 ("s1: unknown serviceId " ++ show serviceId ++ ", on data: " ++ show serviceParams) $ IMap.lookup serviceId (serviceParams_services serviceParams)
          serviceSchedule = service_schedule service

          allTimeFrames
            = timeFrames_allInsideRatingPeriod serviceSchedule rateFromDate rateToDate generateAlsoForOpenTimeFrames
    
          cdrs2OrError = List.foldl' extractError (Right []) $ List.map processServiceInTimeFrame allTimeFrames

          extractError (Left err) _ = Left err
          extractError (Right r1) (Left err) = Left err
          extractError (Right r1) (Right r2) = Right $ r1 ++ r2
                         
          processServiceInTimeFrame :: TimeFrame -> Either String [CDR]
          processServiceInTimeFrame (timeFrameStart2, timeFrameEnd2) 
            = let cdrs3 = IMap.foldlWithKey' processUnit  (Right []) assignedServices
                  timeFrameDuration2 = timeFrame_duration (timeFrameStart2, timeFrameEnd2)
          
                  processUnit :: Either String [CDR] -> UnitId -> [AssignedService] -> Either String [CDR]
                  processUnit (Left err) _ _  = Left err
                  processUnit (Right cdrs4) unitId assignements 
                    = let -- DEV-NOTE: this code is based on fact that assignments are in reverse order of applicability date. 

                          calculatedServicesOrErrors = processAssignment assignements ([], Nothing)

                          calculatedServices = case calculatedServicesOrErrors of
                                                 Right (r, _) -> r
                                                 _ -> error "never used"
                          
                          calculatedCdrs = List.concatMap calcService calculatedServices

                          processAssignment :: [AssignedService] -> ([CalcService], Maybe CallDate) -> Either String ([CalcService], Maybe CallDate)
                          processAssignment [] result = return result
                          processAssignment (assignement:rest) (calcs1, maybePartialDate) 
                            = let items = assignedService_nrOfItems assignement
                                  assignmentCallDate = assignedService_fromDate assignement

                                  -- The reference date to use for searching in the price-list
                                  priceListDate
                                    = case service_priceChangeWithPriceList service of
                                        True
                                          -> max assignmentCallDate timeFrameStart2
                                             -- consider the price at the moment of activation or rating: it is the more precise price
                                        False
                                          -> -- In this case the price is the price paid from the customer the first time he activated the service.
                                             -- In case the service was set to 0 in the past, and then activated again, consider the price of the new activation.
                                             let f resultCallDate [] = resultCallDate
                                                 f resultCallDate (ass:rest)
                                                   = case (assignedService_fromDate ass) >= assignmentCallDate of
                                                       True
                                                         -> f resultCallDate rest
                                                            -- discard assignment in the future respect current assignment
                                                       False
                                                         -> case assignedService_nrOfItems ass of
                                                              0 -> resultCallDate
                                                                   -- in this case the previous used prices can be discarded, and the new price is determined from this point
                                                              _ -> f (assignedService_fromDate ass) rest
                                                                   -- go to the first assignment (the first price used)
 
                                             in f assignmentCallDate assignements 

                                  -- The price of the service, using priceListDate as reference
                                  maybePrice :: Maybe MonetaryValue 
                                    = let f [] = Nothing
                                          f (price:rest)
                                            = case (servicePrice_fromDate price) <= priceListDate of
                                                True -> Just $ servicePrice_price price
                                                False -> f rest

                                      in case IMap.lookup serviceId (serviceParams_prices serviceParams) of
                                           Nothing 
                                             -> Nothing

                                           Just prices 
                                             -> f prices
                                                -- search the more recent price (according priceListDate) in the prices list 

                                  calc2 price
                                    = CalcService {
                                        calcService_nrOfItems = items
                                      , calcService_price = price
                                      , calcService_discount = assignedService_discount assignement
                                      , calcService_timeFrame 
                                          = (case service_priceIsProportionalToActivationDate service of
                                               True -> max assignmentCallDate timeFrameStart2
                                               False -> timeFrameStart2
                                            , case maybePartialDate of
                                                Nothing -> timeFrameEnd2
                                                Just d -> case service_priceIsProportionalToActivationDate service of
                                                            True -> min d timeFrameEnd2
                                                            False -> timeFrameEnd2
                                            )
                                      }


                                  -- Test if the current assignment can be applied in the current time frame, 
                                  -- and if there can be results continuing processing.
                                  (canBeApplied, continueProcessing) :: (Bool, Bool)
                                    = case service_isAppliedOnlyOneTime service of
                                        True
                                          -> ((assignmentCallDate >= timeFrameStart2 && assignmentCallDate < timeFrameEnd2), True)
                                             -- process only the assignment in the first time-frame
                                        False
                                          -> case assignmentCallDate >= timeFrameEnd2 of
                                               True
                                                 -> (False, True)
                                                    -- this assignment was done in the future respect current time frame, and it does not affect it
                                               False
                                                 -> case maybePartialDate of
                                                      Nothing
                                                        -> (True, True)
                                                           -- the assignment is in the past, but it is still actual in the current time frame
                                                      Just partialDate
                                                        -> case partialDate <= timeFrameStart2 of
                                                             True
                                                               -> (False, False)
                                                                  -- the assignment is in the past respect the timeframe, and already overwritten from a more recent assignment
                                                             False
                                                               -> (True, True)
                                                                  -- the assignment is in the past, but it extend also to the current time-frame, until a more recent assignment take effect 
 
                                  nextPartialDate 
                                    = case canBeApplied of
                                        True -> Just assignmentCallDate
                                        False -> Nothing

                              in do calcs2
                                      <- case canBeApplied of
                                           False 
                                             -> return calcs1
                                           True
                                             -> do price 
                                                     <- case maybePrice of
                                                          Nothing -> throwError $ "There is no price for service " ++ (show $ service_id service) ++ ", " ++ (show $ service_name service) ++ ", at date " ++ showLocalTime priceListDate ++ ", in its price list."    
                                                          Just p -> return p

                                                   return $ (calc2 price):calcs1 
                                                   case service_priceIsProportionalToActivationDate service of
                                                     True -> return $ (calc2 price):calcs1
                                                             -- in case of prices depending from the activation date,
                                                             -- keep note of all different activations, and sum them.

                                                     False -> let maxItems = List.maximum $ items:(List.map calcService_nrOfItems calcs1)
                                                              in return $ [(calc2 price) { calcService_nrOfItems = maxItems } ]
                                                              -- in case of prices not depending from activation date,
                                                              -- keep note of the maximum number of activated items in the time frame period,
                                                              -- using the price of the first activation in the time-frame.

                                    case continueProcessing of
                                      False
                                        -> return $ (calcs2, Just assignmentCallDate)
                                      True
                                        -> processAssignment rest (calcs2, nextPartialDate)

                          calcService :: CalcService -> [CDR]
                          calcService c
                            = let duration = timeFrame_duration $ calcService_timeFrame c
                                  proportion
                                    = case (service_priceIsProportionalToActivationDate service) of
                                        True -> (toRational duration) / (toRational timeFrameDuration2)
                                        False -> toRational 1
                                        
                                  scaledPrice = (calcService_price c) * proportion
                                  discountSum =  scaledPrice * (calcService_discount c)
                                  income = (scaledPrice - discountSum) * (toRational $ calcService_nrOfItems c)
            
                                  externalTelephoneNumber = service_defaultExternalTelephoneNumber service 
                                  -- NOTE: the rate compilation procedure generate a telephone prefix associated to this telephone number.

                                  serviceInfo
                                    = let descr = (Text.unpack $ service_description service)
                                          maybeDescr = case List.null descr of
                                                         True -> ""
                                                         False -> " - " ++ descr
                                                         
                                      in  Text.pack $ (Text.unpack $ service_name service) ++ maybeDescr
          
                                  cdr = (cdr_empty timeFrameStart2 precision) {
                                          cdr_countOfCalls = calcService_nrOfItems c
                                        , cdr_toCalldate = Just timeFrameEnd2
                                        , cdr_direction = CDR_outgoing
                                        , cdr_errorDirection = CDR_none
                                        , cdr_isRedirect = False
                                        , cdr_duration = Just 0
                                        -- NOTE: I'm using 0 because the total of calls is already in normal calls,
                                        -- and if I set a value here, it will be added two times to the totals in call report.
                                        , cdr_billsec = Just 0
                                        , cdr_internalTelephoneNumber = "" 
                                        , cdr_organizationUnitId = Just $ unitId
                                        , cdr_bundleOrganizationUnitId = Nothing
                                        , cdr_income = income
                                        , cdr_cost = 0               
                                        , cdr_channel = Just serviceCdr_defaultCommunicationChannel
                                        , cdr_externalTelephoneNumber =  externalTelephoneNumber
                                        , cdr_externalTelephoneNumberWithAppliedPortability = Just externalTelephoneNumber
                                        -- DEV NOTE: this number is matched with the telephone prefix-table, so use a symbolic name
                                        , cdr_displayedExternalTelephoneNumber = Just $ serviceInfo
                                        , cdr_displayedMaskedExternalTelephoneNumber = Just $ serviceInfo
                                        , cdr_debug_bundle_left_calls = Nothing 
                                        , cdr_debug_bundle_left_duration = Nothing
                                        , cdr_debug_bundle_left_cost = Nothing                               
                                        }
                                        
                              in if income == 0
                                 then []
                                 else [cdr]

                      in case calculatedServicesOrErrors of
                           Left err
                             -> Left err
                           Right _
                             -> Right $ cdrs4 ++ calculatedCdrs
                         
              in cdrs3

      in case cdrs2OrError of
           Left err
             -> Left err
           Right cdrs2
             -> Right $ cdrs1 ++ cdrs2

service_defaultExternalTelephoneNumber :: Service -> Text.Text
service_defaultExternalTelephoneNumber service
  = serviceCdr_defaultExternalTelephoneNumber (service_name service) (service_description service) 

-- | Export the telephone prefixes to use for service-cdrs.
--   These values will be imported from PHP world, and used during rating phase for describing the produced CDRs.
service_exportToCSVTheServiceCDRSTelephonePrefixes :: ServiceParams -> BS.ByteString
service_exportToCSVTheServiceCDRSTelephonePrefixes mp 
  = CSV.encode $ List.map f $ List.map snd $ IMap.toList $ serviceParams_services mp
 where

  f :: Service -> CSVFormat_TelephonePrefix1
  f p
    = let sName = service_name p
          sType = service_description p
      in CSVFormat_TelephonePrefix1 {
              t1_prefix = service_defaultExternalTelephoneNumber p
            , t1_matchOnlyExactDigits = 1
            , t1_name = sName
            , t1_geographic_location = sName
            , t1_operator_type = "Service"
            , t1_display_priority = 10
           }

--
-- Unit Tests
--
  
tt_servicesTests

  = [ isExpectedResult
        "simple case 1"
        cdrs1_1
        [(fromJust $ fromMySQLDateTimeToLocalTime "2014-02-01 00:00:00", 1, toRational 20)]
    , isExpectedResult
        "simple case 2"
        cdrs1_2
        [(fromJust $ fromMySQLDateTimeToLocalTime "2014-02-01 00:00:00", 1, toRational 20)
        ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-03-01 00:00:00", 1, toRational 20)  
        ]

    , isExpectedResult
        "simple case 3"
        cdrs1_3
        []
        
    , isExpectedError "simple case 4" error2
    
    , isExpectedResult "complex case 1" cdrs3_1 expected3_1
    , isExpectedResult "complex case 2" cdrs4_1 expected4_1      
    ]

 where

  precisionDigits = 4
   
  env = env_empty $ initialClassificationParams_empty { params_currencyPrecision = precisionDigits }

  serviceMap :: [Service] -> ServiceIdMap Service
  serviceMap ss = IMap.fromList $ List.map (\s -> (service_id s, s)) ss

  priceListMap :: [ServicePrice] -> ServiceIdMap [ServicePrice]
  priceListMap ss
    = let sameService s1 s2 = (servicePrice_serviceId s1) == (servicePrice_serviceId s2)
          l1 = List.groupBy sameService ss
          l2 = List.map (\(e:r) -> ((servicePrice_serviceId e), (e:r))) l1
          l3 = List.map (\(i, l) -> (i, List.reverse $ List.sortBy (\x y -> compare (servicePrice_fromDate x) (servicePrice_fromDate y)) l)) l2 
      in  IMap.fromList l3

  assignmentMap :: [AssignedService] -> ServiceIdMap (UnitIdMap [AssignedService])
  assignmentMap ss
    = let ss1 = List.foldl' serviceParams_insertAssignment IMap.empty ss
          ss2 = IMap.map (\m -> IMap.map (\s -> List.reverse $ List.sortBy (\x y -> compare (assignedService_fromDate x) (assignedService_fromDate y)) s) m) ss1
      in ss2

  generate p fromDate toDate
    = service_generate True env p (fromJust1 "sa1" $ fromMySQLDateTimeToLocalTime fromDate) (fromJust1 "sa2" $ fromMySQLDateTimeToLocalTime toDate) True

  isExpectedResult
    :: String
    -- ^ test title   
    -> [ServiceCDR]
    -- ^ calculated result   
    -> [(CallDate, UnitId, MonetaryValue)]
    -- ^ expected result   
    -> HUnit.Test
 
  isExpectedResult msg c1 e1
    = let extract s = ( cdr_calldate s
                      , fromJust1 "sa3" $ cdr_organizationUnitId s
                      , cdr_income s
                      )  

          c2 = List.sort $ List.map extract c1
          e2 = List.sort e1
          
      in  HUnit.TestCase $ HUnit.assertEqual msg e2 c2     
  
  isExpectedError :: String -> Either String a -> HUnit.Test
  isExpectedError msg a
    = let f (Left _) = True
          f _ = False
      in  HUnit.TestCase $ HUnit.assertBool msg (f a)

  s1
    = Service {
         service_id = 1
       , service_name = "1"
       , service_description = ""                 
       , service_priceIsProportionalToActivationDate = False
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1
       }

  sp1
    = ServicePrice {
          servicePrice_id = 1
        , servicePrice_serviceId = service_id s1
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2013-01-01 00:00:00"
        , servicePrice_price = toRational 10                  
        }

  unit1 = 1
  
  sa1
    = AssignedService {
        assignedService_id = 1
      , assignedService_serviceId = service_id s1
      , assignedService_unitId = unit1
      , assignedService_nrOfItems = 2
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-01-01 00:00:00"
      , assignedService_discount = fromRational 0
     }

  p1
    = ServiceParams {
        serviceParams_services = serviceMap [s1]
      , serviceParams_prices = priceListMap [sp1]
      , serviceParams_assigned = assignmentMap [sa1]   
      }

  (Right cdrs1_1)
    = generate p1 "2014-02-01 00:00:00" "2014-03-01 00:00:00"

  (Right cdrs1_2)
    = generate p1 "2014-02-01 00:00:00" "2014-04-01 00:00:00"

  (Right cdrs1_3)
    = generate p1 "2013-11-01 00:00:00" "2013-12-01 00:00:00"

  -- do not specify a needed price list
  sp2
    = ServicePrice {
          servicePrice_id = 1
        , servicePrice_serviceId = service_id s1
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-01-15 00:00:00"
        , servicePrice_price = toRational 10
        }
  
  sa2
    = AssignedService {
        assignedService_id = 1
      , assignedService_serviceId = service_id s1
      , assignedService_unitId = unit1
      , assignedService_nrOfItems = 2
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-01-03 00:00:00"
      , assignedService_discount = fromRational 0
     }

  p2
    = ServiceParams {
        serviceParams_services = serviceMap [s1]
      , serviceParams_prices = priceListMap [sp2]
      , serviceParams_assigned = assignmentMap [sa2]   
      }
  
  error2
    = generate p2 "2014-01-01 00:00:00" "2014-02-01 00:00:00"

  -- Test different combinations of services, with different parameters.
  -- I'm using a month with 30 days for having round values.

  s3_1
    = Service {
         service_id = 31
       , service_name = "3_1"
       , service_description = "Service depend from activation date."                 
       , service_priceIsProportionalToActivationDate = True
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1
       }

  s3_2
    = Service {
         service_id = 32
       , service_name = "3_2"
       , service_description = "Service price change with time."                 
       , service_priceIsProportionalToActivationDate = False
       , service_priceChangeWithPriceList = True
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1
       }

  s3_3
    = Service {
         service_id = 33
       , service_name = "3_3"
       , service_description = "Service price change with time, and depend from activation date."                 
       , service_priceIsProportionalToActivationDate = True
       , service_priceChangeWithPriceList = True
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1
       }

  s3_4
    = Service {
         service_id = 34
       , service_name = "3_4"
       , service_description = "Service is applied only one time."                 
       , service_priceIsProportionalToActivationDate = True
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = True
       , service_schedule = MonthlyTimeFrame 1
       }

  s3_5
    = Service {
         service_id = 35
       , service_name = "3_5"
       , service_description = "Weekly service."                 
       , service_priceIsProportionalToActivationDate = False
       , service_priceChangeWithPriceList = False
       , service_isAppliedOnlyOneTime = False
       , service_schedule = MonthlyTimeFrame 1
       }

  generateCommonSp1 serviceId
    = [
        ServicePrice {
          servicePrice_id = 1
        , servicePrice_serviceId = serviceId
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
        , servicePrice_price = toRational 50
        }
        
      , ServicePrice {
          servicePrice_id = 2
        , servicePrice_serviceId = serviceId
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-20 00:00:00"
        , servicePrice_price = toRational 100
        }

      , ServicePrice {
          servicePrice_id = 3
        , servicePrice_serviceId = serviceId
        , servicePrice_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-05-16 00:00:00"
        , servicePrice_price = toRational 200
        }
      ]

  sa3_1
    = AssignedService {
        assignedService_id = 1
      , assignedService_serviceId = service_id s3_1
      , assignedService_unitId = 1
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  sa3_2
    = AssignedService {
        assignedService_id = 2
      , assignedService_serviceId = service_id s3_4
      , assignedService_unitId = 4
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  sa3_3
    = AssignedService {
        assignedService_id = 3
      , assignedService_serviceId = service_id s3_2
      , assignedService_unitId = 2
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  sa3_4
    = AssignedService {
        assignedService_id = 4
      , assignedService_serviceId = service_id s3_3
      , assignedService_unitId = 3
      , assignedService_nrOfItems = 1
      , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-16 00:00:00"
      , assignedService_discount = fromRational 0.1
     }

  p3
    = ServiceParams {
        serviceParams_services = serviceMap [s3_1, s3_2, s3_3, s3_4, s3_5]
      , serviceParams_prices = priceListMap $ (List.concatMap generateCommonSp1 $ List.map service_id $ [s3_1, s3_2, s3_3, s3_4, s3_5])
      , serviceParams_assigned = assignmentMap [sa3_1, sa3_2, sa3_3, sa3_4]   
      }
  
  (Right cdrs3_1)
    = generate p3 "2014-04-01 00:00:00" "2014-06-01 00:00:00"

  expected3_1
    = [(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 1, toRational 22.5)
      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 1, toRational 45)
       -- price proportional to activation date, and with discount applied
       -- next month there is a full activation time frame
       -- next month the old price list is applied

      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 3, toRational 22.5)
      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 3, toRational 90) 
       -- next month there is a full activation date
       -- next month the new price list is appliedy

      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 4, toRational 22.5)
       -- it is applied only one month

      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 2, toRational 45)
      ,(fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 2, toRational 90) 
       -- use the new price for calculating the cost in the second time frame
      ]

  -- Test multiple assignments of a price in the same timeframe

  p4
    = ServiceParams {
        serviceParams_services = serviceMap [s3_1, s3_2, s3_3, s3_4, s3_5]
      , serviceParams_prices = priceListMap $ (List.concatMap generateCommonSp1 $ List.map service_id $ [s3_1, s3_2, s3_3, s3_4, s3_5])
      , serviceParams_assigned
          = assignmentMap [
              AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 1
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 1
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

            -- test assignment to 0 
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 11
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 11
              , assignedService_nrOfItems = 0
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_1
              , assignedService_unitId = 11
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00"
              , assignedService_discount = fromRational 0
              }

              -- use a service not proportional, but the price that is changing
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_2
              , assignedService_unitId = 2
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_2
              , assignedService_unitId = 2
              , assignedService_nrOfItems = 2
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }
              
              -- proportional timeframe and variable price list
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_3
              , assignedService_unitId = 3
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_3
              , assignedService_unitId = 3
              , assignedService_nrOfItems = 2
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

              -- no proportional timeframe and no variable price list
            , AssignedService {
                assignedService_id = 1
              , assignedService_serviceId = service_id s3_5
              , assignedService_unitId = 5
              , assignedService_nrOfItems = 1
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00"
              , assignedService_discount = fromRational 0
              }
            , AssignedService {
                assignedService_id = 2
              , assignedService_serviceId = service_id s3_5
              , assignedService_unitId = 5
              , assignedService_nrOfItems = 2
              , assignedService_fromDate = fromJust $ fromMySQLDateTimeToLocalTime "2014-04-22 00:00:00"
              , assignedService_discount = fromRational 0
              }

            ]

      }
  
  (Right cdrs4_1)
    = generate p4 "2014-04-01 00:00:00" "2014-06-01 00:00:00"

  expected4_1
    = [(fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 1, toRational 35)
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 1, toRational 15)
       -- combine two time frames, but with fixed price
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 1, toRational 50)
       -- entire time frame, using the original price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 11, toRational 35)
      -- rate until units are not 0

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 11, toRational 100)
      -- recognize a setting to 0 of a service, and start with the new price
        
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 2, toRational 100)
        -- consider 2 items on the entire time frame, but using the last price
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 2, toRational 200)
        -- consider 2 items using the new price
        
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 3, toRational 35)
        -- consider 1 item
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 3, toRational 60)
        -- consider 2 items using the new price, and proportional timeframe
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 3, toRational 200)
        -- consider 2 items using the new price

      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-04-01 00:00:00", 5, toRational 100)
        -- consider 2 items using the initial price
        
      , (fromJust $ fromMySQLDateTimeToLocalTime "2014-05-01 00:00:00", 5, toRational 100)
        -- consider 2 items using the old price

      ]
