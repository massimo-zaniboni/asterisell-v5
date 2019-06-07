{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, DeriveGeneric, DeriveAnyClass  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Import organization info from external sources.
--   These are usually customizations for specific customers.
--   They are still released as shared source code, because:
--   * the code can be useful also for others
--   * it is no private info, but only DB schema, and other not so important things
--   * (frankly) up to date I can not manage in Fabric customer specific Haskell modules, but the RatingEngine must be a unique executable
--
-- Design constraints that this code should respect:
-- * maintain history of changes of price categories, on the Asterisell side
-- * do not generate too much history
-- * do not generate too much rerating events
--
module Asterisell.CustomOrganizationInfoImporters (
   itec1_synchro
) where

import Asterisell.DB
import Asterisell.Error
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.Params
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.Cdr
import Asterisell.RateEngine
import Asterisell.RatePlan

import Data.List as L
import qualified Data.Set as Set
import qualified Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as IntSet
import Data.Char
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy as LT
import qualified Data.Text as Text
import Data.Monoid
import Data.Maybe
import Data.IORef
import Control.Monad
import Data.Hash.MD5 as MD5
import Data.Hashable
import Data.Time.LocalTime
import Control.Monad.Except (ExceptT, throwError, lift, runExceptT)
import Control.Monad.IO.Class
import qualified Control.DeepSeq as DeepSeq
import Data.Foldable
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import Data.Csv as CSV
import qualified Data.Vector as V
import Control.Exception.Assert.Sugar
import Debug.Trace

import GHC.Generics
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)

-- -------------
-- Itec1 project

type ProcessedUnitIds = Set.Set UnitId

-- | Replace spaces with "-" inside an identifier, for transforming it in a legal internal name.
internalName_sanityze :: Text.Text -> Text.Text
internalName_sanityze t1 = Text.map (\c -> if c == ' ' then '-' else toLower c) t1

-- | A trust level from 0 (minimum trust) to unspecified bigger Int.
--   An higher trust level signifies that the data can be used for importing,
--   instead of data with lower trust level.
type TrustLevel = Int

-- | Return an excetpion in case of error,
--   an exception with Nothing in case of an ignore,
--   or a value in the IO.
type ImportMonad = ExceptT (Maybe AsterisellError) IO

itec1_errorGarbageKey :: DataSourceName -> BS.ByteString
itec1_errorGarbageKey dsn = BS.concat ["org-synchro-", fromTextToByteString dsn]

-- | Import customers.
--   Add customer problems in the error table, and not import the specified customer.
--   Throw an exception (without writing in the error table) only in case of critical problems, and not import all customers.
--
--   This algo works assuming that this process/function is executed for each external data source provider.
--   So it is an iterative import of data.
--
--   Put in `unit_internalName` the CRM code of organization, that is also its primary name. It is taken from remote "account" field.
--   Put it `unit_internalChecksum4` the DataSourceName (Provider) from which the organization is imported
--   Put in `unit_internalChecksum1` the checksum of fields needing a rewrite of the history.
--   Put in `unit_internalChecksum2` the checksum of fields needing a simple update of current informations.
--   Put in `unit_internalChecksum3` a BS.ByteString with the TrustLevel of the imported organization.
--   Put in `unit_internalChecksum5` info about the source of the info (infoFromProvider or infoFromCRMContract)
--
--   The complete specification of the conversion is the code, but from an high-level point of view:
--   * import users with login access if the fields ... are specified on the remote side
--   * associate a partyTag according the fields ...
--   * a party/customer can be defined in more than one external data sources, so try to use/prefer the data source with active info
--   * remote table ``cc_tariffgroup`` contains info about price-categories
--   * remote table ``cc_card`` contains info about customers, according fields described in `itec1_tfields``
--   * ignore `ZZZ*` accounts, because it is  test accounts, and they are specified in the initialization procedure
--   * if a new customer has a LegacyRate on A2Billing, then apply the rate specified in the itec1_crmToContractRate file instead
--   * if a new customer has a new rate, first apply (if exists) the itec1_crmToContractRate, and then the new rate in the current billing time frame
--
--   DEV-NOTE: if you change the logic of this code, update also the `CustomerSpecificImporters.itec1_` related code.
--
itec1_synchro
    :: DBConf
    -> DB.ConnectInfo
    -> Text.Text -- ^ organization to ignore
    -> CurrencyPrecisionDigits
    -> DataSourceName
    -> CmdParams
    -> IO ()
itec1_synchro localDBConf remoteDBConf organizationToIgnore currencyInfo dataSourceName cparams = do
  withResource'
    (do
        localConn <- db_openConnection localDBConf False
        db_openTransaction localConn
        remoteConn <- DB.connect remoteDBConf
        return (localConn, remoteConn)
    )
    (\(localConn, remoteConn) -> do

          -- start with an empty transaction because a new one will be created for every import
          db_garbagePastErrors localConn (itec1_errorGarbageKey dataSourceName) Nothing Nothing
          db_commitTransaction localConn

          --
          -- Before importing customers, load local data
          --
          -- DEV-NOTE: use DeepSeq for forcing the read of  the data, before writing the new data into the DB

          remoteLegacyRates <- itec1_legacyRates "data_files/old-legacy-price-categories.csv"
          crmToContractRate <- itec1_crmToContractRate "data_files/itec-contracts.csv"

          (_, !localUnitType) <- DeepSeq.force <$> unitType_load localConn False
          let !extensionUnitTypeId = DeepSeq.force $ unitType_defaultId localUnitType UnitType_extension
          !partyTagInfo <- DeepSeq.force <$> partyTagInfo_load localConn False
          !userRoleInfo <- DeepSeq.force <$> userRole_load localConn False
          (!fromRemoteToLocalRateId, !remoteLegacyRateIds, !maybeTestAccountRateId) <-
              DeepSeq.force <$> synchroPriceCategories localConn remoteConn remoteLegacyRates
          !unbilledCallsFromDate <- defaultParams_unbilledCallsFrom localConn "02676"

          --
          -- Process all data on remote database
          --

          (localRateCategories :: Map.Map RateCategoryCode RateCategoryId, _) <- rateCategories_load localConn False

          remoteInfo <- liftIO $ tfields_results remoteConn (TFields "cc_card" itec1_tfields) Nothing

          parentIdForExtensionsToIgnore <- extensionToIgnoreId localConn organizationToIgnore
          partyTagInfoRef <- newIORef partyTagInfo
          newExtensionsR <- newIORef Set.empty
          newExtensionsToIgnoreR <- newIORef Set.empty
          processedAccountsR <- newIORef Set.empty

          updateOrganizationsS <-
              S.makeOutputStream $
               updateOrganizations
                 localConn
                 parentIdForExtensionsToIgnore
                 userRoleInfo
                 localUnitType
                 localRateCategories
                 fromRemoteToLocalRateId
                 remoteLegacyRateIds
                 crmToContractRate
                 maybeTestAccountRateId
                 unbilledCallsFromDate
                 partyTagInfoRef
                 newExtensionsR
                 newExtensionsToIgnoreR
                 processedAccountsR

          S.connect remoteInfo updateOrganizationsS
          -- start the importing processing, connecting the input stream (data) with the output stream (actions)

          db_openTransaction localConn

          -- Add new extensions to ignore
          newExtensionsToIgnore <- readIORef newExtensionsToIgnoreR
          mapM_
            (\newExtension -> do
                let unitRec
                      = trecord_setMany
                          (trecord_empty)
                          [ ("automatically_managed_from", toDBInt64 1) ]

                unitId <- trecord_save localConn "ar_organization_unit" unitRec Nothing

                let recStructure
                      = trecord_setMany
                          (trecord_empty)
                          [("ar_organization_unit_id", toDBInt64 unitId)
                          ,("ar_organization_unit_type_id", toDBInt64 extensionUnitTypeId)
                          ,("ar_parent_organization_unit_id", toDBInt64 parentIdForExtensionsToIgnore)
                          ,("from", toDBLocalTime unbilledCallsFromDate)
                          ,("exists", toDBBool True)
                          ,("ar_party_id", DB.MySQLNull)
                          ,("ar_rate_category_id", DB.MySQLNull) -- NOTE: the same of the parent
                          ,("extension_codes", toDBText newExtension)
                          ]


                _ <- trecord_save localConn "ar_organization_unit_has_structure" recStructure Nothing

                putStrLn $ "Added extension to ignore: " ++ Text.unpack newExtension

                return ()
            ) newExtensionsToIgnore

          -- Disable extensions to ignore, if now they are active and imported.
          -- NOTE: it suffices to check only new found extensions.
          newExtensions <- readIORef newExtensionsR
          mapM_
            (\newExtension -> do
                   maybeExtensionRec
                     <- tfields_result
                           localConn
                           organizationUnitStruct_tfields
                           (Just ( "extension_codes = ? AND ar_parent_organization_unit_id = ? ORDER BY `from` DESC LIMIT 1"
                                 , [toDBText newExtension, toDBInt64 parentIdForExtensionsToIgnore]))

                   case maybeExtensionRec of
                     Nothing -> return ()
                     Just extensionRec -> do
                       info_notIgnoreAnymoreThisExtension
                         localConn
                         (Just parentIdForExtensionsToIgnore)
                         (fromDBInt $ trecord "ar_organization_unit_id" extensionRec)

                       putStrLn $ "Extension not any more to ignore: " ++ Text.unpack newExtension

            ) newExtensions
    )
    (\isOk (localConn, remoteConn) -> do
         DB.close remoteConn
         db_releaseResource isOk localConn
         return ())
    (\exc -> SomeException $ AsterisellException $ "Unrecoverable error during organization synchro with " ++ Text.unpack dataSourceName ++ ": exception " ++ show exc)

 where

  -- | False for ignoring the customer.
  --   NOTE: CDRS of ignored customers are recognized, and imported as CDR to ignore, so they are silently deleted from the DB.
  isCustomerToImport :: TRecord -> Bool
  isCustomerToImport remoteInfo
    = let ri1 = fromMaybeDBValue fromDBText $ trecord "debitorder" remoteInfo
          ri = case ri1 of
                 Nothing -> Nothing
                 Just s -> Just $ Text.map toUpper s

      in True
      -- import all customers
      -- (ri == Just "IVOICE" || ri == Just "VOICEGATE")
      -- ignore these users, according https://git.asterisell.com/rolf/itec-voicegate/issues/6#issuecomment-3039
      -- and https://git.asterisell.com/rolf/itec-voicegate/issues/4

  -- | Map all remote price categories to local one, creating them if they are missing.
  --   Return also the RemoteLegacyPriceCategoryIds.
  synchroPriceCategories
      :: DB.MySQLConn
      -> DB.MySQLConn
      -> Set.Set RateCategoryCode
      -> IO ( IMap.IntMap Int
              -- ^ from local RateCategoryCode to the local RateCategoryId
             , IntSet.IntSet
               -- ^ remote legacy rate categories that are not used anymore on Asterisell side
             , Maybe Int
               -- ^ the ID of the local price-category "ignored" associated to 0 rating customers
             )
  synchroPriceCategories localConn remoteConn remotePriceCategories = do
    !(rateCategories, _) <- rateCategories_load localConn False
    (_, remotePricesS) <- DB.query_ remoteConn "SELECT id, tariffgroupname FROM cc_tariffgroup"
    (r1, r2) <- S.foldM (synchroPriceCategory localConn rateCategories remotePriceCategories) (IMap.empty, IntSet.empty) remotePricesS
    return (r1, r2, Map.lookup "ignored" rateCategories)

  -- | Import a single price-category.
  synchroPriceCategory
      :: DB.MySQLConn
      -> Map.Map RateCategoryCode RateCategoryId
         -- ^ from local RateCategoryCode to the local RateCategoryId
         --   These are the values before the synchro phase.
      -> Set.Set RemoteRateCategoryCode
         -- ^ remote legacy rate categories that are not used anymore on Asterisell side
      -> ( IMap.IntMap Int -- ^ from RemotePriceCategoryId to LocalPriceCategoryId
         , IntSet.IntSet  -- ^ the RemotedIds of LegacyPriceCategories
         )
      -> [DB.MySQLValue]
         -- ^ a single record, with remote price-category-id, and the remote name
      -> IO ( IMap.IntMap Int -- ^ from RemotePriceCategoryId to LocalPriceCategoryId, but only for new found mappings
            , IntSet.IntSet   -- ^ the RemotedIds of LegacyPriceCategories
            )
  synchroPriceCategory localConn rateCategories remotePriceCategories (map1, set1) [rrateId, rrateName] = do
    let remoteRateName = fromDBText rrateName
    let rateName = internalName_sanityze remoteRateName
    let rateKey = fromTextToByteString rateName
    let remoteId = fromDBInt rrateId

    let !isLegacy = Set.member remoteRateName remotePriceCategories
    let !set2 = if isLegacy then (IntSet.insert remoteId set1) else set1

    map2 <-
      case (isLegacy, Map.lookup rateName rateCategories) of
        (_, Just localRateId) -> do
          return $ IMap.insert remoteId localRateId map1
        (True, Nothing) -> do
          return map1
          -- NOTE: do not create remote legacy PriceCategories because they will be substitute by explicit itec1_crmToContractRate
        (False, Nothing) -> do
          let priceRec
                = trecord_setMany
                    (trecord_empty)
                    [("internal_name", toDBText rateName)
                    ,("short_description", toDBText $ Text.concat ["Rate ", rateName])
                    ]

          localRateId <- trecord_save localConn "ar_rate_category" priceRec Nothing
          putStrLn $ "Added new price-category: " ++ Text.unpack rateName
          return $ IMap.insert remoteId localRateId map1

    return (map2, set2)

  -- | Create a minimal error, that will be completed by the framework.
  createWarning'
    :: String
    -- ^ description
    -> String
    -- ^ proposed solution
    -> Maybe AsterisellError

  createWarning' d s
    = Just $ createError
               Type_Warning
               Domain_VOIP_ACCOUNTS
               "" -- NOTE: completed later
               ("Data source name: " ++ Text.unpack dataSourceName ++ "\n" ++ d)
               "" -- NOTE: completed later
               s

  -- | Create a minimal error, that will be completed by the framework.
  createError'
    :: String
    -- ^ description
    -> String
    -- ^ proposed solution
    ->AsterisellError

  createError' d s
    = createError
         Type_Error
         Domain_VOIP_ACCOUNTS
         "" -- NOTE: completed later
         ("Data source name: " ++ Text.unpack dataSourceName ++ "\n" ++ d)
         "" -- NOTE: completed later
         s

  createAppError' c
    = createError'
        ("Error " ++ c ++ ". This is an error in application code.")
        ("Contact the assistance, because it is an error in the application.")

  -- | Given a `cc_card` TRecord, returns the trust-level of the customer info inside it.
  --   Higher is the trust level, more it is likely that the customer is active.
  --   According specifications of #26
  --   It is necessary doing this, because customers can be defined on more than one remote server,
  --   managing their calls, but only one of them contains the accurate billing information
  --   (customer name, accounts, price-categories and so on).
  --
  --   NOTE: I will import only active = true and status = 1 customers according #143,
  --   but MAYBE I need also to import disactivated customers in case there are
  --   CDRS of the past to rate. So now this logic (import best customer) is
  --   virtually disactivated, but in future I can use it again.
  --
  tcard_trustLevel :: TRecord -> TrustLevel
  tcard_trustLevel trec
      = let activeCustomer = fromDBBool $ trecord "activated" trec
            customerStatus = fromMaybeDBValueToDefault 0 fromDBInt $ trecord "status" trec
            deletedCustomer = fromDBBool $ trecord "deleted" trec
            maybeFirstUseDate = fromMaybeDBValue fromDBLocalTime $ trecord "firstusedate" trec

            addToTrustLevel True tl = tl + 1
            addToTrustLevel False tl = tl

        in if activeCustomer && customerStatus == 1 then 2 else 0 

  -- | Ignore all customers with tcard_trustLevel below this level,
  --   assuming the customer is well defined in other parts.
  tcard_maxTrustLevel :: TrustLevel
  tcard_maxTrustLevel = 1
  {-# INLINE tcard_maxTrustLevel #-}

  -- | Update organization info.
  --   An action called for every remote record (it is an S.Output stream).
  --   Create only
  updateOrganizations
    :: DB.MySQLConn
    -> UnitId -- ^ organization to ignore
    -> UserRoleIdFromInternalName
    -> UnitTypeFromInternalName
    -> Map.Map RemoteRateCategoryCode RateCategoryId
    -> IMap.IntMap Int  -- ^ from remote to local rate id
    -> IntSet.IntSet -- ^ legacy RemotePriceCategoryId
    -> Map.Map CRMCode RateCategoryCode -- ^ itec1_crmToContractRate
    -> Maybe Int -- ^ the local "ignored" rate
    -> CallDate -- ^ official calldate
    -> IORef PartyTagInfo
    -> IORef (Set.Set ExtensionAsText) -- ^ new created extensions, that are not to ignore
    -> IORef (Set.Set ExtensionAsText) -- ^ new found extensions to ignore
    -> IORef (Set.Set ExtensionAsText) -- ^ processed accounts. Used for recognizing errors.
    -> Maybe TRecord
    -> IO ()

  updateOrganizations localConn parentIdForExtensionsToIgnore userRoleInfo localUnitType localRateCategories fromRemoteToLocalRateId legacyRates crmToContractRate maybeTestAccountRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR maybeRemoteInfo = do
    db_openTransaction localConn
    r <- runExceptT $ updateOrganizations1 localConn parentIdForExtensionsToIgnore userRoleInfo localUnitType localRateCategories fromRemoteToLocalRateId legacyRates crmToContractRate maybeTestAccountRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR maybeRemoteInfo
    case r of
      Left Nothing
        -> do db_rollBackTransaction localConn
              return ()
      Left (Just err)
        ->

           let err' = case maybeRemoteInfo of
                        Just remoteInfo
                          -> err {
                                   asterisellError_key
                                     = BS.concat [itec1_errorGarbageKey dataSourceName, "-customer-", fromStringToByteString $ show $ fromDBInt $ trecord "id" remoteInfo]
                                 , asterisellError_effect
                                     = "All the calls of this customer will be not rated correctly. Remote customer info: \n" ++ (fromByteStringToString $ trecord_show remoteInfo)
                                 }
                        Nothing
                          -> err {
                                   asterisellError_key
                                      = BS.concat [itec1_errorGarbageKey dataSourceName, "-missing-customers"]
                                 , asterisellError_effect
                                     = "Customers deleted from the remote database are not correctly disabled on the local Asterisell side, so there can be conflicts, and calls assigned to wrong customers."
                                 }

           in do db_rollBackTransaction localConn
                 db_openTransaction localConn
                 dbErrors_insert
                   localConn
                   Nothing
                   (err' { asterisellError_garbageKey = itec1_errorGarbageKey dataSourceName })
                   (Just (unbilledCallsFromDate, unbilledCallsFromDate))
                 db_commitTransaction localConn
                 return ()
      Right rr
        -> do db_commitTransaction localConn
              return ()

  -- | Execute an update pass, or signal an error, using `createError'` function, or ignore the importing returning Nothing.
  updateOrganizations1
    :: DB.MySQLConn
    -> UnitId -- ^ organization to ignore
    -> UserRoleIdFromInternalName
    -> UnitTypeFromInternalName
    -> Map.Map RemoteRateCategoryCode RateCategoryId
    -> IMap.IntMap Int -- ^ map remote price-categories, with local price-categories
    -> IntSet.IntSet -- ^ RemotePriceCategoryId
    -> Map.Map CRMCode RateCategoryCode -- ^ itec1_crmToContractRate
    -> Maybe Int -- ^ the "ignored" rateID
    -> CallDate -- ^ unbilledCallsFromDate
    -> IORef PartyTagInfo
    -> IORef (Set.Set ExtensionAsText)  -- ^ new inserted extensions, that are not to ignore
    -> IORef (Set.Set ExtensionAsText)  -- ^ new extensions to ignore
    -> IORef (Set.Set ExtensionAsText)  -- ^ accounts already processed, used for recognizing errors
    -> Maybe TRecord                    -- ^ Nothing when the last record is reached
    -> ImportMonad ()

  updateOrganizations1 localConn _ userRoleInfo localUnitType _ fromRemoteToLocalRateId _ _ _ unbilledCallsFromDate partyTagInfoRef _ _ _ Nothing = return ()

  updateOrganizations1 localConn parentIdForExtensionsToIgnore userRoleInfo localUnitType localRateCategories fromRemoteToLocalRateId remoteLegacyRateIds crmToContractRate maybeTestAccountRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR (Just remoteInfo0) = do

    --
    -- Read all local (Asterisell) and remote (A2Billing) data, without writing.
    -- Return with a throw in case of errors or data to ignore.
    --

    remoteInfo <- return remoteInfo0

    let trustLevel = tcard_trustLevel remoteInfo

    (accountCode :: Text.Text)
      <- let   account' = Text.strip $ fromMaybeDBValueToDefault (Text.empty) fromDBText (trecord "account" remoteInfo)
         in do when (Text.length account' == 0)
                    (throwError $ Just $ createError' "The customer has no CRM code." "Complete the info on the provider side.")
               return account'

    -- Skip test account
    when (Text.isPrefixOf "ZZZ" accountCode) (throwError Nothing)
    -- MAYBE move this code in another position

    let internalName = accountCode
    let extensionInternalName = internalName <> "-ext"
    let (_, _, addToHistoryChecksum0, infoChecksum) = trecord_internalName dataSourceName remoteInfo

    let activeCustomer = toDBBool True
    -- up to date not active customer (according the field "activated") are simply ignored in `isCustomerToImport`
    -- so all remaining customers are active.

    let userLogin' = trecord "username" remoteInfo
    let userPassword' = trecord "uipass" remoteInfo

    let maybeUserLoginAndPassword
          = case (isDBValueEmptyOrNull userLogin' || isDBValueEmptyOrNull userPassword') of
              True -> Nothing
              False -> Just (fromDBText userLogin', fromDBText userPassword')

    maybeUnitRec <- liftIO $ tfields_result localConn organizationUnit_tfields (Just ("internal_name = ?", [toDBText accountCode]))

    maybeExtensionStructRec <- liftIO $
      tfields_result
        localConn
        organizationUnitStruct_tfields
        (Just ( "extension_codes = ? AND ar_parent_organization_unit_id <> ? ORDER BY `from` DESC LIMIT 1"
              , [toDBText extensionInternalName, toDBInt64 parentIdForExtensionsToIgnore]))

    --
    -- Interrupt processing if it is an extension to ignore.
    --

    when (not $ isCustomerToImport remoteInfo) $ do
        case maybeUnitRec of
          Just _ -> throwError Nothing
                    -- there is some data source using a sane version of this extension.
                    -- NOTE: defined extensions, can never became extensions to ignore.
          Nothing -> do
            maybeExtensionToIgnoreRec <- liftIO $
                tfields_result
                   localConn
                   organizationUnitStruct_tfields
                   (Just ( "extension_codes = ? AND ar_parent_organization_unit_id = ? ORDER BY `from` DESC LIMIT 1"
                   , [toDBText extensionInternalName, toDBInt64 parentIdForExtensionsToIgnore]))

            case maybeExtensionToIgnoreRec of
              Just _ -> do
                throwError Nothing
                -- the extension is already signaled as to-ignore, or it is used in some place,
                -- overridding this setting

              Nothing -> do
                liftIO $ modifyIORef' newExtensionsToIgnoreR  (\s -> Set.insert extensionInternalName s)
                throwError Nothing

    --
    -- Interrupt processing if it is not a perfectly defined customer, as requested by Itec.
    -- NOTE: in this case the account is not signaled as to ignore, but only skipped.
    -- So it is assumed that it is correctly defined elsewhere, and if not, that there are no CDRS of this customer.
    --

    when (trustLevel < tcard_maxTrustLevel)
         (throwError Nothing)

    --
    -- Check if it is a repeated account
    --

    processedAccounts <- liftIO $ readIORef processedAccountsR
    when (Set.member accountCode processedAccounts)
         (throwError $
            createWarning'
              ("The customer with CRM code " ++ Text.unpack accountCode ++ " is defined two times on the provider " ++ (Text.unpack dataSourceName) ++ ", and both definitions are active.")
              ("Disable one of the customer definition on the provider " ++ Text.unpack dataSourceName))

    liftIO $ modifyIORef' processedAccountsR (\s -> Set.insert accountCode s)

    --
    -- Decide how to import data locally:
    -- * new data,
    -- * add to history,
    -- * fix current history,
    -- * not update info
    --
    -- The imported info is:
    -- * CRM unitId
    -- ** checksum codes
    -- ** data-source defining it, and trust level
    -- * CRM structureId
    -- ** price-category (can change)
    -- ** partyId (always the same, and copied)
    -- * partyId
    -- ** billing info and so on
    -- ** in-place updated if it change
    -- * user-login-info
    -- ** login/password
    -- ** in-place modified
    -- * extension unitId
    -- ** once created, never change
    -- * extension structureId
    -- ** contains the extension code
    -- ** once created never change
    -- * CRM to ignore
    -- ** structure-id with extension codes to ignore
    --
    -- Once a new CRM structure is created, new CRM structureId can be added only if there are changes to price-category.
    -- Other info is immutable, or in-place modified.
    --
    -- In case the customer data had to be skip, a `throwError Nothing` is raised.

    let importButRateAt0 =
          case fromMaybeDBValue fromDBText $ trecord "debitorder" remoteInfo of
            Nothing -> False
            Just s -> (Text.map toUpper s) == "TES"

    remotePriceCategoryId <-
      case fromMaybeDBValue fromDBInt (trecord "tariff" remoteInfo) of
        Just v -> return v 
        Nothing ->
          throwError $ Just $ createError' ("The customer with CRM code " ++ show accountCode ++ " has no price-category.") "Set the price-category of the customer"

    let isRemoteLegacyRate =
          (not importButRateAt0) && (IntSet.member remotePriceCategoryId remoteLegacyRateIds)

    !maybeContractRate <-
      case Map.lookup accountCode crmToContractRate of
        Nothing -> return Nothing
        Just remoteCategoryCode ->
          case Map.lookup (internalName_sanityze remoteCategoryCode) localRateCategories of
            Just i -> return $ Just (remoteCategoryCode, i)
            Nothing ->
              throwError $
                Just $
                  createError'
                    ("The price-category " ++ show remoteCategoryCode ++ " specified in the Itec CRM to BY-CONTRACT PRICE-CATEGORY file was not created during initial installation.")
                    ("Create the price-category manually, using as internal code " ++ (show $ internalName_sanityze remoteCategoryCode) ++ " or inform the assistance")

    -- Complete this info or exit with ``throwError Noting``, ignoring it
    (  maybeUnitId :: Maybe UnitId               -- ^ Nothing if new customer had to be created
     , maybePartyId :: Maybe Int                 -- ^ Nothing if new party had to be created
     , maybeStructureId :: Maybe UnitId          -- ^ Nothing if new history has to be added
     , changeFromDate :: CallDate                -- ^ add/update history info using this date of activation
     , maybeExtensionUnitId :: Maybe UnitId      -- ^ Nothing if it is a new customer
     , maybeExtensionStructureId :: Maybe UnitId -- ^ Nothing if it a new customer
     , maybeUserId :: Maybe UserId
       -- ^ Nothing for creating a new login/password user in case there is login/password
       --   Just userId for updating an existing login/password user
     , addToHistory :: Bool                -- ^ True if there is new data to add to the history of the customer, or it is a new customer
     , isNewCustomer :: Bool               -- ^ True if it is a newly created customer
     , isNewInfoFromProvider :: Bool       -- ^ False if the new PriceCategory is retrieved from itec1_crmToContractRate 
     , useFakeChecksum :: Bool) <-

       case (maybeUnitRec, maybeExtensionStructRec) of
           (Nothing, Nothing) -> do
             -- in this case we have a new customer to import

             liftIO $ modifyIORef' newExtensionsR (\s -> Set.insert extensionInternalName s)

             let startWithCRMContract = isJust maybeContractRate
             let atNextSchedulingPassUseTheA2BillingRate = not isRemoteLegacyRate

             when (isRemoteLegacyRate && (isNothing maybeContractRate))
                  (throwError $
                     Just $
                       createError'
                         ("The CRM customer code " ++ show accountCode ++ " used in A2Billing server has a legacy-rate, but he has no entry in the Itec CRM to BY-CONTRACT PRICE-CATEGORY file. Asterisell can not assign the A2Billing rate because it is a not used anymore LEGACY-RATE, but it can not use the BY-CONTRACT rate because it is unspecified in the file.")
                         ("Complete the Itec file and reinstall the application."))

             return ( Nothing, Nothing, Nothing  -- create new data
                    , fromDBLocalTime $  trecord "creationdate" remoteInfo   -- start with the date of activation of the customer
                    , Nothing, Nothing, Nothing, True, True  -- add to history all
                    , not startWithCRMContract   -- use the A2Billing info if there is no contract info
                    , atNextSchedulingPassUseTheA2BillingRate -- use a fake checksum for forcing load of info from A2Billing server at next passage
                    )

           (Just _, Nothing) ->
              pError "ERR 66753: unexpected error in the code. Contact the assistance."

           (Nothing, Just _) ->
              pError "ERR 66754: unexpected error in the code. Contact the assistance."

           (Just unitRec, Just extensionStructRec) -> do

             -- There were already imported info about this customer.
             -- So it had to decide what to do.

             let unitId = trecord "id" unitRec

             let oldDataSourceName = fromDBText $ trecord "internal_checksum4" unitRec

             let oldTrustLevel = fromJust1 "ERR 60056" $ fromTextToInt $ fromDBText $ trecord "internal_checksum3" unitRec

             let oldInfoChecksum = fromDBByteString $ trecord "internal_checksum2" unitRec

             let oldHistoryChecksum = fromDBByteString $ trecord "internal_checksum1" unitRec

             let oldInfoFrom = fromMaybeDBByteStringToMaybeEmpty $ trecord "internal_checksum5" unitRec

             let isOldInfoFromProvider = not (oldInfoFrom == infoFromCRMContract)

             when ((trustLevel == oldTrustLevel) && (oldDataSourceName /= dataSourceName))
                  (throwError
                     $ createWarning'
                         ("This customer with CRM code " ++ Text.unpack accountCode ++ " is defined also on the provider " ++ (Text.unpack oldDataSourceName) ++ ", and both are active.")
                         ("Disable this customer info, or the corresponding customer info on the provider " ++ Text.unpack oldDataSourceName))

             -- Skip processing if the trustLevel is less than previous found trustLevel.
             -- Skip processing if the data has not changed
             -- NOTE: up to date only tcard_maxTrustLevel are imported, so this code is never fired
             -- DEV-NOTE: probably this code is buggy, but it is under unit-testing, so if the maxTrustLevel condition is removed, at least unit tests will fail
             when (trustLevel < oldTrustLevel || (trustLevel == oldTrustLevel && oldInfoChecksum == infoChecksum && oldHistoryChecksum == addToHistoryChecksum0))
                  (throwError Nothing)

             structRec
               <- liftIO $ tfields_result
                             localConn
                             organizationUnitStruct_tfields
                             (Just ( "ar_organization_unit_id = ? ORDER BY `from` DESC LIMIT 1"
                                   , [unitId]))

             let partyId = trecord "ar_party_id" $ fromJust1 "ERR 1027" structRec

             let extensionStructId = fromDBInt $ trecord "id" extensionStructRec

             let extensionUnitId = fromDBInt $ trecord "ar_organization_unit_id" extensionStructRec

             maybeUserRec <- liftIO $
                 tfields_result
                    localConn
                    user_tfields
                    ( Just ( "ar_organization_unit_id = ? AND is_root_admin = 0 ORDER BY id ASC LIMIT 1"
                           , [unitId]))

             maybeUserId
                 <- case maybeUserRec of
                      Just r -> case maybeUserLoginAndPassword of
                                  Nothing -> do
                                    -- Delete the user id because it has no user or password associated.
                                    -- Deleting is better than disabling, so the username login is freed.
                                    _ <- liftIO $ DB.execute localConn "DELETE FROM ar_user_has_role WHERE ar_user_id = ?" [trecord "id" r]
                                    _ <- liftIO $ DB.execute localConn "DELETE FROM ar_user WHERE id = ?" [trecord "id" r]

                                    liftIO $ putStrLn $ "Deleted user login/password for account: " ++ Text.unpack accountCode

                                    return Nothing

                                  Just _ -> return $ Just $ fromDBInt $ trecord "id" r
                      Nothing -> return Nothing

             let structId = fromDBInt $ trecord "id" $ fromJust1 "ERR 1030" structRec

             let structFromDate = fromDBLocalTime $ trecord "from" $ fromJust1 "ERR 1031" structRec

             let (maybeStructureId, changeFromDate) =
                    case isRemoteLegacyRate of
                      True -> (Just structId, structFromDate) -- do not switch to old legacy rate
                      False ->
                        case (oldHistoryChecksum == addToHistoryChecksum0) of
                          True -> (Just structId, structFromDate) -- PriceCategory is not changed so in worst case update normal info
                          False ->
                            case (isOldInfoFromProvider && structFromDate >= unbilledCallsFromDate) of
                              True -> (Just structId, structFromDate)
                                       -- NOTE: if the structure is changed in an unbilled time-frame, simply fix the history.
                              False -> (Nothing, unbilledCallsFromDate)
                                       -- NOTE: if the structure is changed, preserve the old (already billed) info,
                                       -- and add new info to the history.

             return ( Just $ fromDBInt $ unitId
                    , Just $ fromDBInt $ partyId
                    , maybeStructureId
                    , changeFromDate
                    , Just extensionUnitId
                    , Just extensionStructId
                    , maybeUserId
                    , isNothing maybeStructureId                   -- create a new Structure for adding info to the history
                    , False                                        -- this is an already imported customer to maybe update
                    , isRemoteLegacyRate && isOldInfoFromProvider  -- do not use A2Billing info if we have still an old legacy rate on it, and mantain contract rate
                    , False                                        -- use real checksum code
                    )

    -- NOTE: if code reach this point, then there is info to update/add,
    -- otherwise ``throwError Nothing`` would be generated!

    --
    -- Write info
    --
    -- NOTE: the code process this only if there is change in data, otherwine a `throwError Nothing` is throwed in previous section

    when (isNewCustomer || addToHistory) (liftIO $ putStrLn $ "Update account: " ++ Text.unpack accountCode)

    !localPriceCategoryId <-
      case isNewInfoFromProvider of
        False ->
          -- infoFromCRMContract case
          case maybeContractRate of
            Just (remoteContractCode, i) -> return i
            Nothing ->
              throwError $
                Just $
                  createError'
                    ("The CRM customer code " ++ show accountCode ++ " used in A2Billing server has a legacy-rate, but he has no entry in the Itec CRM to BY-CONTRACT PRICE-CATEGORY file. Asterisell can not assign the A2Billing rate because it is a not used anymore LEGACY-RATE, but it can not use the BY-CONTRACT rate because it is unspecified in the file.")
                    ("Complete the Itec file and reinstall the application.")

        True ->
          -- infoFromProvider case
          case importButRateAt0 of
            True ->
                -- use a "ignored" price-category for rating all calls to 0
               case maybeTestAccountRateId of
                 Just i -> return i
                 Nothing ->
                   throwError $
                     Just $
                       createError'
                         ("The CRM customer code " ++ show accountCode ++ " used in A2Billing server is a TES customer, and so it is imported but with cost and income rate set to 0, in particular \"ignored\" price-category. But this special rate is not defined.")
                         ("This is an error in the code. Contact the assistance.")

            False -> 
              case IMap.lookup remotePriceCategoryId fromRemoteToLocalRateId of
                Nothing ->
                  throwError $
                    Just $
                      createError'
                        ("The price category with A2Billing remote id " ++ show remotePriceCategoryId ++ " used for the customer with CRM code " ++ show accountCode ++ " seems a correct rate (no LEGACY-RATE) but it was not created on Asterisell.")
                        ("This is an error in the application code. Contact the assistance.")
                Just i -> return i 

    let addToHistoryChecksum
            = if useFakeChecksum
              then (BS.append addToHistoryChecksum0 infoFromCRMContract)
              else addToHistoryChecksum0

    --
    -- Add `ar_organization_unit`
    --

    -- DEV-NOTE I do not support (up to date) hierarchical organizations, according #15,
    -- so every account is directly billable.
    -- let billTo = fromDBText $ trecord "billto" remoteInfo
    let billTo = accountCode
    (isBillable, maybeParentBillableUnitId)
       <- case (accountCode == billTo) of
             True -> return (True, Nothing)
             False -> do
               maybeBillToRec <- liftIO $
                       tfields_result
                           localConn
                           organizationUnit_tfields
                           (Just ( "internal_name = ?"
                                 , [toDBText billTo]))

               case maybeBillToRec of
                 Just billToRec -> return (True, Just $ fromDBInt $ trecord "id" billToRec)
                 Nothing
                   -> throwError $ Just $
                        createError'
                          ("The customer with CRM code " ++ (Text.unpack accountCode)
                              ++ " is not directly billable, and his calls are billed to customer with CRM code "
                              ++ (Text.unpack billTo)
                              ++ ", but this customer is not present in the system.\n")
                          ("IMPORTANT NOTE: during initial import of new customers, some of them can be imported later, and so not yet visible.\n"
                            ++ "So this can be a transient problem, solved automatically at the next execution of importing procedure.\n"
                            ++ "If the problem persist, then you had to fix the CRM code of this customer, or of his parent billable.")

    let unitRec
                   = trecord_setMany
                       (trecord_empty)
                       [("internal_name", toDBText accountCode)
                       ,("internal_checksum4", toDBText dataSourceName)
                       ,("internal_checksum1", toDBByteString addToHistoryChecksum)
                       ,("internal_checksum2", toDBByteString infoChecksum)
                       ,("internal_checksum3", toDBByteString $ fromStringToByteString $ show trustLevel)
                       ,("internal_checksum5", toDBByteString (if isNewInfoFromProvider then infoFromProvider else infoFromCRMContract))
                       ,("automatically_managed_from", toDBInt64 1)
                       ]

    unitId <- lift $ trecord_save localConn "ar_organization_unit" unitRec maybeUnitId

    -- Add `ar_party`

    let maxLimit30 = case fromDBInt (trecord "creditlimit" remoteInfo) of
                       0 -> DB.MySQLNull
                       v -> toDBInt64 $ toMonetaryValueWithFixedPrecisionInt currencyInfo (fromIntegral v)

    let country = case fromMaybeDBValue fromDBText (trecord "country" remoteInfo) of
                    Nothing -> "ZAF"
                    Just "1" -> "ZAF"
                    Just v -> v

    let recParty
          = trecord_setMany
              (trecord_empty)
              [("external_crm_code", toDBText accountCode)
              ,("is_billable", toDBBool isBillable)
              ,("contract_number", normalizeDbNull (toDBByteString "") $ trecord "contract_number" remoteInfo)
              ,("is_active",  activeCustomer)
              ,("contact_name", toDBByteString $ appendFields remoteInfo ["firstname", "lastname"])
              ,("legal_address", normalizeDbNull (toDBByteString "") $ trecord "address" remoteInfo)
              ,("legal_city", normalizeDbNull (toDBByteString "") $ trecord "city" remoteInfo)
              ,("legal_state_province", normalizeDbNull (toDBByteString "") $ trecord "state" remoteInfo)
              ,("legal_country", toDBText country)
              ,("legal_zipcode", normalizeDbNull (toDBByteString "") $ trecord "zipcode" remoteInfo)
              ,("email", normalizeDbNull (toDBByteString "") $ trecord "email" remoteInfo)
              ,("phone", normalizeDbNull (toDBByteString "") $ trecord "phone" remoteInfo)
              ,("fax", normalizeDbNull (toDBByteString "") $ trecord "fax" remoteInfo)
              ,("max_limit_30", maxLimit30)
              ,("legal_registration_number", normalizeDbNull (toDBByteString "") $ trecord "id_number" remoteInfo)
              ,("web_site", normalizeDbNull (toDBByteString "") $ trecord "company_website" remoteInfo)
              ,("vat", normalizeDbNull (toDBByteString "") $ trecord "vat_rn" remoteInfo)
              ,("name", normalizeDbNull (toDBByteString "") $ trecord "company_name" remoteInfo)
              ]

    partyId <- lift $ trecord_save localConn "ar_party" recParty maybePartyId

    -- Add `ar_party_has_tag`

    let maybeTagName = trecord "debitorder" remoteInfo
    case fromMaybeDBValue fromDBText maybeTagName of
        Nothing
          -> return ()
        Just tagName'
          -> do let tagName = Text.toUpper tagName'
                partyTagInfo <- lift $ readIORef partyTagInfoRef
                tagId
                  <- case Map.lookup tagName partyTagInfo of
                       Just i -> return $ partyTag_id i
                       Nothing -> do
                         let recTag
                               = trecord_setMany
                                   (trecord_empty)
                                   [("internal_name", toDBText tagName)
                                   ,("note_for_admin", toDBText "")
                                   ,("name_for_customer", toDBText tagName)
                                   ,("note_for_customer", toDBText "")
                                   ]

                         tagId' <- liftIO $ trecord_save localConn "ar_tag" recTag Nothing
                         let partyTag = PartyTag {
                                          partyTag_id = tagId'
                                        , partyTag_internalName = tagName
                                        , partyTag_name = tagName
                                        }

                         liftIO $ modifyIORef' partyTagInfoRef (\m -> Map.insert tagName partyTag partyTagInfo)
                         return tagId'

                lift $ DB.execute localConn "DELETE FROM ar_party_has_tag WHERE ar_party_id = ?" [toDBInt64 partyId]

                let recPartyTag
                             = trecord_setMany
                                 (trecord_empty)
                                 [("ar_party_id", toDBInt64 partyId)
                                 ,("ar_tag_id", toDBInt64 tagId)
                                 ]

                lift $ trecord_save localConn "ar_party_has_tag" recPartyTag Nothing

                return ()

    -- Add `ar_organization_unit_has_structure`

    let recStructure
          = trecord_setMany
              (trecord_empty)
              [("ar_organization_unit_id", toDBInt64 unitId)
              ,("ar_organization_unit_type_id", toDBInt64 $ unitType_defaultId localUnitType UnitType_customer)
              ,("ar_parent_organization_unit_id", toMaybeInt64 maybeParentBillableUnitId)
              ,("from", toDBLocalTime changeFromDate)
              ,("exists", activeCustomer)
              ,("ar_party_id", toDBInt64 partyId)
              ,("ar_rate_category_id", toDBInt64 localPriceCategoryId)
              ]

    structureId <- lift $ trecord_save localConn "ar_organization_unit_has_structure" recStructure maybeStructureId

    -- Add extension `ar_organization_unit`

    let unitRec
          = trecord_setMany
              (trecord_empty)
              [("internal_name", toDBText extensionInternalName)
              ,("internal_checksum1", toDBByteString addToHistoryChecksum)
              ,("internal_checksum2", toDBByteString infoChecksum)
              ,("automatically_managed_from", toDBInt64 1)
              ]

    extensionUnitId <- lift $ trecord_save localConn "ar_organization_unit" unitRec maybeExtensionUnitId
    -- NOTE: symbolic extension ids are always updated, and there is no history about them

    -- Add extension `ar_organization_unit_has_structure`

    let recStructure
          = trecord_setMany
              (trecord_empty)
              [("ar_organization_unit_id", toDBInt64 extensionUnitId)
              ,("ar_organization_unit_type_id", toDBInt64 $ unitType_defaultId localUnitType UnitType_extension)
              ,("ar_parent_organization_unit_id", toDBInt64 unitId)
              ,("from", trecord "creationdate" remoteInfo)
              ,("exists", toDBBool True)
                -- NOTE: mantain active, because disabilitation must involve the history,
                -- and history is only mantained for the party parent structure
              ,("ar_party_id", DB.MySQLNull)
              ,("ar_rate_category_id", DB.MySQLNull) -- NOTE: the same of the parent
              ,("extension_codes", toDBText extensionInternalName)
              ]

    extensionStructureId <- lift $ trecord_save localConn "ar_organization_unit_has_structure" recStructure maybeExtensionStructureId
    -- NOTE: also in this case they are only updated, and there is no history,
    -- because they are a fixed symbol, associated to the root unit-id

    -- Check if there is an user with the same login

    case maybeUserLoginAndPassword of
      Nothing -> return ()
      Just (userLogin, _) -> do
        mr <- liftIO $ tfields_result
                         localConn
                         user_tfields
                         ( Just ( "login = ? AND ar_organization_unit_id <> ? LIMIT 1"
                                , [toDBText userLogin, toDBInt64 unitId]))
        case mr of
          Nothing -> return ()
          Just r ->
            throwError $ Just $
                createError'
                  ("The user login " ++ Text.unpack userLogin
                      ++ " is used from user with unitId " ++ (show unitId)
                      ++ " and also for another user with unitId " ++ (show $ fromDBInt $ trecord "ar_organization_unit_id" r)
                      ++ ", but it had to be unique.")
                  "Find the repeated user login, and fix it."

    -- Update `ar_user`
    -- NOTE: the case of an `ar_user` to delete is managed before calling this code

    case maybeUserLoginAndPassword of
      Nothing -> return ()
      Just (userLogin, userPassword) -> do
        let recUser
              = trecord_setMany
                  (trecord_empty)
                  [("ar_organization_unit_id", toDBInt64 unitId)
                  ,("login", toDBText userLogin)
                  ,("clear_password_to_import", toDBText userPassword)
                  ,("is_enabled", toDBBool True)
                    -- NOTE: mantain active, because an inactive user has still the rigts to see the calls of the past
                  ,("ar_party_id", DB.MySQLNull)
                  ,("is_root_admin", toDBBool False)
                  ]

        id' <- lift $ trecord_save localConn "ar_user" recUser maybeUserId

        _ <- liftIO $
               DB.execute
                 localConn
                 "INSERT INTO ar_user_has_role SET ar_user_id = ?, ar_role_id = ?"
                 [toDBInt64 id', toDBInt64 $ userRole_defaultId userRoleInfo UserRole_user]

        return ()
        -- NOTE: also in this case they are only updated, and there is no history,
        -- because they are a fixed symbol, associated to the root unit-id

  appendFields :: TRecord -> [BS.ByteString] -> BS.ByteString
  appendFields tr ns = BS.concat $ L.intersperse " " $ fromMaybeDBValues fromDBByteString $ L.map (\n -> trecord n tr) ns

  infoFromProvider :: BS.ByteString
  infoFromProvider = "provider"

  infoFromCRMContract :: BS.ByteString
  infoFromCRMContract = "CRM-CONTRACT"

-- | The fields to import and analyze.
--   DEV-NOTE: if more fields are imported, and they affect the history,
--   then the hash can change, and a massive reimport and change of history can be done.
--   In case a conversion passage should be done, for applying the new HASH code to the current
--   fields.
itec1_tfields :: [TField]
itec1_tfields = [
    TField "id" True True False
  , TField "account" False True False
    -- ^ unique CRM code also without adding the prefix
  , TField "contract_number" False True False
    -- ^ contract number signed from the customer
  , TField "billto" False True True
  -- ^ the billable party
  , TField "creationdate" False True False
  -- ^ when the customer was created. The first activation date
  , TField "firstusedate" False True False
  -- ^ the first call of the customer. Ignored
  , TField "expirationdate" False False True
  -- ^ ignore
  , TField "enableexpire" False False True
  -- ^ ignore
  , TField "expiredays" False False True
  -- ^ ignore
  , TField "username" False True False
  -- ^ the web UI login name of the user
  , TField "useralias" False False False
  -- ^ ignore
  , TField "uipass" False True False
  -- ^ the web UI password
  , TField "credit" False False True
  -- ^ ignore: the current left credit, but it decrease at every call
  , TField "credit_data" False False True
  -- ^ ignore
  , TField "tariff" False True True
  -- ^ the customer price-category
  , TField "id_didgroup" False False False
  -- ^ ignore
  , TField "activated" False True True
  -- ^ false when the customer is disactivated, and it can not process calls
  , TField "status" False True False
  -- ^ 1 for active customers
  , TField "lastname" False True False
  -- ^ not the billable party, but the contact info
  , TField "firstname" False True False
  -- ^ not the billable party, but the contact info
  , TField "address" False True False
  , TField "city" False True False
  , TField "state" False True False
  , TField "country" False True False
  , TField "zipcode" False True False
  , TField "phone" False True False
  , TField "email" False True False
  , TField "fax" False True False
  , TField "inuse" False False False
  -- ^ ignore
  , TField "simultaccess" False False False
  -- ^ ignore
  , TField "currency" False False False
  , TField "lastuse" False False False
  -- ^ ignore
  , TField "nbused" False False False
  -- ^ ignore
  , TField "typepaid" False False False
  -- ^ ignore
  , TField "creditlimit" False True False
  -- ^ the maximum calls that the customer can do in a month
  , TField "creditlimit_data" False False False
  -- ^ ignore
  , TField "voipcall" False False False
  -- ^ ignore
  , TField "sip_buddy" False False False
  -- ^ ignore
  , TField "iax_buddy" False False False
  -- ^ ignore
  , TField "language" False False False
  -- ^ ignore
  , TField "redial" False False False
  -- ^ ignore
  , TField "runservice" False False False
  -- ^ ignore
  , TField "nbservice" False False False
  -- ^ ignore
  , TField "id_campaign" False False False
  -- ^ ignore
  , TField "num_trials_done" False False False
  -- ^ ignore
  , TField "vat" False False False
  -- ^ ignore
  , TField "servicelastrun" False False False
  -- ^ ignore
  , TField "initialbalance" False False False
  -- ^ ignore
  , TField "invoiceday" False False False
  -- ^ ignore
  , TField "autorefill" False False False
  -- ^ ignore
  , TField "loginkey" False False False
  -- ^ ignore
  , TField "mac_addr" False False False
  -- ^ ignore
  , TField "id_timezone" False False False
  -- ^ ignore
  , TField "tag" False False False
   -- ^ ignore
  , TField "voicemail_permitted" False False False
   -- ^ ignore
  , TField "voicemail_activated" False False False
   -- ^ ignore
  , TField "last_notification" False False False
   -- ^ ignore
  , TField "email_notification" False False False
   -- ^ ignore
  , TField "notify_email" False False False
   -- ^ ignore
  , TField "notify_monitoring" False False False
   -- ^ ignore
  , TField "email_monitoring" False False False
   -- ^ ignore
  , TField "contact_monitoring" False False False
   -- ^ ignore
  , TField "number_monitoring" False False False
   -- ^ ignore
  , TField "credit_notification" False False False
   -- ^ ignore
  , TField "id_group" False False False
   -- ^ ignore
  , TField "id_agent" False False False
   -- ^ ignore
  , TField "company_name" False True False
    -- ^ the name to use in invoices
  , TField "reg_number" False True False
   -- ^ ignore
  , TField "id_number" False True False
  -- ^ company legal registration number
  , TField "company_website" False True False
   -- ^ company web site
  , TField "vat_rn" False True False
   -- ^ the VAT of the company
  , TField "traffic" False False False
   -- ^ ignore
  , TField "traffic_target" False False False
   -- ^ ignore
  , TField "discount" False False False
   -- ^ ignore
  , TField "dovalue" False False False
   -- ^ ignore
  , TField "debitorder" False True False
   -- ^ the type of the customer (e.g. TES, IVOICE, etc..)
  , TField "restriction" False False False
   -- ^ ignore
  , TField "id_seria" False False False
   -- ^ ignore
  , TField "serial" False False False
   -- ^ ignore
  , TField "block" False False False
   -- ^ ignore
  , TField "lock_pin" False False False
   -- ^ ignore
  , TField "lock_date" False False False
   -- ^ ignore
  , TField "acctype" False False False
   -- ^ ignore
  , TField "lastmile" False False False
   -- ^ ignore
  , TField "bandwidth" False False False
   -- ^ ignore
  , TField "deleted" False True True
   -- ^ 1 for customers I can ignore
  , TField "max_concurrent" False False False
   -- ^ ignore
  , TField "client_type" False False False
   -- ^ ignore
  , TField "data_bundle" False False False
   -- ^ ignore
  , TField "in_bundle_rate" False False False
   -- ^ ignore
  , TField "out_bundle_rate" False False False
   -- ^ ignore
  , TField "line_loca" False False False
   -- ^ ignore
  ]

-- | Import all data in static-way
itec1_stc_tfields :: [TField]
itec1_stc_tfields = L.map (\tf ->  tf { tfield_addToHistory = False }) itec1_tfields
{-# INLINE itec1_stc_tfields #-}

type CRMCode = Text.Text

data Itec1CRMToContractRate
       = Itec1CRMToContractRate {
           itec1CRM_nr :: !Text.Text
         , itec1CRM_customerName :: !Text.Text
         , itec1CRM_crm :: !CRMCode
         , itec1CRM_contractCode :: !Text.Text
         , itec1CRM_localPrice :: !Text.Text
         , itec1CRM_nationalPrice :: !Text.Text
         , itec1CRM_mobilePrice :: !Text.Text
         , itec1CRM_priceCategory :: !Text.Text
         }
 deriving (Generic)

instance CSV.FromRecord Itec1CRMToContractRate

type RemoteRateCategoryCode = RateCategoryCode

-- | Load an Itec configuration file mapping CRM customer codes to ContractRates
itec1_crmToContractRate :: String -> IO (Map.Map CRMCode RemoteRateCategoryCode)
itec1_crmToContractRate fileName = do
  fileContent <- LBS.readFile fileName
  let maybeError = CSV.decode HasHeader fileContent
  case maybeError of
    Left err -> throwIO $ AsterisellException $ "Error during parsing of " ++ fileName ++ " - " ++ err
    Right ds -> do
      return $
        V.foldr
          (\d m -> Map.insert
                     (itec1CRM_crm d)
                     (itec1CRM_priceCategory d)
                     m
          ) Map.empty ds

data Itec1LegacyRate
       = Itec1LegacyRate { itec1LR_remoteRateName :: !Text.Text }
           deriving (Generic)

instance CSV.FromRecord Itec1LegacyRate

-- | Load list of legacy PriceCategories on A2Billing servers that are not any more used.
--   They are replaced by itec1_crmToContractRate.
itec1_legacyRates :: String -> IO (Set.Set RateCategoryCode)
itec1_legacyRates fileName = do
  fileContent <- LBS.readFile fileName
  let maybeError = CSV.decode NoHeader fileContent
  case maybeError of
    Left err -> throwIO $ AsterisellException $ "Error during parsing of " ++ fileName ++ " - " ++ err
    Right ds -> do
      return $ V.foldr (\d s -> Set.insert (itec1LR_remoteRateName d) s) Set.empty ds
