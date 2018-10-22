{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, DeriveGeneric, DeriveAnyClass  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

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
   rolf1_synchro,
   rolf1_staticImport
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

import Control.Exception.Assert.Sugar
import Debug.Trace

import GHC.Generics
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)

-- -------------
-- Rolf1 project

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

rolf1_errorGarbageKey :: DataSourceName -> BS.ByteString
rolf1_errorGarbageKey dsn = BS.concat ["org-synchro-", fromTextToByteString dsn]

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
--
--   The complete specification of the conversion is the code, but from an high-level point of view:
--   * import users with login access if the fields ... are specified on the remote side
--   * associate a partyTag according the fields ...
--   * a party/customer can be defined in more than one external data sources, so try to use/prefer the data source with active info
--   * remote table ``cc_tariffgroup`` contains info about price-categories
--   * remote table ``cc_card`` contains info about customers, according fields described in `rolf1_tfields``
--   * ignore `ZZZ999` accounts, because it is  test accounts, and they are specified in the initialization procedure
--   DEV-NOTE: if you change the logic of this code, update also the `CustomerSpecificImporters.rolf1_` related code.
--
--   DEV-NOTE: archived code (not used in production), but maintained because it is a tested example of
--   active synchronization of customers on external databases.
rolf1_synchro
    :: DBConf
    -> DB.ConnectInfo
    -> Text.Text -- ^ organization to ignore
    -> CurrencyPrecisionDigits
    -> DataSourceName
    -> CmdParams
       -- ^ all params of type "SOME-PRICE-CATEGORY-NAME,add-provider-prefix"
       --   instruct the importer for adding the provider prefix to the price-category
       --   with the specified name. If there is no match, then the original name is maintained.
       --   Adding the provider prefix, it is possible having custom price-categories, depending also
       --   from the provider in which the customer is mainly defined.
       --   TODO see if this is still necessary, now that I merged many price-categories, during importing phase.
    -> IO ()
rolf1_synchro localDBConf remoteDBConf organizationToIgnore currencyInfo dataSourceName cparams = do
  withResource'
    (do dbStateR <- db_init localDBConf Nothing 4
        remoteConn <- DB.connect remoteDBConf
        return (dbStateR, remoteConn)
    )
    (\(dbStateR, remoteConn) -> do

          dbState <- readIORef dbStateR
          let localConn = dbps_conn dbState

          -- start with an empty transaction because a new one will be created for every import
          db_garbagePastErrors dbStateR (rolf1_errorGarbageKey dataSourceName) Nothing Nothing
          db_commitTransaction localConn

          --
          -- Before importing customers, load local data
          --
          -- DEV-NOTE: use DeepSeq for forcing the read of  the data, before writing the new data into the DB

          (_, !localUnitType) <- DeepSeq.force <$> unitType_load localConn False
          let !extensionUnitTypeId = DeepSeq.force $ unitType_defaultId localUnitType UnitType_extension
          !partyTagInfo <- DeepSeq.force <$> partyTagInfo_load localConn False
          !userRoleInfo <- DeepSeq.force <$> userRole_load localConn False
          !fromRemoteToLocalRateId <- DeepSeq.force <$> synchroPriceCategories dbStateR remoteConn
          !unbilledCallsFromDate <- defaultParams_unbilledCallsFrom localConn "02676"

          --
          -- Process all data on remote database
          --

          remoteInfo <- liftIO $ tfields_results remoteConn (TFields "cc_card" rolf1_tfields) Nothing

          parentIdForExtensionsToIgnore <- extensionToIgnoreId localConn organizationToIgnore
          partyTagInfoRef <- newIORef partyTagInfo
          newExtensionsR <- newIORef (Set.empty)
          newExtensionsToIgnoreR <- newIORef (Set.empty)
          processedAccountsR <- newIORef (Set.empty)

          updateOrganizationsS <- S.makeOutputStream $ updateOrganizations dbStateR parentIdForExtensionsToIgnore userRoleInfo localUnitType fromRemoteToLocalRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR

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


                trecord_save localConn "ar_organization_unit_has_structure" recStructure Nothing

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
    (\isOk (dbStateR, remoteConn) -> do
         DB.close remoteConn
         db_releaseResourceR isOk dbStateR
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
  synchroPriceCategories :: IORef DBState -> DB.MySQLConn -> IO (IMap.IntMap Int)
  synchroPriceCategories dbStateR remoteConn = do
    dbState <- readIORef dbStateR
    let localConn = dbps_conn dbState

    !(rateCategories, _) <- rateCategories_load localConn False
    (_, remotePricesS) <- DB.query_ remoteConn "SELECT id, tariffgroupname FROM cc_tariffgroup"
    S.foldM (synchroPriceCategory dbStateR rateCategories) IMap.empty remotePricesS

  -- | Import a single price-category.
  synchroPriceCategory
      :: IORef DBState
      -> Map.Map RateCategoryCode RateCategoryId
         -- ^ from price-category code (its name, with maybe the provider added as prefix),
         --   to the Id of the corresponding Asterisell `ar_rate_category` (price-category)
      -> IMap.IntMap Int
         -- ^ map the remote-id of the price-category, with its local id
      -> [DB.MySQLValue]
         -- ^ a single record, with remote price-category-id, and the remote name
      -> IO (IMap.IntMap Int)
         -- ^ the new mapping extended wit the imported price-category, if it was new
  synchroPriceCategory dbStateR rateCategories map1 [rrateId, rrateName] = do
    dbState <- readIORef dbStateR
    let localConn = dbps_conn dbState

    let rateName = internalName_sanityze $ fromDBText rrateName
    let rateNameWithProvider = internalName_sanityze $ Text.concat [dataSourceName, "-", rateName]
    let rateKey = fromTextToByteString rateName

    let rateNameToUse
            = case Map.lookup rateKey cparams of
                Just "add-provider-prefix"
                  -> rateNameWithProvider
                _ -> rateName

    let remoteId = fromDBInt rrateId

    case Map.lookup rateNameToUse rateCategories of
      Just localRateId
          -> return $ IMap.insert remoteId localRateId map1
      Nothing
          -> do let priceRec
                      = trecord_setMany
                          (trecord_empty)
                          [("internal_name", toDBText rateNameToUse)
                          ,("short_description", toDBText $ Text.concat ["Rate ", rateName, " on provider ", dataSourceName])
                          ]

                localRateId <- trecord_save localConn "ar_rate_category" priceRec Nothing

                putStrLn $ "Added new price-category: " ++ Text.unpack rateNameToUse

                return $ IMap.insert remoteId localRateId map1

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
  --   After changes in requirements, we have now only one trust-level, so this code is overkill, but I mantain it.
  tcard_trustLevel :: TRecord -> TrustLevel
  tcard_trustLevel trec
      = let activeCustomer = fromDBBool $ trecord "activated" trec
            customerStatus = fromMaybeDBValueToDefault 0 fromDBInt $ trecord "status" trec
            deletedCustomer = fromDBBool $ trecord "deleted" trec
            maybeFirstUseDate = fromMaybeDBValue fromDBLocalTime $ trecord "firstusedate" trec

            addToTrustLevel True tl = tl + 1
            addToTrustLevel False tl = tl

        in addToTrustLevel activeCustomer 0

  tcard_maxTrustLevel :: TrustLevel
  tcard_maxTrustLevel = 1

  -- | Update organization info.
  --   An action called for every remote record (it is an S.Output stream).
  --   Create only
  updateOrganizations
    :: IORef DBState
    -> UnitId -- ^ organization to ignore
    -> UserRoleIdFromInternalName
    -> UnitTypeFromInternalName
    -> IMap.IntMap Int  -- ^ from remote to local rate id
    -> LocalTime -- ^ official calldate
    -> IORef PartyTagInfo
    -> IORef (Set.Set ExtensionAsText) -- ^ new created extensions, that are not to ignore
    -> IORef (Set.Set ExtensionAsText) -- ^ new found extensions to ignore
    -> IORef (Set.Set ExtensionAsText) -- ^ processed accounts. Used for recognizing errors.
    -> Maybe TRecord
    -> IO ()

  updateOrganizations dbStateR parentIdForExtensionsToIgnore userRoleInfo localUnitType fromRemoteToLocalRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR maybeRemoteInfo = do
    dbState <- readIORef dbStateR
    let localConn = dbps_conn dbState

    db_openTransaction localConn
    r <- runExceptT $ updateOrganizations1 dbStateR parentIdForExtensionsToIgnore userRoleInfo localUnitType fromRemoteToLocalRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR maybeRemoteInfo
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
                                     = BS.concat [rolf1_errorGarbageKey dataSourceName, "-customer-", fromStringToByteString $ show $ fromDBInt $ trecord "id" remoteInfo]
                                 , asterisellError_effect
                                     = "All the calls of this customer will be not rated correctly. Remote customer info: \n" ++ (fromByteStringToString $ trecord_show remoteInfo)
                                 }
                        Nothing
                          -> err {
                                   asterisellError_key
                                      = BS.concat [rolf1_errorGarbageKey dataSourceName, "-missing-customers"]
                                 , asterisellError_effect
                                     = "Customers deleted from the remote database are not correctly disabled on the local Asterisell side, so there can be conflicts, and calls assigned to wrong customers."
                                 }

           in do db_rollBackTransaction localConn
                 db_openTransaction localConn
                 dbErrors_insert
                   localConn
                   Nothing
                   (err' { asterisellError_garbageKey = rolf1_errorGarbageKey dataSourceName })
                   (Just (unbilledCallsFromDate, unbilledCallsFromDate))
                 db_commitTransaction localConn
                 return ()
      Right rr
        -> do db_commitTransaction localConn
              return ()

  -- | Execute an update pass, or signal an error, using `createError'` function, or ignore the importing returning Nothing.
  updateOrganizations1
    :: IORef DBState
    -> UnitId -- ^ organization to ignore
    -> UserRoleIdFromInternalName
    -> UnitTypeFromInternalName
    -> IMap.IntMap Int -- ^ map remote price-categories, with local price-categories
    -> LocalTime
    -- ^ the official call-date.
    --   Changes to customers are added to the history only after invoices are sent,
    --   otherwise they are fixes.
    -> IORef PartyTagInfo
    -> IORef (Set.Set ExtensionAsText)  -- ^ new inserted extensions, that are not to ignore
    -> IORef (Set.Set ExtensionAsText)  -- ^ new extensions to ignore
    -> IORef (Set.Set ExtensionAsText)  -- ^ accounts already processed, used for recognizing errors
    -> Maybe TRecord                    -- ^ Nothing when the last record is reached
    -> ImportMonad ()

  updateOrganizations1 dbStateR _ userRoleInfo localUnitType fromRemoteToLocalRateId unbilledCallsFromDate partyTagInfoRef _ _ _ Nothing = return ()

  updateOrganizations1 dbStateR parentIdForExtensionsToIgnore userRoleInfo localUnitType fromRemoteToLocalRateId unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR (Just remoteInfo) = do
    dbState <- liftIO $ readIORef dbStateR
    let localConn = dbps_conn dbState

    --
    -- Read all data, without writing.
    -- Return with a throw in case of errors or data to ignore.
    --

    let trustLevel = tcard_trustLevel remoteInfo

    (accountCode :: Text.Text)
      <- let   account' = Text.strip $ fromMaybeDBValueToDefault (Text.empty) fromDBText (trecord "account" remoteInfo)
         in do when (Text.length account' == 0)
                    (throwError $ Just $ createError' "The customer has no CRM code." "Complete the info on the provider side.")
               return account'

    let internalName = accountCode
    let extensionInternalName = internalName <> "-ext"
    let (_, _, addToHistoryChecksum, infoChecksum) = trecord_internalName dataSourceName remoteInfo

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
    -- Skip test account
    --

    when (accountCode == "ZZZ999") (throwError Nothing)

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

    -- Next sections works assuming this structure of data
    --
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
    (  maybeUnitId :: Maybe UnitId -- ^ Nothing if new customer had to be created
     , maybePartyId :: Maybe Int -- ^ Nothing if new party had to be created
     , maybeStructureId :: Maybe UnitId -- ^ Nothing if new history has to be added (i.e. new customer, or change of data for a customer)
     , structFromDate :: CallDate -- ^ add/update history info using this date of activation
     , maybeExtensionUnitId :: Maybe UnitId -- ^ Nothing if it is a new customer
     , maybeExtensionStructureId :: Maybe UnitId -- ^ Nothing if it a new customer
     , maybeUserId :: Maybe UserId
      -- ^ Nothing for creating a new login/password user in case there is login/password
      --   Just userId for updating an existing login/password user
     , addToHistory :: Bool
      -- ^ True if there is new data to add to the history of the customer, or it is a new customer
     , isNewCustomer :: Bool
     -- ^ True if it is a newly created customer
     )
      <- case (maybeUnitRec, maybeExtensionStructRec) of
           (Nothing, Nothing) -> do
             liftIO $ modifyIORef' newExtensionsR (\s -> Set.insert extensionInternalName s)
             return (Nothing, Nothing, Nothing, fromDBLocalTime $  trecord "creationdate" remoteInfo, Nothing, Nothing, Nothing, True, True)


           (Just _, Nothing) ->
              error "ERR 66753: unexpected error in the code. Contact the assistance."

           (Nothing, Just _) ->
              error "ERR 66754: unexpected error in the code. Contact the assistance."

           (Just unitRec, Just extensionStructRec) -> do
             let unitId = trecord "id" unitRec

             let oldDataSourceName = fromDBText $ trecord "internal_checksum4" unitRec

             let oldTrustLevel = fromJust1 "ERR 60056" $ fromTextToInt $ fromDBText $ trecord "internal_checksum3" unitRec

             let oldInfoChecksum = fromDBByteString $ trecord "internal_checksum2" unitRec

             let oldHistoryChecksum = fromDBByteString $ trecord "internal_checksum1" unitRec

             when ((trustLevel == oldTrustLevel) && (oldDataSourceName /= dataSourceName))
                  (throwError
                     $ createWarning'
                         ("This customer with CRM code " ++ Text.unpack accountCode ++ " is defined also on the provider " ++ (Text.unpack oldDataSourceName) ++ ", and both are active.")
                         ("Disable this customer info, or the corresponding customer info on the provider " ++ Text.unpack oldDataSourceName))

             -- Skip processing if the trustLevel is less than previous found trustLevel.
             -- Skip processing if the data has not changed
             -- NOTE: up to date only tcard_maxTrustLevel are imported, so this code is never fired
             -- DEV-NOTE: probably this code is buggy, but it is under unit-testing, so if the maxTrustLevel condition is removed, at least unit tests will fail
             when (trustLevel < oldTrustLevel || (trustLevel == oldTrustLevel && oldInfoChecksum == infoChecksum && oldHistoryChecksum == addToHistoryChecksum))
                  (throwError Nothing)

             structRec
               <- liftIO $ tfields_result
                             localConn
                             organizationUnitStruct_tfields
                             (Just ( "ar_organization_unit_id = ? ORDER BY `from` DESC LIMIT 1"
                                   , [unitId]))

             let partyId' = trecord "ar_party_id" $ fromJust1 "ERR 1027" structRec

             let extensionStructId' = fromDBInt $ trecord "id" extensionStructRec

             let extensionUnitId' = fromDBInt $ trecord "ar_organization_unit_id" extensionStructRec

             maybeUserRec' <- liftIO $
                 tfields_result
                    localConn
                    user_tfields
                    ( Just ( "ar_organization_unit_id = ? AND is_root_admin = 0 ORDER BY id ASC LIMIT 1"
                           , [unitId]))

             maybeUserId'
                 <- case maybeUserRec' of
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

             let (maybeStructureId', useDate', addToHistory')
                    = case (oldHistoryChecksum == addToHistoryChecksum) of
                        True -> (Just $ structId, structFromDate, False)
                        False -> case (structFromDate >= unbilledCallsFromDate) of
                                   True -> (Just $ structId, structFromDate, False)
                                            -- NOTE: if the structure is changed in an unbilled time-frame, simply fix the history.
                                   False -> (Nothing, unbilledCallsFromDate, True)
                                            -- NOTE: if the structure is changed, preserve the old (already billed) info,
                                            -- and add new info to the history.

             return ( Just $ fromDBInt $ unitId
                    , Just $ fromDBInt $ partyId'
                    , maybeStructureId'
                    , useDate'
                    , Just extensionUnitId'
                    , Just extensionStructId'
                    , maybeUserId'
                    , assert ((addToHistory' && isNothing maybeStructureId') || (not addToHistory' && isJust maybeStructureId')) addToHistory'
                    , False
                    )

    --
    -- Write info
    --
    -- NOTE: the code process this only if there is change in data, otherwine a `throwError Nothing` is throwed in previous section

    liftIO $ putStrLn $ "Update account: " ++ Text.unpack accountCode

    -- Add `ar_organization_unit`

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

    let maybeRemoteRateId = fromMaybeDBValue fromDBInt $ trecord "tariff" remoteInfo

    localRateId
      <- case maybeRemoteRateId of
           Nothing -> throwError $ Just $ createError' "There is a customer with missing price-category." "Set the price-category of the customer"
           Just remoteRateId
             -> case IMap.lookup remoteRateId fromRemoteToLocalRateId of
                  Nothing -> throwError $
                               Just $
                                 createError'
                                   ("The price category with remote id " ++ show remoteRateId ++ " has no local counterpart.")
                                   ("Change the price-category of the customer, to an existing one, or define this new price-category on the Asterisell side.")
                  Just i -> return i

    let recStructure
          = trecord_setMany
              (trecord_empty)
              [("ar_organization_unit_id", toDBInt64 unitId)
              ,("ar_organization_unit_type_id", toDBInt64 $ unitType_defaultId localUnitType UnitType_customer)
              ,("ar_parent_organization_unit_id", toMaybeInt64 maybeParentBillableUnitId)
              ,("from", toDBLocalTime structFromDate)
              ,("exists", activeCustomer)
              ,("ar_party_id", toDBInt64 partyId)
              ,("ar_rate_category_id", toDBInt64 localRateId)
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

-- | The fields to import and analyze.
--   DEV-NOTE: if more fields are imported, and they affect the history,
--   then the hash can change, and a massive reimport and change of history can be done.
--   In case a conversion passage should be done, for applying the new HASH code to the current
--   fields.
rolf1_tfields :: [TField]
rolf1_tfields = [
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
   -- ^ only customer tagged as "Ivoice" has to be exported to the external billing system
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

-- ---------------------------------------------
-- Rolf1 static import of customer data

-- | Import customers, more or less like `rolf1_synchro` but:
--   * in a static way (called only one time)
--   * assume to be called only one time on an empty database
--   * ignore price-categories, and assign all to `to-complete` price-category
--
rolf1_staticImport
    :: DBConf
    -> DB.ConnectInfo
    -> Text.Text -- ^ organization to ignore
    -> CurrencyPrecisionDigits
    -> DataSourceName
    -> CmdParams
       -- ^ all params of type "SOME-PRICE-CATEGORY-NAME,add-provider-prefix"
       --   instruct the importer for adding the provider prefix to the price-category
       --   with the specified name. If there is no match, then the original name is maintained.
       --   Adding the provider prefix, it is possible having custom price-categories, depending also
       --   from the provider in which the customer is mainly defined.
       --   TODO see if this is still necessary, now that I merged many price-categories, during importing phase.
    -> IO ()
rolf1_staticImport localDBConf remoteDBConf organizationToIgnore currencyInfo dataSourceName cparams = do
  withResource'
    (do dbStateR <- db_init localDBConf Nothing 4
        remoteConn <- DB.connect remoteDBConf
        return (dbStateR, remoteConn)
    )
    (\(dbStateR, remoteConn) -> do

          dbState <- readIORef dbStateR
          let localConn = dbps_conn dbState

          -- start with an empty transaction because a new one will be created for every import
          db_garbagePastErrors dbStateR (rolf1_errorGarbageKey dataSourceName) Nothing Nothing
          db_commitTransaction localConn

          --
          -- Before importing customers, load local data
          --
          -- DEV-NOTE: use DeepSeq for forcing the read of  the data, before writing the new data into the DB

          (_, !localUnitType) <- DeepSeq.force <$> unitType_load localConn False
          let !extensionUnitTypeId = DeepSeq.force $ unitType_defaultId localUnitType UnitType_extension
          !partyTagInfo <- DeepSeq.force <$> partyTagInfo_load localConn False
          !userRoleInfo <- DeepSeq.force <$> userRole_load localConn False
          !unbilledCallsFromDate <- defaultParams_unbilledCallsFrom localConn "02676"

          -- Create "missing" price-category (but only if it is new)
          let rateCategoryToComplete = "to-complete"
          (!rateCategories, _) <- rateCategories_load localConn False
          priceCategoryId
            <- case Map.lookup rateCategoryToComplete rateCategories of
                 Just i -> return i
                 Nothing -> do
                   let priceRec
                        = trecord_setMany
                            (trecord_empty)
                            [("internal_name", toDBText rateCategoryToComplete)
                            ,("short_description", toDBText "Imported customer, with price-category to complete")
                            ]

                   trecord_save localConn "ar_rate_category" priceRec Nothing

          --
          -- Process all data on remote database
          --

          remoteInfo <- liftIO $ tfields_results remoteConn (TFields "cc_card" rolf1_stc_tfields) Nothing

          parentIdForExtensionsToIgnore <- extensionToIgnoreId localConn organizationToIgnore
          partyTagInfoRef <- newIORef partyTagInfo
          newExtensionsR <- newIORef (Set.empty)
          newExtensionsToIgnoreR <- newIORef (Set.empty)
          processedAccountsR <- newIORef (Set.empty)

          updateOrganizationsS <- S.makeOutputStream $ updateOrganizations dbStateR parentIdForExtensionsToIgnore priceCategoryId userRoleInfo localUnitType unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR

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


                trecord_save localConn "ar_organization_unit_has_structure" recStructure Nothing

                putStrLn $ "Added extension to ignore: " ++ Text.unpack newExtension

                return ()
            ) newExtensionsToIgnore

    )
    (\isOk (dbStateR, remoteConn) -> do
         DB.close remoteConn
         db_releaseResourceR isOk dbStateR
         -- NOTE: save also the generated errors
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
  --   After changes in requirements, we have now only one trust-level, so this code is overkill, but I mantain it.
  tcard_trustLevel :: TRecord -> TrustLevel
  tcard_trustLevel trec
      = let activeCustomer = fromDBBool $ trecord "activated" trec
            customerStatus = fromMaybeDBValueToDefault 0 fromDBInt $ trecord "status" trec
            deletedCustomer = fromDBBool $ trecord "deleted" trec
            maybeFirstUseDate = fromMaybeDBValue fromDBLocalTime $ trecord "firstusedate" trec
        in L.sum $ L.map (\c -> if c then 1 else 0) $ [activeCustomer]

  tcard_maxTrustLevel :: TrustLevel
  tcard_maxTrustLevel = 1

  -- | Update organization info.
  --   An action called for every remote record (it is an S.Output stream).
  --   Create only
  updateOrganizations
    :: IORef DBState
    -> UnitId -- ^ organization to ignore
    -> Int -- ^ price-category id
    -> UserRoleIdFromInternalName
    -> UnitTypeFromInternalName
    -> LocalTime -- ^ official calldate
    -> IORef PartyTagInfo
    -> IORef (Set.Set ExtensionAsText) -- ^ new created extensions, that are not to ignore
    -> IORef (Set.Set ExtensionAsText) -- ^ new found extensions to ignore
    -> IORef (Set.Set ExtensionAsText) -- ^ processed accounts. Used for recognizing errors.
    -> Maybe TRecord
    -> IO ()

  updateOrganizations dbStateR parentIdForExtensionsToIgnore priceCategoryId  userRoleInfo localUnitType unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR maybeRemoteInfo = do
    dbState <- readIORef dbStateR
    let localConn = dbps_conn dbState

    db_openTransaction localConn
    r <- runExceptT $ updateOrganizations1 dbStateR parentIdForExtensionsToIgnore priceCategoryId userRoleInfo localUnitType unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR maybeRemoteInfo
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
                                     = BS.concat [rolf1_errorGarbageKey dataSourceName, "-customer-", fromStringToByteString $ show $ fromDBInt $ trecord "id" remoteInfo]
                                 , asterisellError_effect
                                     = "All the calls of this customer will be not rated correctly. Remote customer info: \n" ++ (fromByteStringToString $ trecord_show remoteInfo)
                                 }
                        Nothing
                          -> err {
                                   asterisellError_key
                                      = BS.concat [rolf1_errorGarbageKey dataSourceName, "-missing-customers"]
                                 , asterisellError_effect
                                     = "Customers deleted from the remote database are not correctly disabled on the local Asterisell side, so there can be conflicts, and calls assigned to wrong customers."
                                 }

           in do db_rollBackTransaction localConn
                 db_openTransaction localConn
                 dbErrors_insert
                   localConn
                   Nothing
                   (err' { asterisellError_garbageKey = rolf1_errorGarbageKey dataSourceName })
                   (Just (unbilledCallsFromDate, unbilledCallsFromDate))
                 db_commitTransaction localConn
                 return ()
      Right rr
        -> do db_commitTransaction localConn
              return ()

  -- | Execute an update pass, or signal an error, using `createError'` function, or ignore the importing returning Nothing.
  updateOrganizations1
    :: IORef DBState
    -> UnitId -- ^ organization to ignore
    -> Int -- ^ price category to use
    -> UserRoleIdFromInternalName
    -> UnitTypeFromInternalName
    -> LocalTime
    -- ^ the official call-date.
    --   Changes to customers are added to the history only after invoices are sent,
    --   otherwise they are fixes.
    -> IORef PartyTagInfo
    -> IORef (Set.Set ExtensionAsText)  -- ^ new inserted extensions, that are not to ignore
    -> IORef (Set.Set ExtensionAsText)  -- ^ new extensions to ignore
    -> IORef (Set.Set ExtensionAsText)  -- ^ accounts already processed, used for recognizing errors
    -> Maybe TRecord                    -- ^ Nothing when the last record is reached
    -> ImportMonad ()

  updateOrganizations1 dbStateR _ _ userRoleInfo localUnitType unbilledCallsFromDate partyTagInfoRef _ _ _ Nothing = return ()

  updateOrganizations1 dbStateR parentIdForExtensionsToIgnore priceCategoryId userRoleInfo localUnitType unbilledCallsFromDate partyTagInfoRef newExtensionsR newExtensionsToIgnoreR processedAccountsR (Just remoteInfo) = do
    dbState <- liftIO $ readIORef dbStateR
    let localConn = dbps_conn dbState

    --
    -- Read all data, without writing.
    -- Return with a throw in case of errors or data to ignore.
    --

    let trustLevel = tcard_trustLevel remoteInfo

    (accountCode :: Text.Text)
      <- let   account' = Text.strip $ fromMaybeDBValueToDefault (Text.empty) fromDBText (trecord "account" remoteInfo)
         in do when (Text.length account' == 0)
                    (throwError $ Just $ createError' "The customer has no CRM code." "Complete the info on the provider side.")
               return account'

    let internalName = accountCode
    let extensionInternalName = internalName <> "-ext"
    let (_, _, addToHistoryChecksum, infoChecksum) = trecord_internalName dataSourceName remoteInfo

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
    -- Skip test account
    --

    when (accountCode == "ZZZ999") (throwError Nothing)

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

    -- Next sections works assuming this structure of data
    --
    -- * CRM unitId
    -- ** checksum codes
    -- ** data-source defining it, and trust level
    -- * CRM structureId
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
    (  maybeUnitId :: Maybe UnitId -- ^ Nothing if new customer had to be created
     , maybePartyId :: Maybe Int -- ^ Nothing if new party had to be created
     , maybeStructureId :: Maybe UnitId -- ^ Nothing if new history has to be added (i.e. new customer, or change of data for a customer)
     , structFromDate :: CallDate -- ^ add/update history info using this date of activation
     , maybeExtensionUnitId :: Maybe UnitId -- ^ Nothing if it is a new customer
     , maybeExtensionStructureId :: Maybe UnitId -- ^ Nothing if it a new customer
     , maybeUserId :: Maybe UserId
      -- ^ Nothing for creating a new login/password user in case there is login/password
      --   Just userId for updating an existing login/password user
     , addToHistory :: Bool
      -- ^ True if there is new data to add to the history of the customer, or it is a new customer
     , isNewCustomer :: Bool
     -- ^ True if it is a newly created customer
     )
      <- case (maybeUnitRec, maybeExtensionStructRec) of
           (Nothing, Nothing) -> do
             liftIO $ modifyIORef' newExtensionsR (\s -> Set.insert extensionInternalName s)
             return (Nothing, Nothing, Nothing, fromDBLocalTime $  trecord "creationdate" remoteInfo, Nothing, Nothing, Nothing, True, True)


           (Just _, Nothing) ->
              error "ERR 66753: unexpected error in the code. Contact the assistance."

           (Nothing, Just _) ->
              error "ERR 66754: unexpected error in the code. Contact the assistance."

           (Just unitRec, Just extensionStructRec) -> do
             let unitId = trecord "id" unitRec

             let oldDataSourceName = fromDBText $ trecord "internal_checksum4" unitRec

             let oldTrustLevel = fromJust1 "ERR 60056" $ fromTextToInt $ fromDBText $ trecord "internal_checksum3" unitRec

             let oldInfoChecksum = fromDBByteString $ trecord "internal_checksum2" unitRec

             let oldHistoryChecksum = fromDBByteString $ trecord "internal_checksum1" unitRec

             when ((trustLevel == oldTrustLevel) && (oldDataSourceName /= dataSourceName))
                  (throwError
                     $ createWarning'
                         ("This customer with CRM code " ++ Text.unpack accountCode ++ " is defined also on the provider " ++ (Text.unpack oldDataSourceName) ++ ", and both are active.")
                         ("Disable this customer info, or the corresponding customer info on the provider " ++ Text.unpack oldDataSourceName))

             -- Skip processing if the trustLevel is less than previous found trustLevel.
             -- Skip processing if the data has not changed
             -- NOTE: up to date only tcard_maxTrustLevel are imported, so this code is never fired
             -- DEV-NOTE: probably this code is buggy, but it is under unit-testing, so if the maxTrustLevel condition is removed, at least unit tests will fail
             when (trustLevel < oldTrustLevel || (trustLevel == oldTrustLevel && oldInfoChecksum == infoChecksum && oldHistoryChecksum == addToHistoryChecksum))
                  (throwError Nothing)

             structRec
               <- liftIO $ tfields_result
                             localConn
                             organizationUnitStruct_tfields
                             (Just ( "ar_organization_unit_id = ? ORDER BY `from` DESC LIMIT 1"
                                   , [unitId]))

             let partyId' = trecord "ar_party_id" $ fromJust1 "ERR 1027" structRec

             let extensionStructId' = fromDBInt $ trecord "id" extensionStructRec

             let extensionUnitId' = fromDBInt $ trecord "ar_organization_unit_id" extensionStructRec

             maybeUserRec' <- liftIO $
                 tfields_result
                    localConn
                    user_tfields
                    ( Just ( "ar_organization_unit_id = ? AND is_root_admin = 0 ORDER BY id ASC LIMIT 1"
                           , [unitId]))

             maybeUserId'
                 <- case maybeUserRec' of
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

             let (maybeStructureId', useDate', addToHistory')
                    = case (oldHistoryChecksum == addToHistoryChecksum) of
                        True -> (Just $ structId, structFromDate, False)
                        False -> case (structFromDate >= unbilledCallsFromDate) of
                                   True -> (Just $ structId, structFromDate, False)
                                            -- NOTE: if the structure is changed in an unbilled time-frame, simply fix the history.
                                   False -> (Nothing, unbilledCallsFromDate, True)
                                            -- NOTE: if the structure is changed, preserve the old (already billed) info,
                                            -- and add new info to the history.

             return ( Just $ fromDBInt $ unitId
                    , Just $ fromDBInt $ partyId'
                    , maybeStructureId'
                    , useDate'
                    , Just extensionUnitId'
                    , Just extensionStructId'
                    , maybeUserId'
                    , assert ((addToHistory' && isNothing maybeStructureId') || (not addToHistory' && isJust maybeStructureId')) addToHistory'
                    , False
                    )

    --
    -- Write info
    --
    -- NOTE: the code process this only if there is change in data, otherwine a `throwError Nothing` is throwed in previous section

    liftIO $ putStrLn $ "Update account: " ++ Text.unpack accountCode

    -- Add `ar_organization_unit`

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

    let maybeRemoteRateId = fromMaybeDBValue fromDBInt $ trecord "tariff" remoteInfo

    let recStructure
          = trecord_setMany
              (trecord_empty)
              [("ar_organization_unit_id", toDBInt64 unitId)
              ,("ar_organization_unit_type_id", toDBInt64 $ unitType_defaultId localUnitType UnitType_customer)
              ,("ar_parent_organization_unit_id", toMaybeInt64 maybeParentBillableUnitId)
              ,("from", toDBLocalTime structFromDate)
              ,("exists", activeCustomer)
              ,("ar_party_id", toDBInt64 partyId)
              ,("ar_rate_category_id", toDBInt64 priceCategoryId)
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

-- | Import all data in static-way
rolf1_stc_tfields :: [TField]
rolf1_stc_tfields = L.map (\tf ->  tf { tfield_addToHistory = False }) rolf1_tfields
