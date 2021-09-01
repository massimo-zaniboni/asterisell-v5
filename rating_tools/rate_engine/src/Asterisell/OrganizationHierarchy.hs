{-# Language BangPatterns, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts, QuasiQuotes  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Import organization hierarchies and extensions.
--
--   This code must be keept in synchro with `apps/asterisell/lib/OrganizationUnitInfo.php`
--

module Asterisell.OrganizationHierarchy (
  Extensions,
  ParentHiearchy,
  DirectPriceCategories,
  ParentIdHierarchy,
  DirectPriceCategory,
  Info(..),
  UserId,
  info_getUnitIdByInternalName,
  info_getDataInfoForUnitId,
  info_getDataInfoParent,
  info_getParentId,
  info_getDirectPriceCategory,
  info_getPriceCategoryId,
  info_getDataInfoForExtensionCode,
  info_getParentIdHierarchy,
  info_getBillableDataInfo,
  info_getRateCategoryId,
  info_getFullName,
  info_getFullNameAfterUnitId, 
  info_getAllActiveExtensions,
  info_empty,
  info_getDataInfoParentHierarchy,
  info_getParentHiearchyIds,
  info_isOrganizationToIgnore,
  info_getActualExtensionsToIgnore,
  info_getActualDataInfoToIgnore,
  info_fromDataInfoToExtensions,
  info_notIgnoreAnymoreTheseExtensions,
  info_notIgnoreAnymoreThisExtension,
  extensionToIgnoreId,
  UnitId,
  DataInfo(..),
  extensions_load,
  testWithDataImportedFromPHP,
  PriceCategoryId,
  IsApplicationError,
  IsStrictResult,
  info_respectCodeContracts,
  extensionCodes_extractFromUserSpecification,
  hackedAccount,
  User(..),
  UserInfo,
  userInfo_load,
  UnitType(..),
  unitType_load,
  UnitTypeInfo,
  UnitTypeFromInternalName,
  DefaultUnitType(..),
  unitType_defaultId,
  PartyTag(..),
  PartyTagInfo,
  partyTagInfo_load,
  userRole_load,
  userRole_defaultId,
  DefaultUserRole(..),
  UserRoleIdFromInternalName,
  userInfo_logins,
  organizationUnit_tfields,
  organizationUnitStruct_tfields,
  user_tfields,
  organizationHierarchy_test
) where

import Asterisell.Trie
import Asterisell.Utils
import Asterisell.Error
import Asterisell.DB
import Asterisell.Params

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as IO
import qualified Data.ByteString as BS

import qualified Data.Trie.BigEndianPatricia.Base as Trie
import qualified Data.Trie.BigEndianPatricia.Internal as TrieInternal
import qualified Data.Trie.BigEndianPatricia.Convenience as Trie

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq
import Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set as Set

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import Data.Time.Calendar
import Data.List as L
import Data.Maybe
import Debug.Trace
import qualified System.IO as SIO
import Control.Monad (when)
import Text.Heredoc

import System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S
import qualified Data.Vector as V
import Control.Monad

import Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import Text.Heredoc

import qualified Test.HUnit as HUnit
import qualified Test.HUnit.Base as HUnit
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

-- ----------------
-- Basic data types

-- | A unique identifier for ar_organization_unit.id.
--
type UnitId = Int

type UserId = Int

type DataInfoId = Int

type PriceCategoryId = Int

-- | The head is the root organization id, and then there is the first parent,
--   until the final child.
type ParentIdHierarchy = [UnitId]

-- | An identifier shared with PHP code,
--   for identifying hacked accounts.
--
--   NOTE: if you change this code, update also the PHP code.
hackedAccount :: T.Text
hackedAccount = "__hacked__account__"

-- | Info for Organizations and Extensions, at a certain point in time.
--   The info is valid from the specified date, until there is another info
--   on the same unit_id.
--
--   NOTE: not all the fields can vary on the time, it depends from the database schema,
--   from wich this data structure is derived.
--
data DataInfo
  = DataInfo {
      unit_id :: !UnitId,
      -- ^ the subject of the info

      unit_internalName :: !(Maybe T.Text),
      unit_internalName2 :: !(Maybe T.Text),
      unit_internalChecksum1 :: !(Maybe BS.ByteString),
      unit_internalChecksum2 :: !(Maybe BS.ByteString),
      unit_internalChecksum3 :: !(Maybe BS.ByteString),
      unit_internalChecksum4 :: !(Maybe BS.ByteString),
      unit_internalChecksum5 :: !(Maybe BS.ByteString),

      structure_from :: !LocalTime,
      -- ^ from when the info replace the old info, on the same unit_id, and it became valid

      unitType_id :: !Int,
      unitType_name :: !(Maybe T.Text),
      unitType_shortCode :: !(Maybe T.Text),
      unitType_internalName :: !(Maybe T.Text),

      structure_id :: !DataInfoId,
      structure_parentUnitId :: !(Maybe UnitId),
      structure_exists :: !Bool,
      structure_rateCategoryId :: !(Maybe PriceCategoryId),
      structure_partyId :: !(Maybe Int),
      structure_extensionCodes :: !(Maybe T.Text),
      structure_extensionName :: !(Maybe T.Text),
      structure_extensionUserCode :: !(Maybe T.Text),

      rateCategory_name :: !(Maybe T.Text),
      rateCategory_internalName :: !(Maybe T.Text),

      party_name :: !(Maybe T.Text),
      party_compactName :: !(Maybe T.Text),
      party_isBillable :: !Bool,
      party_isActive :: !Bool,
      party_resellerId :: !(Maybe Int),
      party_externalCRMCode :: !(Maybe T.Text)

    }
 deriving(Eq, Ord, Show, Generic, NFData)

-- | Used for accessing in an efficent way the organization info.
--   The Int key is the unit_id of the organization.
--   The value is the list of associated DataInfo, in reverse order of structure_from time.
--   This data structure is efficient if an organization does not change too much hierarchy structure over time,
--   and one is interested into more recent values.
type Organizations = IMap.IntMap [DataInfo]

type ExtensionExists = Bool

-- | Used for searching the extensions.
--   The key is the extension code, for fast lookup.
--
type Extensions = Trie [DataInfo]

-- | Fast access to direct price-categories assignments,
--   used in bundle-rates, for discovering which
--   unit-id have a bundle-rate state.
--   DataInfo are inserted in the list in reverse order   of date.
--   Map a price category, to a list of possibile units that can be directly assigned to it.
type DirectPriceCategories = IMap.IntMap [DataInfo]

-- | Info used for retrieving in a fast way info about organizations.
--   These data structures are efficient to use, only if there are not too much
--   changes in the history of organizations, and extensions.
--
data Info
  = Info {
      info_extensions :: Extensions
    , info_organizations :: Organizations
    -- ^ organizatiotns retrieved by Id
    , info_organizationsByInternalName :: Map.Map T.Text UnitId
    -- ^ organizations retrieved by internal_name
    , info_organizationsByInternalName2 :: Map.Map T.Text UnitId
    -- ^ organizations retrieved by internal_name2
    , info_organizationIdsThatCanBeBillable :: [UnitId]
    -- ^ organization id that can be billable at a certain date
    , info_maybeRootOrganizations :: [UnitId]
    -- ^ organizations that can be root organizations, now or in the past
    , info_maybeOrganizationToIgnore :: Maybe UnitId
    -- ^ associated to `organizationToIgnore`
    , info_directPriceCategories :: DirectPriceCategories
   } deriving(Show)

info_empty :: Info
info_empty = Info {
    info_extensions = trie_empty
  , info_organizations = IMap.empty
  , info_organizationsByInternalName = Map.empty
  , info_organizationsByInternalName2 = Map.empty
  , info_organizationIdsThatCanBeBillable = []
  , info_maybeRootOrganizations = []
  , info_maybeOrganizationToIgnore = Nothing
  , info_directPriceCategories = IMap.empty
}

info_respectCodeContracts :: Info -> CallDate -> Either String ()
info_respectCodeContracts info rateFromDate
  = case (testIntMapWithDataInfo $ info_organizations info) of
      False -> Left $ dbc_error "ohi1"
      True -> 
       case (testIntMapWithDataInfo $ info_directPriceCategories info) of
         False -> Left $ dbc_error "ohi2"
         True ->
           case thereAreUniqueExtensions of
             Nothing -> Right ()
             Just err -> Left err

 where

   testIntMapWithDataInfo :: IMap.IntMap [DataInfo] -> Bool
   testIntMapWithDataInfo map1
     = let map2 = IMap.map (\l1 -> isDescendingOrder $ L.map structure_from l1) map1
       in  L.all id (L.map snd $ IMap.toList map2)

   thereAreUniqueExtensions :: Maybe String
   thereAreUniqueExtensions
     = L.foldl' isUnique Nothing $ Trie.keys $ info_extensions info

   isUnique pastError extension
     = case (info_getDataInfoForExtensionCode info extension rateFromDate) of
         Left err -> Just $ show $ asterisellError_toException $ err 
         Right Nothing -> pastError -- ^ it can be an extension disactivate at this date
         Right (Just _) -> pastError 

-- --------------------------------------------
-- Import from DB

-- | Load extension data from the DB.
--   Create also (if specified) the organization to ignore.
extensions_load :: DB.MySQLConn -> Bool -> Maybe T.Text -> IO Info
extensions_load conn isDebugMode maybeOrganizationToIgnore = do
  case maybeOrganizationToIgnore of
    Nothing -> return ()
    Just o -> do _ <- extensionToIgnoreId conn o
                 return ()

  let q = [str|SELECT ar_organization_unit.id
              |, ar_organization_unit.internal_name
              |, ar_organization_unit.internal_name2
              |, ar_organization_unit.internal_checksum1
              |, ar_organization_unit.internal_checksum2
              |, ar_organization_unit.internal_checksum3
              |, ar_organization_unit.internal_checksum4
              |, ar_organization_unit.internal_checksum5
              |, ar_organization_unit_type.id
              |, ar_organization_unit_type.name
              |, ar_organization_unit_type.short_code
              |, ar_organization_unit_type.internal_name
              |, ar_organization_unit_has_structure.id
              |, ar_organization_unit_has_structure.ar_parent_organization_unit_id
              |, ar_organization_unit_has_structure.from
              |, ar_organization_unit_has_structure.exists
              |, ar_organization_unit_has_structure.ar_rate_category_id
              |, ar_organization_unit_has_structure.ar_party_id
              |, ar_organization_unit_has_structure.extension_codes
              |, ar_organization_unit_has_structure.extension_name
              |, ar_organization_unit_has_structure.extension_user_code
              |, ar_rate_category.short_description
              |, ar_rate_category.internal_name
              |, ar_party.name
              |, ar_party.compact_name
              |, ar_party.is_billable
              |, ar_party.is_active
              |, ar_party.ar_reseller_id
              |, ar_party.external_crm_code
              |FROM ar_organization_unit_has_structure
              |LEFT JOIN ar_organization_unit ON ar_organization_unit_has_structure.ar_organization_unit_id = ar_organization_unit.id
              |LEFT JOIN ar_organization_unit_type ON ar_organization_unit_has_structure.ar_organization_unit_type_id = ar_organization_unit_type.id
              |LEFT JOIN ar_rate_category ON ar_organization_unit_has_structure.ar_rate_category_id = ar_rate_category.id
              |LEFT JOIN ar_party ON ar_organization_unit_has_structure.ar_party_id = ar_party.id
              |ORDER BY ar_organization_unit_has_structure.from
              |]

  (_, inS) <- DB.query_ conn (DB.Query q)
  (!info, !errors) <- S.fold (addExtensionRecord maybeOrganizationToIgnore) (info_empty, Set.empty) inS
  case Set.null errors of
    True
      -> return info
    False
        -> throwIO $ AsterisellException $ "There are extensions configured not correctly. " ++ (L.concatMap (\t -> t ++ "\n") (Set.toList errors))

-- | The UnitId of the extension to ignore.
--   Create one if it does not exist.
extensionToIgnoreId :: DB.MySQLConn -> ExtensionAsText ->  IO UnitId
extensionToIgnoreId conn extensionToIgnore1 = do
  mi <- isThereYetOrganizationToIgnore
  case mi of
    Just i -> return i
    Nothing -> createOrganizationToIgnore

 where

  extensionToIgnore =
    if (T.null extensionToIgnore1) then "Ignored accounts" else extensionToIgnore1

  isThereYetOrganizationToIgnore = do 
     (_, inS) <- DB.query conn "SELECT id FROM ar_organization_unit WHERE internal_name = ?" [toDBText extensionToIgnore]
     inS' <- S.toList inS
     case inS' of
       [[i]] -> return $ Just $ fromDBInt i
       _ -> return Nothing

  createOrganizationToIgnore = do
      let unitRec
              = trecord_setMany
                  (trecord_empty)
                  [("internal_name", toDBText extensionToIgnore)
                  ,("automatically_managed_from", toDBInt64 1)
                  ]

      unitId <- trecord_save conn "ar_organization_unit" unitRec Nothing

      !billedFromDate <- defaultParams_unbilledCallsFrom conn "07726"

      let partyRec
              = trecord_setMany
                  (trecord_empty)
                  [("name", toDBText extensionToIgnore)
                  ,("is_billable", toDBBool True)
                  ]

      partyId <- trecord_save conn "ar_party" partyRec Nothing

      (_, !localUnitType) <- unitType_load conn False
      let !rootUnitTypeId = unitType_defaultId localUnitType UnitType_root
 
      let recStructure
              = trecord_setMany
                  (trecord_empty)
                  [("ar_organization_unit_id", toDBInt64 unitId)
                  ,("ar_organization_unit_type_id", toDBInt64 rootUnitTypeId) 
                  ,("from", toDBLocalTime billedFromDate)
                  ,("exists", toDBBool True)
                  ,("ar_party_id", toDBInt64 partyId)
                  ,("ar_rate_category_id", DB.MySQLNull) -- NOTE: these organizations are not billable
                  ]

      _ <- trecord_save conn "ar_organization_unit_has_structure" recStructure Nothing

      return unitId

addExtensionRecord :: Maybe T.Text -> (Info, ConfigurationErrors) -> [DB.MySQLValue] -> (Info, ConfigurationErrors)
addExtensionRecord
  maybeOrganizationToIgnore
  (info1, errors1)
  [ unitId
  , unitInternalName
  , unitInternalName2
  , unitChecksum1
  , unitChecksum2
  , unitChecksum3
  , unitChecksum4
  , unitChecksum5
  , typeId
  , typeName
  , typeShortCode
  , typeInternalName
  , structureId
  , parentId
  , fromDate
  , existsFlag
  , rateCategoryId
  , partyId
  , extensionCodes
  , extensionName
  , extensionUserCode
  , rateCategoryShortDescr
  , rateCategoryInternalName
  , partyName
  , partyCompactName
  , partyIsBillable1
  , partyIsActive1
  , partyResellerId
  , partyCRM
  ]
    = let partyIsBillable2 = case fromMaybeDBValue fromDBBool partyIsBillable1 of
                               Nothing -> False
                               Just v -> v

          partyIsActive2 = case fromMaybeDBValue fromDBBool partyIsActive1 of
                             Nothing -> False
                             Just v -> v

          value = DataInfo {
                      unit_id = fromDBInt unitId,
                      unit_internalName = fromMaybeDBValue fromDBText unitInternalName,
                      unit_internalName2 = fromMaybeDBValue fromDBText unitInternalName2,
                      unit_internalChecksum1 = fromMaybeDBValue fromDBByteString unitChecksum1,
                      unit_internalChecksum2 = fromMaybeDBValue fromDBByteString unitChecksum2,
                      unit_internalChecksum3 = fromMaybeDBValue fromDBByteString unitChecksum3,
                      unit_internalChecksum4 = fromMaybeDBValue fromDBByteString unitChecksum4,
                      unit_internalChecksum5 = fromMaybeDBValue fromDBByteString unitChecksum5,
                      unitType_id = fromDBInt typeId,
                      unitType_name = fromMaybeDBValue fromDBText typeName,
                      unitType_shortCode = fromMaybeDBValue fromDBText typeShortCode,
                      unitType_internalName = fromMaybeDBValue fromDBText typeInternalName,
                      structure_id = fromDBInt structureId,
                      structure_parentUnitId = fromMaybeDBValue fromDBInt parentId,
                      structure_from = fromDBLocalTime fromDate,
                      structure_exists = fromDBBool existsFlag,
                      structure_rateCategoryId = fromMaybeDBValue fromDBInt rateCategoryId,
                      structure_partyId = fromMaybeDBValue fromDBInt partyId,
                      structure_extensionCodes = fromMaybeDBValue fromDBText extensionCodes,
                      structure_extensionName = fromMaybeDBValue fromDBText extensionName,
                      structure_extensionUserCode = fromMaybeDBValue fromDBText extensionUserCode ,
                      rateCategory_name = fromMaybeDBValue fromDBText rateCategoryShortDescr,
                      rateCategory_internalName = fromMaybeDBValue fromDBText rateCategoryInternalName ,
                      party_name = fromMaybeDBValue fromDBText partyName,
                      party_compactName = fromMaybeDBValue fromDBText partyCompactName,
                      party_isBillable = partyIsBillable2,
                      party_isActive = partyIsActive2,
                      party_resellerId = fromMaybeDBValue fromDBInt partyResellerId,
                      party_externalCRMCode = fromMaybeDBValue fromDBText partyCRM
                    }

          (!info2, !errors2) = info_addDataInfo (info1, errors1) value
          !info3 = case maybeOrganizationToIgnore of
                     Nothing -> info2
                     Just organizationToIgnore
                       -> case unit_internalName value of
                            Nothing -> info2
                            Just n -> case n == organizationToIgnore of
                                        False -> info2
                                        True -> info2 { info_maybeOrganizationToIgnore = Just $ unit_id value }
     in (info3, errors2)

-- | Extract the codes separated from ",", and manage quoted value: "\,"
extensionCodes_extractFromUserSpecification :: T.Text -> [Extension]
extensionCodes_extractFromUserSpecification userCodes
  = L.map fromStringToByteString $ extractResult $ L.foldl' proc ([], [], False, True) (T.unpack userCodes)

 where

  proc (res, str, isThereQuotation, skipSpaces) ch
    = if isThereQuotation
      then (res, str ++ [ch], False, False)
      else if (skipSpaces && ch == ' ')
           then (res, str, False, True)
           else if ch == ','
                then (res ++ [str], [], False, True)
                else if ch == '\\'
                     then (res, str, True, False)
                     else (res, str ++ [ch], False, False)

  extractResult (r1, r2, _, _) = r1 ++ [r2]

-- | Add an info about organizations, maintaining updated the global Info data structure.
--   @require: DataInfo is sent in order of ar_organization_unit_has_structure.from
info_addDataInfo :: (Info, ConfigurationErrors) -> DataInfo -> (Info, ConfigurationErrors)
info_addDataInfo (info, errors) dataInfo
  = let extensions1
          = info_extensions info

        (addedExtensions, addedErrors)
          = case (structure_extensionCodes dataInfo) of
              Nothing
                -> (extensions1, errors)
              Just codes
                -> L.foldl' (\(trie1, errors1) extension
                                  -> let code =  extension_toExtensionCode extension
                                     in  (trie_insertWith trie1 code [dataInfo] (flip (++)), errors1)

                            ) (extensions1, errors) (extensionCodes_extractFromUserSpecification codes)

        unitId
          = unit_id dataInfo

        imapInsert k v m
          = IMap.insertWith (\newValue oldValue -> newValue ++ oldValue) k [v] m
            -- NOTE: if newValues are sent in ascending order of call-date,
            -- then they are stored in the list in reverse order of call-date.

        addedOrganizations
          = imapInsert unitId dataInfo (info_organizations info)

        -- add to its parent this child
        addedMaybeRootOrganizations
          = case structure_parentUnitId dataInfo of
              Nothing
                -> info_maybeRootOrganizations info ++ [unitId]
              Just parentId
                -> info_maybeRootOrganizations info

        addedOrganizationIdsThatCanBeBillable
          = if party_isBillable dataInfo && structure_exists dataInfo
            then info_organizationIdsThatCanBeBillable info ++ [unitId]
            else info_organizationIdsThatCanBeBillable info

        addedDirectPriceCategories
          = case structure_rateCategoryId dataInfo of
              Nothing
                -> info_directPriceCategories info
              Just priceId
                -> imapInsert priceId dataInfo (info_directPriceCategories info)

        addedOrganizationsByInternalName
          = case unit_internalName dataInfo of
              Nothing
                -> info_organizationsByInternalName info 
              Just n
                -> Map.insert n unitId (info_organizationsByInternalName info)
                   -- NOTE: the DB schema enforces they are unique

        addedOrganizationsByInternalName2
          = case unit_internalName2 dataInfo of
              Nothing
                -> info_organizationsByInternalName2 info 
              Just n
                -> Map.insert n unitId (info_organizationsByInternalName2 info)
                   -- NOTE: the DB schema enforces they are unique

    in (info {
           info_extensions = addedExtensions
         , info_organizations = addedOrganizations
         , info_organizationsByInternalName = addedOrganizationsByInternalName
         , info_organizationsByInternalName2 = addedOrganizationsByInternalName2
         , info_maybeRootOrganizations = addedMaybeRootOrganizations
         , info_organizationIdsThatCanBeBillable = addedOrganizationIdsThatCanBeBillable
         , info_directPriceCategories = addedDirectPriceCategories
         }
       , addedErrors
       )

-- ----------------------------------------------
-- Retrieve the best info according the hierarchy

-- | List of parents, from root to child.
type ParentHiearchy = [DataInfo]

getBestDataInfoRespectingDateAndExists :: [DataInfo] -> LocalTime -> Maybe DataInfo
getBestDataInfoRespectingDateAndExists infos date
  = let validDate info = structure_from info <= date
    in case L.find validDate infos of
         Nothing
           -> Nothing
         Just validInfo
           -> case structure_exists validInfo of
                 True -> Just validInfo
                 False -> Nothing

-- | True for returning DataInfo existing at the specified calldate.
--   False for returning the first DataInfo in the future, in case there is no strict result.
--   In case of services, there can be services with a starting date that is before the creation of the unit-id.
type IsStrictResult = Bool

info_getDataInfoForUnitId :: Info -> UnitId -> LocalTime -> IsStrictResult -> Maybe DataInfo
info_getDataInfoForUnitId info unitId date isStrictResult
  = let
        organizations = info_organizations info
    in case IMap.lookup unitId organizations of
         Nothing
           -> Nothing
         Just infos
           -> -- trace ("unitId: " ++ show unitId ++ ", at date: " ++ show date ++ ", on info: " ++ show info) $ getBestDataInfoRespectingDateAndExists infos date
             case getBestDataInfoRespectingDateAndExists infos date of
               Just r
                 -> Just r
               Nothing
                 -> case isStrictResult of
                      True
                        -> Nothing
                      False
                        -> if L.null infos then Nothing else (Just $ L.last infos)

info_getDataInfoForExtensionCode :: Info -> Extension -> LocalTime -> Either AsterisellError (Maybe (DataInfo, IsExtensionalMatch))
info_getDataInfoForExtensionCode info extension date
  = let trie = info_extensions info
    in  case trie_match trie extension of
          Nothing
            -> Right Nothing
          Just (_, isExtensionalMatch, dataInfos1)
            -> case L.filter (\i1 -> let unitId = unit_id i1
                                     in case info_getDataInfoForUnitId info unitId date True of
                                           Nothing
                                             -> False
                                                -- do not accept because in the meantime the extension was removed from the unit-id
                                           Just i2
                                             -> structure_id i1 == structure_id i2
                                                -- accept only if it is the most updated piece of info,
                                                -- otherwise there is something of better later during scan of the list
                                                -- associated to the same unit-id, but to a better DataInfo
                                                -- @ensure: there is later a result in the list
                             ) dataInfos1 of
                 [] -> Right Nothing
                 [r] -> Right $ Just (r, isExtensionalMatch)
                 (r1:r2:r) -> Left $ createError
                                       Type_Error
                                       Domain_VOIP_ACCOUNTS
                                       ("extension code conflict - " ++ fromByteStringToString extension)
                                       ("The extension code \"" ++ fromByteStringToString extension ++ "\", at date " ++ showLocalTime date ++ ", is associated both to organization with id " ++ show (unit_id r1) ++ ", and to organization with id " ++ show (unit_id r2))
                                       ("The calls using this extension code will be not rated.")
                                       ("Fix the associations between extension codes, and organizations, and rerate the calls.")

info_getUnitIdByInternalName :: Info -> T.Text -> Maybe UnitId
info_getUnitIdByInternalName info n
    = Map.lookup n (info_organizationsByInternalName info)

info_getDataInfoParent :: Info -> DataInfo -> LocalTime -> IsStrictResult -> Maybe DataInfo
info_getDataInfoParent info unitInfo date isStrict
  = case structure_parentUnitId unitInfo of
      Nothing
        -> Nothing
      Just parentId
        -> info_getDataInfoForUnitId info parentId date isStrict

info_getParentId :: Info -> UnitId -> LocalTime -> Maybe UnitId
info_getParentId info unitId date
  = case info_getDataInfoForUnitId info unitId date True of
      Nothing
        -> Nothing
      Just dataInfo
        -> case structure_exists dataInfo of
             True -> structure_parentUnitId dataInfo
             False -> Nothing
{-# INLINE info_getParentId #-}

-- | All the parents, from the main root, to the specific data-info.
--
info_getDataInfoParentHierarchy :: Info -> DataInfo -> LocalTime -> IsStrictResult -> ParentHiearchy
info_getDataInfoParentHierarchy info unitInfo date isStrict
  = case info_getDataInfoParent info unitInfo date isStrict of
      Nothing
        -> [unitInfo]
      Just parentInfo
        -> info_getDataInfoParentHierarchy info parentInfo date isStrict ++ [unitInfo]

-- | An identifier for a complete organization hierarchy.
--   Something like "/id1/id2/id3/" where "id1" is the main root, and "id3" the last child.
--
info_getParentHiearchyIds :: ParentHiearchy -> T.Text
info_getParentHiearchyIds infos
  = T.pack $ "/" ++ L.intercalate "/" (L.map (\d -> show $ unit_id d) infos) ++ "/"

-- | True if the organization is the one indicated in `info_maybeOrganizationToIgnore`
info_isOrganizationToIgnore :: Info -> ParentIdHierarchy -> Bool
info_isOrganizationToIgnore info ids
    = case info_maybeOrganizationToIgnore info of
        Nothing -> False
        Just idToIgnore -> L.any ((==) idToIgnore) ids

-- | An identifier for a complete organization hierarchy.
--   Something like "/id1/id2/id3/" where "id1" is the main root, and "id3" the last child.
--
info_getParentIdHierarchy :: ParentHiearchy -> ParentIdHierarchy
info_getParentIdHierarchy infos
  = L.map (\d -> unit_id d) infos
{-# INLINE info_getParentIdHierarchy #-}

info_getBillableDataInfo :: ParentHiearchy -> Maybe DataInfo
info_getBillableDataInfo infos
  = let isBillable d = (party_isBillable d)
    in  L.find isBillable (L.reverse infos)

info_getRateCategoryId :: ParentHiearchy -> Maybe Int
info_getRateCategoryId infos
  = case L.find (\d -> isJust $ structure_rateCategoryId d) (L.reverse infos) of
      Nothing -> Nothing
      Just dataInfo -> structure_rateCategoryId dataInfo

info_getDirectPriceCategory :: Info -> UnitId -> LocalTime -> Maybe PriceCategoryId
info_getDirectPriceCategory info unitId localTime
  = case info_getDataInfoForUnitId info unitId localTime True of
      Nothing
        -> Nothing
      Just dataInfo
        -> structure_rateCategoryId dataInfo
{-# INLINE info_getDirectPriceCategory #-}

-- | The first direct price-category assignment, following the parent hierarchy.
type DirectPriceCategory = (PriceCategoryId, UnitId)

-- | Return the first direct price-category assignment following the parent hierarchy.
info_getPriceCategoryId :: Info -> UnitId -> LocalTime -> Maybe DirectPriceCategory 
info_getPriceCategoryId info unitId localTime
  = case info_getDataInfoForUnitId info unitId localTime True of
      Nothing -> Nothing
      Just dataInfo
          -> case structure_exists dataInfo of
             False
               -> Nothing
             True
               -> case structure_rateCategoryId dataInfo of
                       Nothing
                         -> case structure_parentUnitId dataInfo of
                                    Just parentId -> info_getPriceCategoryId info parentId localTime
                                    Nothing -> Nothing
                       Just priceCategoryId -> Just (priceCategoryId, unitId)

-- | Get the name of the organization, starting from the specified level.
--   Use 0 for showing all the organization hierachy.
--   For example if we have "a/b/c/d", we can show only "c/d" if "a/b" is implicit (implicitLevel == 2)
--
info_getFullName :: ParentHiearchy -> Int -> Bool -> Bool -> Bool -> String
info_getFullName infos implicitLevel reverseOrder showExtensionCode useOnlyExtensionShortCode
  = let getName d
          = let typeCode
                  = case unitType_shortCode d of
                      Nothing -> ""
                      Just c -> T.unpack c ++ ": "

                extensionUserCode
                  = case structure_extensionUserCode d of
                      Nothing -> ""
                      Just n -> T.unpack n

                name
                  = case party_name d of
                      Just n
                        -> T.unpack n
                      Nothing
                        -> case structure_extensionName d of
                             Just extensionName
                               -> if showExtensionCode
                                  then if useOnlyExtensionShortCode
                                       then extensionUserCode
                                       else if not (L.null extensionUserCode) && not (extensionUserCode == (T.unpack extensionName))
                                            then T.unpack extensionName ++ " (" ++ extensionUserCode ++ ")"
                                            else T.unpack extensionName
                                  else T.unpack extensionName
                             Nothing
                               ->  "id-" ++ show (unit_id d)
            in typeCode ++ name

        names = L.drop implicitLevel $ L.map getName infos

        names2 = if reverseOrder
                 then L.reverse names
                 else names

    in  L.intercalate " / " names2

-- | Get the name of the organization, starting after the specified unit-id inside the hirearchy.
--   For example if we have "a/b/c/d", we can show only "c/d" if "a/b" is implicit.
--
info_getFullNameAfterUnitId :: ParentHiearchy -> UnitId -> Bool -> Bool -> Bool -> String
info_getFullNameAfterUnitId infos unitId reverseOrder showExtensionCode useOnlyExtensionShortCode
  = let 
        infos2 = 
          case L.dropWhile (\d -> (unit_id d) /= unitId) infos of
            [] -> []
            (_:dd) -> dd
    in  info_getFullName infos2 0 reverseOrder showExtensionCode useOnlyExtensionShortCode

-- ----------------------------
-- Queries on global data

-- | All the extensions codes that are active at the specified date.
--   NOTE: an extension code can contain "X" and "*" pattern matching,
--   so it can match/represent many different extensions, and so the
--   result of this method had to take in consideration this.
--   NOTE: in the result there are also the extensions to ignore, because they are valid match.
info_getAllActiveExtensions :: Info -> CallDate -> Set.Set ExtensionAsText
info_getAllActiveExtensions info fromDate
  = L.foldl' f Set.empty $ Trie.keys $ info_extensions info
 where

   f s1 extension
     = case (info_getDataInfoForExtensionCode info extension fromDate) of
         Left err -> pError $ show $ asterisellError_toException $ err 
         Right Nothing -> s1 
         Right (Just (dataInfo, _))
           -> case structure_exists dataInfo of
                True -> Set.insert (fromByteStringToText extension) s1
                False -> s1

-- ----------------------------
-- Organization info to ignore

-- | Get DataInfo for which the last (most recent) entry shows they are to ignore.
--   To use during importing of organization, and not during rating, because during rating
--   it must be used the CDR calldate, and because this method is more an hack, than a reliable
--   way for classifying extensions.
--   @require there are no extensions specified directly in the organization-to-ignore, but only in its children
info_getActualDataInfoToIgnore :: Info -> Set.Set DataInfo
info_getActualDataInfoToIgnore info
  = case info_maybeOrganizationToIgnore info of
      Nothing  -> Set.empty
      Just id1 -> repeatScan (Set.singleton id1, Set.empty)
 where

  repeatScan :: (Set.Set UnitId, Set.Set DataInfo) -> Set.Set DataInfo
  repeatScan (unitIds1, extensions1)
    = let (unitIds2, extensions2)
              = L.foldl' scanDataInfo (unitIds1, extensions1) $ L.map head $ IMap.elems (info_organizations info)
      in case (Set.size unitIds1 == Set.size unitIds2) of
           True -> extensions2
           False -> repeatScan (unitIds2, extensions2)

  scanDataInfo :: Foldl DataInfo (Set.Set UnitId, Set.Set DataInfo)
  scanDataInfo pastResult@(unitIds1, extensions1) info1
    = let unitId = unit_id info1
      in case structure_parentUnitId info1 of
           Nothing -> pastResult
           Just parentId
             -> case Set.member parentId unitIds1 of
                  True -> ( Set.insert unitId unitIds1
                          , Set.insert info1 extensions1)
                  False -> pastResult

-- | Get extensions for which the last (most recent) entry shows they are to ignore.
--   @require same constraints of `info_getActualDataInfoToIgnore`
info_getActualExtensionsToIgnore :: Info -> Set.Set ExtensionAsText
info_getActualExtensionsToIgnore info
  = let dataInfo = info_getActualDataInfoToIgnore info
    in info_fromDataInfoToExtensions info dataInfo

-- | Get extensions for which the last (most recent) entry shows they are to ignore.
--   @require same constraints of `info_getActualDataInfoToIgnore`
info_fromDataInfoToExtensions :: Info -> Set.Set DataInfo -> Set.Set ExtensionAsText
info_fromDataInfoToExtensions info dataInfo
  = Set.foldl'
          (\s1 info1 -> case structure_extensionCodes info1 of
                          Just codes -> Set.union s1 (Set.fromList $ L.map fromByteStringToText $ extensionCodes_extractFromUserSpecification codes)
                          Nothing -> s1
          ) Set.empty dataInfo

-- | Delete an extension to ignore match.
--   This does not invalidate CDRS because:
--   * CDRS to ignore are not inserted in the `ar_cdr` table, and so they are not pointing to this extension
--   * in case of rerate of already billed CDRS, the info about an extension to ignore had to be replaced with a valid extension code in any case
--   @ensure: after applying this command, Info had to be load again, because it is not any more updated
info_notIgnoreAnymoreTheseExtensions
    :: DB.MySQLConn
    -> Info
    -> Maybe UnitId -- ^ in case filter on this parent unitId
    -> [ExtensionAsText]
    -> IO ()
info_notIgnoreAnymoreTheseExtensions conn info maybeParentId extensionsToEnable
  = Prelude.mapM_ enableExtension extensionsToEnable
 where

  enableExtension code
    = case (trie_lookup (info_extensions info) (extension_toExtensionCode (fromTextToByteString code))) of
        Nothing
          -> do return ()
        Just dataInfo
          -> do let i = structure_id $ head dataInfo
                info_notIgnoreAnymoreThisExtension conn maybeParentId i
                return ()

-- | Delete an extension to ignore match.
--   This does not invalidate CDRS because:
--   * CDRS to ignore are not inserted in the `ar_cdr` table, and so they are not pointing to this extension
--   * in case of rerate of already billed CDRS, the info about an extension to ignore had to be replaced with a valid extension code in any case
--   @ensure: after applying this command, Info had to be load again, because it is not any more updated
info_notIgnoreAnymoreThisExtension
    :: DB.MySQLConn
    -> Maybe UnitId -- ^ in case filter on this parentId
    -> UnitId
    -> IO ()
info_notIgnoreAnymoreThisExtension conn Nothing unitId = do

  let q1 = [str| DELETE FROM ar_organization_unit_has_structure
               | WHERE ar_organization_unit_id = ?
               |]

  _ <- DB.execute conn q1 [toDBInt64 unitId]

  let q2 = [str| DELETE FROM ar_organization_unit
               | WHERE id = ?
               |]

  _ <- DB.execute conn q2 [toDBInt64 unitId]

  return ()

info_notIgnoreAnymoreThisExtension conn (Just parentId) unitId = do

  let q0 = [str| SELECT id
               | FROM ar_organization_unit_has_structure
               | WHERE ar_organization_unit_id = ?
               | AND ar_parent_organization_unit_id = ?
               | LIMIT 1
               |]

  (_, inS) <- DB.query conn q0 [toDBInt64 unitId, toDBInt64 parentId]

  rs <- S.toList inS
  case rs of
    [] -> return ()
    _ -> do
      let q1 = [str| DELETE FROM ar_organization_unit_has_structure
                   | WHERE ar_organization_unit_id = ?
                   |]

      _ <- DB.execute conn q1 [toDBInt64 unitId]

      let q2 = [str| DELETE FROM ar_organization_unit
                   | WHERE id = ?
                   |]

      _ <- DB.execute conn q2 [toDBInt64 unitId]

      return ()

-- ----------------
-- Users management

data User
       = User {
            user_id :: !Int
         ,  user_partyId :: !(Maybe Int)
         ,  user_unitId :: !(Maybe Int)
         ,  user_login :: !T.Text
         ,  user_passwordHash :: !(Maybe BS.ByteString)
            -- ^ the MD5 hash of the password
         ,  user_isEnabled :: !Bool
         ,  user_isAdmin :: !Bool
         }
    deriving (Eq, Ord, Show, Generic, NFData)

-- | From user_unitId to User
type UserInfo = MultiMap (Maybe UnitId) User 

-- | Return all the logins.
userInfo_logins :: UserInfo -> Set.Set T.Text
userInfo_logins info = Set.fromList $ L.map user_login $ L.concat $ Map.elems info

-- | Load extension data from the DB.
userInfo_load :: DB.MySQLConn -> Bool -> IO UserInfo
userInfo_load conn isDebugMode = do
  let 
      q = [str|SELECT id
              |, ar_party_id
              |, ar_organization_unit_id
              |, login
              |, password
              |, is_enabled
              |, is_root_admin
              |FROM ar_user
              |ORDER BY id
              |]

  (_, inS) <- DB.query_ conn (DB.Query q)
  !info <- S.fold addUserRecord (Map.empty) inS
  return info
 where

  addUserRecord :: UserInfo -> [DB.MySQLValue] -> UserInfo
  addUserRecord
    info1
    [ userId
    , partyId
    , unitId
    , login
    , password
    , isEnabled
    , isRootAdmin
    ]
      = let

            maybeUnitId = fromMaybeDBValue fromDBInt unitId

            value = User {
                        user_id = fromDBInt userId,
                        user_partyId = fromMaybeDBValue fromDBInt partyId,
                        user_unitId = maybeUnitId,
                        user_login = fromDBText login,
                        user_passwordHash = fromMaybeDBValue fromDBByteString password,
                        user_isEnabled = fromDBBool isEnabled,
                        user_isAdmin = fromDBBool isRootAdmin
                    }
  
        in multiMap_add info1 maybeUnitId value

-- ----------
-- User roles

data DefaultUserRole
       = UserRole_admin
       | UserRole_user
       | UserRole_accountant
       | UserRole_notifiedForCriticalErrors
       | UserRole_notifiedForErrors
       | UserRole_notifiedForWarnings
      deriving(Eq, Ord, Show)


-- | From `ar_role.internal_name` to `ar_role.id`.
type UserRoleIdFromInternalName = Map.Map T.Text Int 

-- | Retrieve the default organization type.
--   IMPORTANT: this code must be mantained in synchro with `ArOrganizationUnitType` on the PHP side. 
userRole_defaultId :: UserRoleIdFromInternalName -> DefaultUserRole -> Int
userRole_defaultId m tn = fromJust $ Map.lookup (tnn tn) m
 where

  tnn :: DefaultUserRole -> T.Text
  tnn UserRole_admin = "admin"
  tnn UserRole_user = "user"
  tnn UserRole_accountant = "accountant"
  tnn UserRole_notifiedForCriticalErrors = "notified_for_critical_errors"
  tnn UserRole_notifiedForErrors = "notified_for_errors"
  tnn UserRole_notifiedForWarnings = "notified_for_warnings"

userRole_load :: DB.MySQLConn -> Bool -> IO UserRoleIdFromInternalName
userRole_load conn isDebugMode = do
  let 
      q = [str|SELECT id, internal_name
              |FROM ar_role
              |WHERE internal_name IS NOT NULL
              |]

  (_, inS) <- DB.query_ conn (DB.Query q)
  !r <- S.fold addInfo Map.empty inS
  return r
 where

  addInfo :: UserRoleIdFromInternalName -> [DB.MySQLValue] -> UserRoleIdFromInternalName
  addInfo m1 [rid, rinternalName] = Map.insert (fromDBText rinternalName) (fromDBInt rid) m1

-- ----------------
-- Organization types

data UnitType
       = UnitType {
            utp_id :: !Int
         ,  utp_name :: !T.Text
         ,  utp_shortCode :: !(Maybe T.Text)
         ,  utp_internalName :: !(Maybe T.Text)
         }
    deriving (Eq, Ord, Show, Generic, NFData)

data DefaultUnitType
       = UnitType_extension
       | UnitType_customer
       | UnitType_office
       | UnitType_externalTelephoneNumber
       | UnitType_root
       | UnitType_organization
       | UnitType_system
      deriving(Eq, Ord, Show)


-- | From `unitType_id` to `UnitType`.
type UnitTypeInfo = IMap.IntMap UnitType 

type UnitTypeFromInternalName = Map.Map T.Text UnitType

-- | Retrieve the default organization type.
--   IMPORTANT: this code must be mantained in synchro with `ArOrganizationUnitType` on the PHP side. 
unitType_defaultId :: UnitTypeFromInternalName -> DefaultUnitType -> Int
unitType_defaultId m tn
  = utp_id $ fromJust $ Map.lookup (tnn tn) m
 where

  tnn :: DefaultUnitType -> T.Text
  tnn UnitType_extension = "extension"
  tnn UnitType_customer = "customer"
  tnn UnitType_office = "office"
  tnn UnitType_externalTelephoneNumber = "external"
  tnn UnitType_root = "root"
  tnn UnitType_organization = "org"
  tnn UnitType_system = "system"

-- | Load unit type data from the DB.
unitType_load :: DB.MySQLConn -> Bool -> IO (UnitTypeInfo, UnitTypeFromInternalName)
unitType_load conn isDebugMode = do
  let 
      q = [str|SELECT id
              |, name
              |, short_code
              |, internal_name
              |FROM ar_organization_unit_type
              |ORDER BY id
              |]

  (_, inS) <- DB.query_ conn (DB.Query q)
  !r <- S.fold addInfo (IMap.empty, Map.empty) inS
  return r
 where

  addInfo :: (UnitTypeInfo, UnitTypeFromInternalName) -> [DB.MySQLValue] -> (UnitTypeInfo, UnitTypeFromInternalName)
  addInfo
    (uti, utn)
    [ rid
    , rname
    , rshortCode
    , rInternalName
    ]
      = let unitType = UnitType {
                          utp_id = fromDBInt rid
                       ,  utp_name = fromDBText rname
                       ,  utp_shortCode = fromMaybeDBValue fromDBText rshortCode
                       ,  utp_internalName = fromMaybeDBValue fromDBText rInternalName
                       }

            
        in 
           ( IMap.insert (fromDBInt rid) unitType uti
           , case fromMaybeDBValue fromDBText rInternalName of
               Nothing -> utn
               Just n -> Map.insert n unitType utn)

-- -----------
-- Party Tags 

data PartyTag
       = PartyTag {
            partyTag_id :: !Int
         ,  partyTag_internalName :: !T.Text
         ,  partyTag_name :: !T.Text
         }
    deriving (Eq, Ord, Show, Generic, NFData)

-- | From internal name to PartyTag 
type PartyTagInfo = Map.Map T.Text PartyTag 

-- | Load extension data from the DB.
partyTagInfo_load :: DB.MySQLConn -> Bool -> IO PartyTagInfo
partyTagInfo_load conn isDebugMode = do
  let 
      q = [str|SELECT id
              |, internal_name
              |, name_for_customer
              |FROM ar_tag
              |]

  (_, inS) <- DB.query_ conn (DB.Query q)
  !info <- S.fold addRecord (Map.empty) inS
  return info
 where

  addRecord :: PartyTagInfo -> [DB.MySQLValue] -> PartyTagInfo
  addRecord
    info1
    [ tagId
    , internalName
    , nameForCustomer
    ]
      = let

            internalName' = fromDBText internalName

            value = PartyTag {
                        partyTag_id = fromDBInt tagId,
                        partyTag_internalName = internalName',
                        partyTag_name = fromDBText nameForCustomer
                    }
  
        in Map.insert internalName' value info1

-- ----------------
-- TRecord data

organizationUnit_tfields :: TFields
organizationUnit_tfields = TFields "ar_organization_unit" $
    [ TField "id" True True False
    , TField "internal_name" False True False
    , TField "internal_name2" False True False
    , TField "internal_checksum1" False True False
    , TField "internal_checksum2" False True False
    , TField "internal_checksum3" False True False
    , TField "internal_checksum4" False True False
    , TField "internal_checksum5" False True False
    , TField "export_code" False True False
    , TField "automatically_managed_from" False True False
    ]

organizationUnitStruct_tfields :: TFields
organizationUnitStruct_tfields = TFields "ar_organization_unit_has_structure" $
    [ TField "id" True True False
    , TField "ar_organization_unit_id" False True False
    , TField "ar_organization_unit_type_id" False True False
    , TField "ar_parent_organization_unit_id" False True False
    , TField "from" False True False
    , TField "exists" False True False
    , TField "ar_rate_category_id" False True False
    , TField "ar_party_id" False True False
    , TField "extension_codes" False True False
    , TField "extension_name" False True False
    , TField "extension_user_code" False True False
    ]

user_tfields :: TFields
user_tfields = TFields "ar_user" $
    [ TField "id" False True False
    , TField "ar_party_id" False True False
    , TField "ar_organization_unit_id" False True False
    , TField "login" False True False
    , TField "password" False True False
    , TField "clear_password_to_import" False True False
    , TField "is_enabled" False True False
    , TField "is_root_admin" False True False
    ]

-- ----------------
-- Regression tests

organizationHierarchy_test :: [HUnit.Test]
organizationHierarchy_test
  = [ t ["123"] "123"
    , t ["123", "45"] "123, 45"
    , t ["123", "45", "45X"] "123, 45,45X"
    , t ["123", "4,5", "1,2"] "123,4\\,5,1\\,2"
    , t ["123", ",4,5", "1,2,,"] "123,\\,4\\,5,1\\,2\\,\\,"
    ]
 where

   t :: [Extension] -> T.Text -> HUnit.Test
   t l s = HUnit.TestCase $ HUnit.assertEqual (T.unpack s) (l) (extensionCodes_extractFromUserSpecification s)


-- | PHP regression code will generate specific data,
--   that is imported and tested here. Keep in synchro
--   the two codebases.
testWithDataImportedFromPHP :: Info -> Int -> LocalTime -> [HUnit.Test]
testWithDataImportedFromPHP info testPhase time1
  = L.map (\t -> HUnit.TestCase t) (tests !! testPhase)

 where

  getFullName info id localTime notUsed1 notUsed2 startPos reverseOrder
    = let unitInfo = fromJust1 "oha1" $ info_getDataInfoForUnitId info id localTime True
          parentIds = info_getDataInfoParentHierarchy info unitInfo localTime True
      in  info_getFullName parentIds startPos reverseOrder True False

  getFullIds info id localTime
    = let unitInfo = fromJust1 "oha1" $ info_getDataInfoForUnitId info id localTime True
          parentIds = info_getDataInfoParentHierarchy info unitInfo localTime True
      in  info_getParentHiearchyIds parentIds

  getNearestRateCategoryId info id localTime
    = let unitInfo = fromJust1 "oha2" $ info_getDataInfoForUnitId info id localTime True
          parentIds = info_getDataInfoParentHierarchy info unitInfo localTime True
      in  fromJust $ info_getRateCategoryId parentIds

  daysInThePast (LocalTime d1 t1) d
    = LocalTime (addDays (0 - d) d1) t1

  time2 = (daysInThePast time1 (30 * 8))

  time3 = (daysInThePast time1 10)

  oldDate = (daysInThePast time1 60)

  recentDate = daysInThePast time1 20

  newRecentDate = daysInThePast time1 5

  newNewRecentDate = daysInThePast time1 1

  badDate = daysInThePast time1 2

  date5MonthsAgo = daysInThePast time1 (30 * 5)

  date2MonthsAgo = daysInThePast time1 (30 * 2)

  validCode (Right (Just (c, _))) = c

  isError :: Either AsterisellError (Maybe (DataInfo, IsExtensionalMatch)) -> Bool
  isError (Left _) = True
  isError (Right _) = False

  tests
    = [ [ -- test phase 0
          HUnit.assertEqual "" (show 1) (show $ unit_id $ fromJust $ info_getDataInfoForUnitId info 1 time1 True)
        , HUnit.assertEqual "" (show 1) (show $ structure_id $ fromJust $ info_getDataInfoForUnitId info 1 time1 True)
        , HUnit.assertEqual "" (show (Nothing :: Maybe Int)) (show $ structure_parentUnitId $ fromJust $ info_getDataInfoForUnitId info 1 time1 True)
        , HUnit.assertEqual "" "org1" (getFullName info 1 time1 False False 0 False)
        , HUnit.assertEqual "" "/1/" (T.unpack $ getFullIds info 1 time1)
        , HUnit.assertEqual "" (show 1) (show $ getNearestRateCategoryId info 1 time1)
        , HUnit.assertEqual "" (T.unpack $ getFullIds info 1 time1) (T.unpack $ getFullIds info 1 (daysInThePast time1 10))
        ]
      , [ -- test phase 1
          HUnit.assertEqual "" (show 1) (show $ unit_id $ fromJust $ info_getDataInfoForUnitId info 1 time1 True)
        , HUnit.assertEqual "" (show 2010) (show $ structure_id $ fromJust $ info_getDataInfoForUnitId info 1 time1 True)
        , HUnit.assertEqual "" (show 2) (show $ fromJust $ structure_parentUnitId $ fromJust $ info_getDataInfoForUnitId info 1 time1 True)
        , HUnit.assertEqual "" "org2 / org1" (getFullName info 1 time1 False False 0 False)
        , HUnit.assertEqual "" "org1" (getFullName info 1 time2 False False 0 False)
        , HUnit.assertEqual "" "/2/1/" (T.unpack $ getFullIds info 1 time1)
        , HUnit.assertEqual "" "/1/" (T.unpack $ getFullIds info 1 time2)
        , HUnit.assertEqual "" (T.unpack $ getFullIds info 1 time1) (T.unpack $ getFullIds info 1 time3)
        , HUnit.assertEqual "" (show 2) (show $ getNearestRateCategoryId info 1 time1)
        , HUnit.assertEqual "" (show 1) (show $ getNearestRateCategoryId info 1 time2)
        ]
      , [ -- test phase 2
          HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "123" time1)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "1234" time1)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "12345" time1)
        ]
      , [ -- test phase 3
          HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "123" oldDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "1234" oldDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "12345" oldDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "456" recentDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "4567" recentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "45678" recentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "123" recentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "456" oldDate)
        ]
      , [ -- test phase 4
          HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "4567" newRecentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "45678" newRecentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "123" newRecentDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "123" oldDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "1234" oldDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "12345" oldDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "456" recentDate)
        , HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "4567" recentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "45678" recentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "123" recentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "456" oldDate)
        ]
      , [ -- test phase 5
          HUnit.assertEqual "" (show 4) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "456" newNewRecentDate)
        , HUnit.assertEqual "" (show 4) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "4567" newNewRecentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "45678" newNewRecentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "123" newNewRecentDate)
        , HUnit.assertEqual "" "Right Nothing" (show $  info_getDataInfoForExtensionCode info "456" oldDate)
        , HUnit.assertEqual "" (show 4) (show $  unit_id $ validCode $ info_getDataInfoForExtensionCode info "456" badDate)
        ]
      , [ -- test phase 6
          HUnit.assertEqual "" (show 3) (show $ unit_id $ validCode $  info_getDataInfoForExtensionCode info "123" date5MonthsAgo)
        , HUnit.assertEqual "" (show True) (show $ isError $  info_getDataInfoForExtensionCode info "123" date2MonthsAgo)
        ]
      ]
