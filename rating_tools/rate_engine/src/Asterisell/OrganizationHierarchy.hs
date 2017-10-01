{-# Language BangPatterns, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass, FlexibleContexts  #-}

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


-- | Import Organization Hierarchies related tables from the PHP world.
--
--   This code must be keept in synchro with
--   * `apps/asterisell/lib/jobs/data_file_processing/ExportConfigurationsToExternalRateEngine.php`,
--   * `apps/asterisell/lib/OrganizationUnitInfo.php`
--

module Asterisell.OrganizationHierarchy (
  Extensions,
  ParentHiearchy,
  DirectPriceCategories,
  ParentIdHierarchy,
  Info(..),
  info_getDataInfoForUnitId,
  info_getDataInfoParent,
  info_getDirectPriceCategory,
  info_getDataInfoForExtensionCode,
  info_getParentIdHierarchy,
  info_getBillableDataInfo,
  info_getRateCategoryId,
  info_getFullName,
  info_empty,
  info_getDataInfoParentHierarchy,
  info_getParentHiearchyIds,
  UnitId,
  DataInfo(..),
  extensions_load,
  testWithDataImportedFromPHP,
  PriceCategoryId,
  IsApplicationError,
  IsStrictResult,
  info_respectCodeContracts,
  extensionCodes_extractFromUserSpecification
) where

import Asterisell.Trie
import Asterisell.Utils
import Asterisell.Error

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as IO

import GHC.Generics
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

import qualified Test.HUnit as HUnit
import qualified Test.HUnit.Base as HUnit
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

-----------------------
-- BASIC DATA TYPES  --
-----------------------

-- | A unique identifier for ar_organization_unit.id.
--
type UnitId = Int

type DataInfoId = Int

type PriceCategoryId = Int

-- | The head is the root organization id, and then there is the first parent,
--   until the final child.
type ParentIdHierarchy = [UnitId]

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

      structure_from :: !LocalTime,
      -- ^ from when the info replace the old info, on the same unit_id, and it became valid

      unitType_id :: !Int,
      unitType_name :: !(Maybe T.Text),
      unitType_shortCode :: !(Maybe T.Text),

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
      party_isActive :: !(Maybe Bool),
      party_resellerId :: !(Maybe Int)
    }
 deriving(Eq, Ord, Show, Generic, NFData)

-- | Used for accessing in an efficent way the organization info.
--   The Int key is the unit_id of the organization.
--   The value is the list of associated DataInfo, in reverse order of structure_from time.
--   This data structure is efficient if an organization does not change too much hierarchy structure over time.
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
    , info_organizationIdsThatCanBeBillable :: [UnitId]
    -- ^ organization id that can be billable at a certain date
    , info_maybeRootOrganizations :: [UnitId]
    -- ^ organizations that can be root organizations, now or in the past
    , info_directPriceCategories :: DirectPriceCategories
   } deriving(Show, Generic, NFData)

info_empty :: Info
info_empty = Info {
    info_extensions = trie_empty
  , info_organizations = IMap.empty
  , info_organizationIdsThatCanBeBillable = []
  , info_maybeRootOrganizations = []
  , info_directPriceCategories = IMap.empty
}

info_respectCodeContracts :: Info -> Either String ()
info_respectCodeContracts info
  = do (when $ not $ testIntMapWithDataInfo $ info_organizations info)
         (fail $ dbc_error "ohi1")

       (when $ not $ testIntMapWithDataInfo $ info_directPriceCategories info)
         (fail $ dbc_error "ohi2")

       -- NOTE: there is no contract for `info_extensions`
       return ()

 where

   testIntMapWithDataInfo :: IMap.IntMap [DataInfo] -> Bool
   testIntMapWithDataInfo map1
     = let map2 = IMap.map (\l1 -> isDescendingOrder $ L.map structure_from l1) map1
       in  L.all id (L.map snd $ IMap.toList map2)

-------------------------------------------------
-- PARSING OF INFO IMPORTED FROM THE PHP WORLD --
-------------------------------------------------

type ParserState = (Info, StringDictionary, ConfigurationErrors, LineNumber)

-- | Parse a file using Attoparsec, and chunk by chunk.
--   So it can be used for big files, without using too much RAM.
--   @require: the records are in ascending order respect ar_organization_unit_has_structure.from
extensions_load :: Bool -> String -> IO Info
extensions_load isDebugMode fileName
  = SIO.withFile fileName SIO.ReadMode $ \handle -> do
       SIO.hSetEncoding handle SIO.utf8_bom
       fileContent <- IO.hGetContents handle

       let !r = parse (parseExtensions parserState_empty <* endOfInput) fileContent
       case eitherResult r of
         Left msg
           -> throw $ AsterisellException $ "Err 50575. This is an error in the code: contact the assistance. Error loading extensions info: " ++ msg
         Right (info, _, errors, _)
           -> case Set.null errors of
                True
                  -> return info
                False
                  -> throw $ AsterisellException $ "There are extensions configured not correctly. " ++ (L.concatMap (\t -> t ++ "\n") (Set.toList errors))

parserState_empty :: ParserState
parserState_empty = (info_empty, Map.empty, Set.empty, 1)

addExtensionRecord :: ParserState -> Parser ParserState
addExtensionRecord state1@(info1, dict1, errors1, ln)
  = (do field1 <- parseInt
        char ','
        field2 <- parseInt
        char ','
        (field3, dict3) <- parseCSVMaybeSharedString ',' dict1
        char ','
        (field4, dict4) <- parseCSVMaybeSharedString ',' dict3
        char ','
        field5 <- parseInt
        char ','
        field6 <- parseMaybeInt
        char ','
        field7 <- parseMySQLDateTimeToLocalTime ','
        char ','
        field8 <- parse01AsBool
        char ','
        field9 <- parseMaybeInt
        char ','
        field10 <- parseMaybeInt
        char ','
        field11 <- parseCSVMaybeString ','
        char ','
        field12 <- parseCSVMaybeString ','
        char ','
        field13 <- parseCSVMaybeString ','
        char ','
        (field14, dict14) <- parseCSVMaybeSharedString ',' dict4
        char ','
        (field15, dict15) <- parseCSVMaybeSharedString ',' dict14
        char ','
        (field16, dict16) <- parseCSVMaybeSharedString ',' dict15
        char ','
        (field17, dict17) <- parseCSVMaybeSharedString ',' dict16
        char ','
        field18M <- parseMaybe01AsBool
        char ','
        field19 <- parseMaybe01AsBool
        char ','
        field20 <- parseMaybeInt

        (endOfLine <|> endOfInput)

        let field18 = case field18M of
                        Nothing -> False
                        Just v -> v

        let value = DataInfo {
                      unit_id = field1,
                      unitType_id = field2,
                      unitType_name = field3,
                      unitType_shortCode = field4,
                      structure_id = field5,
                      structure_parentUnitId = field6,
                      structure_from = field7,
                      structure_exists = field8,
                      structure_rateCategoryId = field9,
                      structure_partyId = field10,
                      structure_extensionCodes = field11,
                      structure_extensionName = field12,
                      structure_extensionUserCode = field13,
                      rateCategory_name = field14,
                      rateCategory_internalName = field15,
                      party_name = field16,
                      party_compactName = field17,
                      party_isBillable = field18,
                      party_isActive = field19,
                      party_resellerId = field20
                    }

        let (info2, errors2) = info_addDataInfo (info1, errors1) value
        return (info2, dict17, errors2, ln + 1)

    ) <?> ("Error at line: " ++ show ln)

-- | Extract extension codes, from an user specification.
extensionCodes_extractFromUserSpecification :: T.Text -> [ExtensionCode]
extensionCodes_extractFromUserSpecification userCodes
  = extractResult $ L.foldl' proc ([], [], False, True) (T.unpack userCodes)

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

-- | Add an info about organizations, mantaining updated the global Info data structure.
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
                -> let -- | Extract the codes separated from ",", and manage quoted value: "\,"
                       listOfCodes :: [ExtensionCode]
                       listOfCodes = extensionCodes_extractFromUserSpecification codes

                   in L.foldl' (\(trie1, errors1) extensionCode
                                  -> let extensionCodeM = extensionCode_toCharsMatch extensionCode
                                      in case charsMatch_valid extensionCodeM of
                                          Just err
                                            -> (trie1, Set.insert ("Extension \"" ++ extensionCode ++ "\" has an invalid format: " ++ err) errors1)
                                          Nothing
                                            -> (trie_addExtensionCodeWith trie1 extensionCode [dataInfo] (flip (++)), errors1)

                              ) (extensions1, errors) listOfCodes

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

    in (info {
           info_extensions = addedExtensions
         , info_organizations = addedOrganizations
         , info_maybeRootOrganizations = addedMaybeRootOrganizations
         , info_organizationIdsThatCanBeBillable = addedOrganizationIdsThatCanBeBillable
         , info_directPriceCategories = addedDirectPriceCategories
         }
       , addedErrors
       )

-- | Parse zero or many extensions,
--   returning the final state.
--
parseExtensions :: ParserState-> Parser ParserState
parseExtensions state1
  = (do state2 <- addExtensionRecord state1
        parseExtensions state2) <|> (return state1)

----------------------------------------------------
-- RETRIEVE THE BEST INFO ACCORDING THE HIERARCHY --
----------------------------------------------------

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

info_getDataInfoForExtensionCode :: Info -> String -> LocalTime -> Either AsterisellError (Maybe (DataInfo, IsExtensionalMatch))
info_getDataInfoForExtensionCode info extension date
  = let trie = info_extensions info
    in  case trie_getMatch trie_getMatch_initial trie extension of
          Nothing
            -> Right Nothing
          Just ((_, isExtensionalMatch), dataInfos1)
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
                                       ("extension code conflict - " ++ extension)
                                       ("The extension code \"" ++ extension ++ "\", at date " ++ showLocalTime date ++ ", is associated both to organization with id " ++ show (unit_id r1) ++ ", and to organization with id " ++ show (unit_id r2))
                                       ("The calls using this extension code will be not rated.")
                                       ("Fix the associations between extension codes, and organizations, and rerate the calls.")

info_getDataInfoParent :: Info -> DataInfo -> LocalTime -> IsStrictResult -> Maybe DataInfo
info_getDataInfoParent info unitInfo date isStrict
  = case structure_parentUnitId unitInfo of
      Nothing
        -> Nothing
      Just parentId
        -> info_getDataInfoForUnitId info parentId date isStrict

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

-- | An identifier for a complete organization hierarchy.
--   Something like "/id1/id2/id3/" where "id1" is the main root, and "id3" the last child.
--
info_getParentIdHierarchy :: ParentHiearchy -> ParentIdHierarchy
info_getParentIdHierarchy infos
  = L.map (\d -> unit_id d) infos

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

----------------------
-- REGRESSION TESTS --
----------------------

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
