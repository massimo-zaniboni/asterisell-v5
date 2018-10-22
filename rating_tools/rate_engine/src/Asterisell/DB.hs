{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Manage DB connections.
--
module Asterisell.DB (
  DBConf(..),
  db_openConnection,
  db_openTransaction,
  db_commitTransaction,
  db_rollBackTransaction,
  db_releaseResource,
  toDBBool,
  toDBInt64,
  toMaybeInt64,
  toDBMaybeString,
  toDBByteString,
  toDBMaybeText,
  toDBText,
  toDBLocalTime,
  toDBMaybeLocalTime,
  fromDBInt,
  fromDBBool,
  fromDBLocalTime,
  fromDBTime,
  fromMaybeDBValue,
  fromMaybeDBValues,
  fromDBText,
  fromDBByteString,
  isDBValueEmptyOrNull,
  fromMaybeDBByteStringToMaybeEmpty,
  fromMaybeDBValueToDefault,
  normalizeDbNull,
  nn,
  nnn,
  toShowByteString,
  TField(..),
  DataSourceName,
  TableName,
  TFields(..),
  tfields_toImport,
  TRecord,
  trecord_empty,
  trecord_show,
  trecord,
  tfields_results,
  tfields_result,
  trecord_internalName,
  trecord_save,
  trecord_delete,
  trecord_set,
  trecord_setMany
 ) where

import Asterisell.Error
import Asterisell.Utils

import qualified Data.ByteString as BS
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Fixed
import qualified Data.Text as Text
import Control.Exception.Safe (throwIO)
import Database.MySQL.Base as DB
import Data.Hashable
import GHC.Generics (Generic)
import Data.List as L
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy as LT
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import Data.Monoid
import Data.Hash.MD5 as MD5
import Data.Hashable

data DBConf
   = DBConf {
       dbConf_dbName :: BS.ByteString
     , dbConf_user :: BS.ByteString
     , dbConf_password :: BS.ByteString
     } deriving (Eq, Show)


db_openConnection :: DBConf -> Bool -> IO DB.MySQLConn
db_openConnection dbConf isRemoteDB
     = do
          conn <- DB.connect dbParams

          case isRemoteDB of
            True
              -> do return ()
            False
              -> do DB.execute_ conn "SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED"
                    -- set this level because in TokuDB it is the only level for SELECT, not involving LOCKS and SNAPSHOOTS,
                    -- it is faster than others,
                    -- and it prevents many conflicts and long transactions lock timeouts.
                    --
                    -- NOTE: this is still safe in Asterisell because:
                    -- * there is only one write transaction at a time processing CDRs
                    -- * in case there are transactions changing configuration data on the web-side, then the "recalc-flag" is activated by triggers,
                    --   and CDRs are recalculated in any case, so data at the end of recalculation is never in an inconsistent state
                    -- * new errors are written on a temporary table not visible to users, and the content is copied at the end of CDR processing

                    DB.execute_ conn "SET autocommit = 0"
                    return ()

          return conn
 where

   dbParams = DB.defaultConnectInfoMB4 {
                     ciUser = dbConf_user dbConf
                   , ciPassword = dbConf_password dbConf
                   , ciDatabase = dbConf_dbName dbConf
                   }

db_openTransaction :: DB.MySQLConn -> IO ()
db_openTransaction conn = do
  DB.execute_ conn "START TRANSACTION"
  return ()

db_commitTransaction :: DB.MySQLConn -> IO ()
db_commitTransaction conn = do
  DB.execute_ conn "COMMIT"
  return ()

db_rollBackTransaction :: DB.MySQLConn -> IO ()
db_rollBackTransaction conn = do
  DB.execute_ conn "ROLLBACK"
  return ()

db_releaseResource :: Bool -> DB.MySQLConn -> IO ()
db_releaseResource isOk conn
 = case isOk of
     True -> db_commitTransaction conn
     False -> db_rollBackTransaction conn

-- --------------------------------------------
-- Convert DB Values to Haskell  types.
-- This is useful because sometimes is not clear witch particular DBValue
-- a query will return.

fromDBInt :: DB.MySQLValue -> Int
fromDBInt (MySQLInt32 v) = fromIntegral v
fromDBInt (MySQLInt64 v) = fromIntegral v
fromDBInt (MySQLInt32U v) = fromIntegral v
fromDBInt (MySQLInt64U v) = fromIntegral v
fromDBInt (MySQLInt8 v) = fromIntegral v
fromDBInt (MySQLInt8U v) = fromIntegral v
fromDBInt (MySQLInt16 v) = fromIntegral v
fromDBInt (MySQLInt16U v) = fromIntegral v
fromDBInt v = error ("fromDBInt: unsupported MySQL value" ++ show v) 
{-# INLINE fromDBInt #-}

fromDBText :: DB.MySQLValue -> Text.Text
fromDBText (MySQLText v) = v
fromDBText (MySQLBytes v) = fromByteStringToText v
fromDBText v = error ("fromDBText: unsupported MySQL value " ++ show v) 
{-# INLINE fromDBText #-}

fromDBByteString :: DB.MySQLValue -> BS.ByteString
fromDBByteString (MySQLText v) = fromTextToByteString v
fromDBByteString (MySQLBytes v) = v
fromDBByteString v = error ("fromDBByteString: unsupported MySQL value " ++ show v) 
{-# INLINE fromDBByteString #-}

-- | Convert a NULL string into an empty string.
fromMaybeDBByteStringToMaybeEmpty :: DB.MySQLValue -> BS.ByteString
fromMaybeDBByteStringToMaybeEmpty mv
    = case fromMaybeDBValue fromDBByteString mv of
        Nothing -> BS.empty
        Just r -> r

fromDBLocalTime :: DB.MySQLValue -> LocalTime
fromDBLocalTime (MySQLDateTime v) = v
fromDBLocalTime (MySQLTimeStamp v) = v
fromDBLocalTime (MySQLDate v) = LocalTime v midnight 
fromDBLocalTime v = error ("fromDBLocalTime: unsupported MySQL value " ++ show v) 
{-# inline  fromDBLocalTime #-}

fromDBTime :: DB.MySQLValue -> TimeOfDay
fromDBTime (MySQLTime _ td) = td
{-# inline  fromDBTime #-}

-- | MySQL has no built-in bool type, so recognize common variants. 
fromDBBool :: DB.MySQLValue -> Bool
fromDBBool sv
    = case sv of
        (MySQLInt32 v) -> (v > 0)
        (MySQLInt64 v) -> (v > 0) 
        (MySQLInt32U v) -> (v > 0) 
        (MySQLInt64U v) -> (v > 0) 
        (MySQLInt8 v) -> (v > 0) 
        (MySQLInt8U v) -> (v > 0) 
        (MySQLInt16 v) -> (v > 0) 
        (MySQLInt16U v) -> (v > 0)
        (MySQLText v)
          -> case v of
                "t" -> True
                "T" -> True
                "1" -> True
                "f" -> False
                "F" -> False
                "0" -> False
                _ -> error ("fromDBBool: unsupported MySQL value " ++ show sv) 
        (MySQLBytes v)
          -> case v of
                "t" -> True
                "T" -> True
                "1" -> True
                "f" -> False
                "F" -> False
                "0" -> False
                _ -> error ("fromDBBool: unsupported MySQL value " ++ show sv) 
        _ -> error ("fromDBBool: unsupported MySQL value " ++ show sv) 

fromMaybeDBValue :: (DB.MySQLValue -> a) -> DB.MySQLValue -> Maybe a
fromMaybeDBValue  _ (MySQLNull) = Nothing
fromMaybeDBValue f v = Just $ f v
{-# INLINE fromMaybeDBValue #-}

-- | Leave only the no null parts.
fromMaybeDBValues :: (DB.MySQLValue -> a) -> [DB.MySQLValue] -> [a]
fromMaybeDBValues f vs = L.map fromJust $ L.filter isJust $ L.map (fromMaybeDBValue f) vs

-- | In case the value is NULL, return the default value.
fromMaybeDBValueToDefault :: a -> (DB.MySQLValue -> a) -> DB.MySQLValue -> a
fromMaybeDBValueToDefault defaultValue f value
    = case fromMaybeDBValue f value of
        Nothing -> defaultValue
        Just v -> v

-- | In case the value is NULL, return the default value.
normalizeDbNull :: DB.MySQLValue -> DB.MySQLValue -> DB.MySQLValue
normalizeDbNull defaultValue (DB.MySQLNull) = defaultValue
normalizeDbNull _ value = value

isDBValueEmptyOrNull :: DB.MySQLValue -> Bool
isDBValueEmptyOrNull (MySQLNull) = True
isDBValueEmptyOrNull (MySQLText v) = Text.null v
isDBValueEmptyOrNull (MySQLBytes v) = BS.null v
isDBValueEmptyOrNull _ = False

-- | Used during importing of data, for signaling values that are not NULL.
nn :: String -> Int -> (DB.MySQLValue -> a) -> DB.MySQLValue -> IO a
nn table id f x
   = case fromMaybeDBValue f x of
       Just a -> return a
       Nothing -> throwIO $ AsterisellException $ "Unexpected field with NULL value in " ++ table ++ " with id " ++ show id
{-# INLINE nn #-}

-- | Used during importing of data, for signaling values that are not NULL.
nnn :: String -> Int -> (DB.MySQLValue -> a) -> DB.MySQLValue -> a
nnn table id f x
   = case fromMaybeDBValue f x of
       Just a -> a
       Nothing -> error $ "Unexpected field with NULL value in " ++ table ++ " with id " ++ show id
{-# INLINE nnn #-}

-- ---------------------------------------------------
-- Low Level Utils for Converting values to DB values

toDBBool :: Bool -> MySQLValue
toDBBool True = MySQLInt8 1
toDBBool False = MySQLInt8 0
{-# INLINE toDBBool #-}

toDBInt64 :: Int -> MySQLValue
toDBInt64 i =  MySQLInt64 $ fromInteger $ toInteger i
{-# INLINE toDBInt64 #-}


toMaybeInt64 :: Maybe Int -> DB.MySQLValue
toMaybeInt64 Nothing = MySQLNull
toMaybeInt64 (Just s) = toDBInt64 s
{-# INLINE toMaybeInt64 #-}

toDBMaybeString :: Maybe BS.ByteString -> DB.MySQLValue
toDBMaybeString Nothing = MySQLNull
toDBMaybeString (Just s) = MySQLBytes s
{-# INLINE toDBMaybeString #-}


toDBByteString :: BS.ByteString -> DB.MySQLValue
toDBByteString s = MySQLBytes s
{-# INLINE toDBByteString #-}

toDBMaybeText :: Maybe Text.Text -> DB.MySQLValue
toDBMaybeText Nothing = MySQLNull
toDBMaybeText (Just s) = toDBText s
{-# INLINE toDBMaybeText #-}

toDBText :: Text.Text -> DB.MySQLValue
toDBText s = MySQLBytes $ fromTextToByteString s
{-# INLINE toDBText #-}

toDBLocalTime :: LocalTime -> DB.MySQLValue
toDBLocalTime l = MySQLDateTime l
-- NOTE: use MySQLDateTime instead of MySQLTimeStamp, so MySQL store it without messing with UTC conversion
{-# INLINE toDBLocalTime #-}

toDBMaybeLocalTime :: Maybe LocalTime -> DB.MySQLValue
toDBMaybeLocalTime Nothing = MySQLNull
toDBMaybeLocalTime (Just t) = toDBLocalTime t
{-# INLINE toDBMaybeLocalTime #-}

-- | Show the value in human friendly way.
--   It is not meant to be fast.
toShowByteString :: DB.MySQLValue -> BS.ByteString
toShowByteString dbv
    = case dbv of
        MySQLNull -> "<NULL>"
        (MySQLDateTime v) -> fromStringToByteString $ showLocalTime v
        (MySQLTimeStamp v) -> fromStringToByteString $ showLocalTime v
        (MySQLBytes v) ->  v 
        (MySQLText v) -> fromTextToByteString v
        (MySQLInt16 v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt32 v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt64 v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt8  v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt16U v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt32U v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt64U v) -> fromStringToByteString $ show $ toInteger v
        (MySQLInt8U v) -> fromStringToByteString $ show $ toInteger v
        _ -> "<unknown format>" 

-- -----------------
-- Hashable support

instance Hashable Day where
  hashWithSalt salt a = hashWithSalt salt (toGregorian a)

instance Hashable TimeOfDay where
  hashWithSalt salt a
      = hashWithSalt salt (todHour a, todMin a, todSec a)

instance Hashable LocalTime where
  hashWithSalt salt a
    = hashWithSalt salt (localDay a, localTimeOfDay a)

instance Hashable MySQLValue

-- ----------------------------
-- Operations on list of fields

-- | A field of a table.
data TField
  = TField {
      tfield_name :: !BS.ByteString
    , tfield_isPK :: !Bool
    , tfield_import :: !Bool
    -- ^ False for ignoring the field
    , tfield_addToHistory :: !Bool
    -- ^ True for adding the new info to history,
    --   False for updating/fixing without maintaining an old copy in the history.
    --   NOTE: take effect only in case the field is imported, otherwise it is always implicitely set to False.
           }
 deriving (Eq, Ord, Show)

type DataSourceName = Text.Text

type TableName = BS.ByteString

-- | Fields of the same table.
data TFields = TFields TableName [TField]
 deriving(Eq, Ord, Show)

-- | Leave only the fields to import.
tfields_toImport :: TFields -> TFields
tfields_toImport (TFields tn xs)
  = let xs' = L.filter (tfield_import) xs
    in  (TFields tn xs')

-- | The record result, from TField query.
type TRecord = Map.Map BS.ByteString (TField, DB.MySQLValue)

trecord_empty :: TRecord
trecord_empty = Map.empty

trecord_show :: TRecord -> BS.ByteString
trecord_show tr
  = let df (f, (_, v)) = BS.concat [f, ": ", toShowByteString v, "\n"]
    in BS.concat
         $ L.map df $ Map.toList tr

trecord :: BS.ByteString -> TRecord -> DB.MySQLValue
trecord n tr
    = case Map.lookup n tr of
        Just (_, r) -> r
        Nothing -> error ("There is no field " ++ fromByteStringToString n ++ ", inside TRecord " ++ show tr) 

-- | The results of all tfields to import.
tfields_results :: DB.MySQLConn -> TFields -> Maybe (BS.ByteString, [DB.MySQLValue]) -> IO (S.InputStream TRecord)
tfields_results conn (TFields tableName tfields1) maybeCondition
  = let (TFields _ tfields2) = tfields_toImport (TFields tableName tfields1)
        (condB, condV)
          = case maybeCondition of
                  Nothing
                    -> ("", [])
                  Just (cond, params)
                    -> (BS.byteString " WHERE " <> BS.byteString cond, params)

        qb :: BS.Builder
        qb = "SELECT " <> (mconcat $ L.intersperse (BS.charUtf8 ',') (L.map (\n -> BS.charUtf8 '`' <> BS.byteString n <> BS.charUtf8 '`') $ L.map tfield_name tfields2)) <> (BS.byteString " FROM ") <> (BS.byteString tableName) <> condB 
        q = BS.toLazyByteString qb
    in do (_, records) <-DB.query conn (DB.Query q) condV
          S.map (\tvalues
                   -> let z = L.zip tfields2 tvalues
                      in Map.fromList $ L.map (\(tfield, tvalue) -> (((tfield_name tfield), (tfield, tvalue)))) z
                ) records

-- | The head/first result.
tfields_result :: DB.MySQLConn -> TFields -> Maybe (BS.ByteString, [DB.MySQLValue]) -> IO (Maybe TRecord)
tfields_result conn tfields maybeCondition = do
  inS <- tfields_results conn tfields maybeCondition
  rs <- S.toList inS
  case rs of
    [] -> return Nothing
    (r:_) -> return $ Just r

trecord_internalName
  :: DataSourceName
  -> TRecord
  -> (  Text.Text
        -- ^ the unique internal name id of the organization
      , Text.Text
        -- ^ the unique internal name id of the child extension
      , BS.ByteString
        -- ^ the checksum of fields requiring an add to the history
      , BS.ByteString
        -- ^ the checksum of fields requiring the fix/in-place-update of the current history
     )
trecord_internalName dataSourceName source
  = let primaryValues = L.map snd $ L.filter (\(f, _) -> tfield_isPK f) $ Map.elems source
        primaryKeyB = LTB.fromText dataSourceName <> LTB.singleton '-' <> (mconcat $ L.intersperse (LTB.singleton '-') (L.map (\v -> LTB.fromText $ fromByteStringToText $ toShowByteString v) primaryValues))
        -- DEV-NOTE: do not change the method for calculating this because:
        -- * it is used for importing already existing users
        -- * some customer code replicate this code for matching the extensions

        calcSourceHash :: Bool -> String
        calcSourceHash addToHistory = L.concatMap show (L.map snd $ L.filter (\(f, _) -> tfield_addToHistory f == addToHistory) $ Map.elems source)
        -- NOTE: use MD5 instead of faster internal Hash function, because MD5 is not implementation dependent,
        -- while using Hash, if the implementation changes, a massive reimport of customers is forced.
        -- NOTE: the values in `elems` are always in ascending order of field name, so the result is always the same,
        -- if the values do not change, and the database schema remain constant.

        hashStr1 = calcSourceHash True
        hashStr2 = calcSourceHash False

    in ( LT.toStrict $ LTB.toLazyText primaryKeyB
       , LT.toStrict $ LTB.toLazyText $ primaryKeyB <> LTB.fromString "-ext"
       , fromStringToByteString $ md5s (Str hashStr1)
       , fromStringToByteString $ md5s (Str hashStr2)
       )

-- | Insert/update the record, returning the newly created incremental id.
trecord_save :: DB.MySQLConn -> TableName -> TRecord -> Maybe Int -> IO Int
trecord_save conn tn tr maybeTrId
  = let
        tfields  = L.map tfield_name $ L.map fst $ Map.elems tr
        tvalues1 = L.map snd $ Map.elems tr

        qb1 :: BS.Builder
        qb1 = case maybeTrId of
                Nothing -> (BS.byteString "INSERT INTO ") <> BS.byteString tn
                Just _ -> (BS.byteString "UPDATE ") <> BS.byteString tn

        qb2 :: BS.Builder
        qb2 = qb1 <> (BS.byteString " SET ") <> (mconcat $ L.intersperse (BS.charUtf8 ',') (L.map (\fn -> BS.charUtf8 '`' <> BS.byteString fn <> BS.charUtf8 '`' <> BS.byteString " = ? ") tfields))

        (qb3 :: BS.Builder, tvalues2 :: [DB.MySQLValue])
          = case maybeTrId of
                Nothing -> (qb2, tvalues1)
                Just trId -> (qb2 <> BS.byteString " WHERE id = ? ", tvalues1 ++ [toDBInt64 trId])

        q = BS.toLazyByteString qb3

    in do DB.execute conn (DB.Query q) tvalues2
          case maybeTrId of
            Just trId -> return trId
            Nothing -> do (_, vs) <- DB.query_ conn "SELECT LAST_INSERT_ID()"
                          [[v]] <- S.toList vs
                          return $ fromDBInt v

-- | Delete the record with the specified `id`.
trecord_delete :: DB.MySQLConn -> TableName -> Int -> IO ()
trecord_delete conn tn trId
  = let q = LBS.fromChunks ["DELETE FROM ", tn, " WHERE id = ?"]
    in do DB.execute conn (DB.Query $ q) [toDBInt64 trId]
          return ()

-- | Add a field.
trecord_set :: TRecord -> BS.ByteString -> DB.MySQLValue -> TRecord
trecord_set trec1 fieldName fieldValue
  = let tfield = case Map.lookup fieldName trec1 of
                   Just (f, _) -> f
                   Nothing -> TField {
                                tfield_name = fieldName
                              , tfield_isPK = False
                              , tfield_import = True
                              , tfield_addToHistory = False
                              }

    in Map.insert fieldName (tfield, fieldValue) trec1

trecord_setMany :: TRecord -> [(BS.ByteString, DB.MySQLValue)] -> TRecord
trecord_setMany trec1 values
  = L.foldl' (\trec2 (n, v) -> trecord_set trec2 n v) trec1 values

