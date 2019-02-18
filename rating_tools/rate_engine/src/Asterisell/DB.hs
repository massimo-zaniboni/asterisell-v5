{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, BangPatterns, OverloadedStrings, ExistentialQuantification, DeriveGeneric, DeriveAnyClass, RankNTypes, QuasiQuotes #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Manage DB connections.
--
module Asterisell.DB (
  DBConf(..),
  db_openConnection,
  db_openTransaction,
  db_commitTransaction,
  db_rollBackTransaction,
  db_releaseResource,
  cparams_toDBConf,
  cparams_toDBConf2,
  cparams_toRemoteDBConf,
  uniqueDateId_next,
  uniqueDateId_randomStart,
  dbNullToCSV,
  UniqueDateId,
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
  db_init,
  db_garbagePastErrors,
  dbErrors_insert,
  dbErrors_prepareInsertStmt,
  db_commitTransactionR,
  db_rollBackTransactionR,
  db_releaseResourceR,
  db_deleteCDRS,
  db_errorFileName,
  db_groupedCDRSFileName,
  db_groupedErrorsFileName,
  createDBFile,
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
  trecord_setMany,
  db_loadDataFromNamedPipe,
  CurrencyPrecisionDigits,
  DBState(..),
 ) where

import Asterisell.Error
import Asterisell.Utils
import qualified Data.ByteString.Builder as B
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
import qualified Data.Vector as V
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
import qualified System.Posix.Files.ByteString as Posix
import qualified System.Posix.IO.ByteString as Posix
import qualified System.Posix.Process.ByteString as Posix
import qualified System.Posix.Types as Posix
import qualified System.Process as Process
import qualified System.Posix.User as Posix
import Foreign.StablePtr
import Data.Int
import System.IO as IO
import System.Directory as IO
import System.IO
import qualified Data.HashSet as HS
import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)
import Control.Exception.Assert.Sugar
import Control.DeepSeq.Generics
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import Text.Heredoc
import qualified Data.Csv as CSV
import System.Random

-- ---------------------------------------
-- DB connections

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

cparams_toDBConf :: CmdParams -> DBConf
cparams_toDBConf pp = cparams_toDBConf2 pp BS.empty

cparams_toDBConf2 :: CmdParams -> BS.ByteString -> DBConf
cparams_toDBConf2 pp p
    = DBConf {
        dbConf_user = cparams_get pp (BS.concat [p, "db-user"])
      , dbConf_password = cparams_get pp (BS.concat [p, "db-password"])
      , dbConf_dbName = cparams_get pp (BS.concat [p, "db-name"])
      }

cparams_toRemoteDBConf :: CmdParams -> DB.ConnectInfo
cparams_toRemoteDBConf pp
  = DB.defaultConnectInfo {
       ciHost = fromByteStringToString $ cparams_get pp "host"
     , ciPort =  read $ fromByteStringToString $ cparams_get pp "port"
     , ciDatabase = cparams_get pp "dbName"
     , ciUser = cparams_get pp "user"
     , ciPassword = cparams_get pp "password"
    }
    -- NOTE: do not use a defaultConnectInfoMB4 because older remote MySQL DBMS
    -- can not support it. CDR data is mainly ASCII and UTF8 chars,
    -- so it is good-enough.
    -- It will be converted to uft8mb4 when writing o the local database.

-- --------------------------------------
-- Transactions

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
db_releaseResource isOk conn = do
  case isOk of
     True -> db_commitTransaction conn
     False -> db_rollBackTransaction conn
  DB.close conn

-- | Delete all errors with the specified garbage collection key, so only new errors are inserted.
--   To call before inserting new errors.
db_garbagePastErrors
  :: DB.MySQLConn
  -> BS.ByteString
  -- ^ the garbage key
  -> Maybe LocalTime
  -- ^ delete errors from this date
  -> Maybe LocalTime
  -- ^ delete errors to this date
  -> IO ()

db_garbagePastErrors conn garbageKey mfromDate mtoDate
  = case (mfromDate, mtoDate) of
     (Just fromDate, Just toDate)
       -> do _ <- DB.execute
                    conn
                    "DELETE FROM ar_new_problem WHERE garbage_collection_key = ? AND garbage_collection_from >= ? AND garbage_collection_to <= ? ;"
                    [toDBByteString garbageKey, toDBLocalTime fromDate, toDBLocalTime toDate]
             return ()
     (Just fromDate, Nothing)
       -> do _ <- DB.execute
                    conn
                    "DELETE FROM ar_new_problem WHERE garbage_collection_key = ? AND garbage_collection_from >= ?"
                    [toDBByteString garbageKey, toDBLocalTime fromDate]
             return ()
     (Nothing, Just toDate)
       -> do _ <- DB.execute
                    conn
                    "DELETE FROM ar_new_problem WHERE garbage_collection_key = ? AND garbage_collection_to <= ? ;"
                    [toDBByteString garbageKey, toDBLocalTime toDate]
             return ()
     (Nothing, Nothing)
       -> do _ <- DB.execute
                    conn
                    "DELETE FROM ar_new_problem WHERE garbage_collection_key = ?"
                    [toDBByteString garbageKey]
             return ()

dbErrors_prepareInsertStmt :: DB.MySQLConn -> IO StmtID
dbErrors_prepareInsertStmt conn
     = let q =  [str|INSERT INTO ar_new_problem(
                    |   ar_problem_type_id, ar_problem_domain_id, ar_problem_responsible_id, created_at
                    | , duplication_key, garbage_collection_key, garbage_collection_from
                    | , garbage_collection_to
                    | , description, effect, proposed_solution
                    | , signaled_to_admin)
                    |VALUES(?,?,?, NOW(), ?, ?, ?, ?, ?, ?, ?, 0)
                    |ON DUPLICATE KEY UPDATE
                    |  garbage_collection_from = LEAST(garbage_collection_from, VALUES(garbage_collection_from))
                    |, garbage_collection_to = GREATEST(garbage_collection_to, VALUES(garbage_collection_to))
                    |, ar_problem_type_id = VALUES(ar_problem_type_id)
                    |, ar_problem_domain_id  = VALUES(ar_problem_domain_id)
                    |, ar_problem_responsible_id  = VALUES(ar_problem_responsible_id)
                    |, created_at = VALUES(created_at)
                    |, garbage_collection_key = VALUES(garbage_collection_key )
                    |, description = VALUES(description )
                    |, effect = VALUES(effect)
                    |, proposed_solution = VALUES(proposed_solution)
                    |, signaled_to_admin  = signaled_to_admin ;
                    |]
       in DB.prepareStmt conn q

-- | Insert the error.
--   @require the connection is not used from other process (it is not thread safe)
dbErrors_insert
  :: DB.MySQLConn
  -> Maybe StmtID
  -- ^ dbErrors_prepareInsertStmt
  -> AsterisellError
  -> Maybe (CallDate, CallDate)
  -- ^ garbage from - to
  -> IO ()

dbErrors_insert conn maybeStmtId err maybeGarbageTimeFrame = do

  stmtId <- case maybeStmtId of
              Just i -> return i
              Nothing -> dbErrors_prepareInsertStmt conn

  let garbageKey = asterisellError_garbageKey err
  let dupKey = asterisellError_dupKey err
  let garbageKeyTimeFrame
        = case maybeGarbageTimeFrame of
            Just (d1, d2) -> [toDBLocalTime d1, toDBLocalTime d2]
            Nothing -> [MySQLNull, MySQLNull]

  let params1 = [ MySQLInt64 $ fromIntegral $ errorType_toPHPCode $ asterisellError_type err
                , MySQLInt64 $ fromIntegral $ errorDomain_toPHPCode $ asterisellError_domain err
                , MySQLInt64 $ fromIntegral $ errorResponsible_toPHPCode $ asterisellError_responsible err
                , MySQLText dupKey
                , MySQLBytes garbageKey
                ]
  let params2 = [ MySQLText $ Text.pack $ asterisellError_description err
                , MySQLText $ Text.pack $ asterisellError_effect err
                , MySQLText $ Text.pack $ asterisellError_proposedSolution err
                ]

  _ <- DB.executeStmt conn stmtId (params1 ++ garbageKeyTimeFrame ++ params2)

  return ()

-- --------------------------------------------
-- DB UniqueId

-- | The maximum number of CDRS with the same calldate.
--   NOTE: in case of services and bundles, they have all the same date,
--   and they are proportional to the number of customers.
type UniqueDateId = Int32

uniqueDateId_next :: UniqueDateId -> UniqueDateId
uniqueDateId_next n
  = let maxId = 2 ^ 20
    in  mod (n + 1) maxId
{-# INLINE uniqueDateId_next #-}

uniqueDateId_randomStart :: IO UniqueDateId
uniqueDateId_randomStart = do
  n <- randomIO
  return $ uniqueDateId_next n
{-# INLINE uniqueDateId_randomStart #-}

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
fromDBInt v = pError ("fromDBInt: unsupported MySQL value" ++ show v)
{-# INLINE fromDBInt #-}

fromDBText :: DB.MySQLValue -> Text.Text
fromDBText (MySQLText v) = v
fromDBText (MySQLBytes v) = fromByteStringToText v
fromDBText v = pError ("fromDBText: unsupported MySQL value " ++ show v)
{-# INLINE fromDBText #-}

fromDBByteString :: DB.MySQLValue -> BS.ByteString
fromDBByteString (MySQLText v) = fromTextToByteString v
fromDBByteString (MySQLBytes v) = v
fromDBByteString v = pError ("fromDBByteString: unsupported MySQL value " ++ show v)
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
fromDBLocalTime v = pError ("fromDBLocalTime: unsupported MySQL value " ++ show v)
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
                _ -> pError ("fromDBBool: unsupported MySQL value " ++ show sv)
        (MySQLBytes v)
          -> case v of
                "t" -> True
                "T" -> True
                "1" -> True
                "f" -> False
                "F" -> False
                "0" -> False
                _ -> pError ("fromDBBool: unsupported MySQL value " ++ show sv)
        _ -> pError ("fromDBBool: unsupported MySQL value " ++ show sv)

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
       Nothing -> pError $ "Unexpected field with NULL value in " ++ table ++ " with id " ++ show id
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

-- ---------------------------
-- CSV support

dbNullToCSV :: String
dbNullToCSV = "\\N"
{-# INLINE dbNullToCSV #-}

instance CSV.ToField DB.MySQLValue where
  toField MySQLNull = fromStringToByteString dbNullToCSV
  toField (MySQLDateTime v) = CSV.toField v
  toField (MySQLTimeStamp v) = CSV.toField v
  toField (MySQLBytes v) = CSV.toField v
  toField (MySQLText v) = CSV.toField v
  toField (MySQLInt16 v) = CSV.toField v
  toField (MySQLInt32 v) = CSV.toField v
  toField (MySQLInt64 v) = CSV.toField v
  toField (MySQLInt8  v) = CSV.toField v
  toField (MySQLInt16U v) = CSV.toField v
  toField (MySQLInt32U v) = CSV.toField v
  toField (MySQLInt64U v) = CSV.toField v
  toField (MySQLInt8U v) = CSV.toField v
  toField (MySQLDecimal v) = CSV.toField v
  toField (MySQLFloat v) = CSV.toField v
  toField (MySQLDouble v) = CSV.toField v
  toField (MySQLYear v) = CSV.toField v
  toField (MySQLBit v) = CSV.toField v

instance CSV.ToField LocalTime where
  toField v = fromStringToByteString $ fromLocalTimeToMySQLDateTime v

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
        Nothing -> pError ("There is no field " ++ fromByteStringToString n ++ ", inside TRecord " ++ show tr)

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

-- -------------------------------------------------------------------
-- Insert data using named pipes

waitPosixProcess :: Process.ProcessHandle -> IO ()
waitPosixProcess pid = do
  _ <- Process.waitForProcess pid
  -- MAYBE intercept errors in the status and return an exception
  return ()

-- | Send data to a named-pipe, using a unix process as wrapper.
--   It is a lot more robust (but a little slower) respect
--   direct writing to the named pipe, that is not very robust and well supported in Haskell,
--   because GHC threads do not play well with blocking pipes.
--   NOTE: the returned handle supports back-pressure, because halt the sender if the
--   receiver is not ready to process the data, and it is good when the
--   writer (DMBS) is slower than the producer.
namedPipe_create
  :: String
  -- ^ the named pipe where redirecting data
  -> IO (Handle, Process.ProcessHandle)
  -- ^ the handler where sending data, and to close at the end.
  --   When closed the wrapper will terminate.
  --   The returned Id must be used from the caller thread for waiting the termination of the process,
  --   using the command `waitPosixProcess`.

namedPipe_create pipeName = do
  pipeExists <- Posix.fileExist (fromStringToByteString pipeName)
  when (pipeExists) (IO.removeFile pipeName)
  let mysqlCanRead = Posix.unionFileModes Posix.namedPipeMode $ Posix.unionFileModes Posix.ownerModes Posix.groupReadMode

  mysqlUser <- Posix.groupID <$> Posix.getGroupEntryForName "mysql"

  Posix.createNamedPipe (fromStringToByteString pipeName) mysqlCanRead
  Posix.setOwnerAndGroup (fromStringToByteString pipeName) (0 - 1) mysqlUser

  let cmd = (Process.shell ("cat >> " ++ pipeName)) {
                 Process.std_in = Process.CreatePipe
               , Process.std_out = Process.NoStream
               , Process.std_err = Process.NoStream
               , Process.close_fds = True
            }

  (Just hIn, Nothing, Nothing, pid) <- Process.createProcess cmd

  hSetBuffering hIn (BlockBuffering Nothing)
  return (hIn, pid)

-- | Start on a separate process `LOAD DATA INFILE` using a named pipe as input.
--   The DB process will be not anymore accessible until the pipe is closed.
--   This process will manage automatically the `ar_cdr.id` and `ar_source_cdr.id` field,
--   generating consecutive IDS. The ID field had not to be specified in the schema and in the generated data,
--   and it will be automatically inserted.
db_loadDataFromNamedPipe
  :: DBState
  -> BS.ByteString
  -- ^ named-pipe name to use as input
  -> BS.ByteString
  -- ^ the table name
  -> [BS.ByteString]
  -- ^ the columns to import. The order of columns must be the same of the pipe content.
  -- The accepted format is `\t` as field separator, `\n` as record separator,
  -- `\N` for NULL values, and proper escaping of characters.
  -> (a -> B.Builder)
  -- ^ a function converting a record to MySQL CSV-like format, without the '\N' record separator
  -> IO (OrderedStream a
        -- ^ a channel-like with a chunk of records to send to the DB.
        -- Nothing for signaling the end of the data, and terminating the process.
        , V.Vector (Async ())
        -- ^ the processes to wait, for the termination of the loading of data,
        -- and intercepting the exceptions.
        -- @require until this process does not terminate, no other DB actions can be called
        )

db_loadDataFromNamedPipe dbState pipeName tableName columns f = do
  _ <- takeMVar $ dbps_semaphore dbState

  inChan <- newEmptyMVar
  (pipeH, p1) <- namedPipe_create (fromByteStringToString pipeName)
  p2 <-async process_load
  startIndex <- uniqueDateId_randomStart
  p3 <- async (process_writeToMySQLNamedPipe inChan f pipeH startIndex)
  p4 <- async (waitPosixProcess p1)
  return (inChan, V.fromList [p2, p3, p4])

 where

  process_load = do
    case dbps_conn dbState of
      Left outFileName -> do
        -- NOTE: simulate an external process reading from the pipe and writing to disk, like in case of MySQL process.
        Process.callCommand $ "cat " ++ (fromByteStringToString pipeName) ++ " > " ++ outFileName
      Right conn -> do
        let q = B.byteString "LOAD DATA INFILE ? INTO TABLE "
                     <>  B.byteString tableName
                     <> B.byteString " CHARACTER SET utf8mb4 (`id`,"
                     <> mconcat (L.intersperse (B.byteString ",") $ L.map B.byteString columns)
                     <> B.byteString ")"
        _ <- DB.execute conn (DB.Query $ B.toLazyByteString q) [toDBByteString pipeName]
        return ()

    putMVar (dbps_semaphore dbState) ()

  process_writeToMySQLNamedPipe maybeInDataR f outH uniqueId1 = do
     maybeInData <- takeMVar maybeInDataR
     case maybeInData of
      Nothing
        -> do
              hClose outH
              return ()
      Just inData
        -> do
              !lastUniqueId
                <- V.foldM' (\uniqueId cdr -> do
                                B.hPutBuilder outH $
                                 -- MAYBE DeepSeq.force $
                                   B.int32Dec uniqueId <> B.charUtf8 '\t' <> f cdr <> B.charUtf8 '\n'
                                return $ uniqueDateId_next uniqueId) uniqueId1 inData

              process_writeToMySQLNamedPipe maybeInDataR f outH lastUniqueId

-- ---------------------------------
-- Asterisell specific transactions

-- | How many precision digits to use when exporting currencies.
--
type CurrencyPrecisionDigits = Int

data DBState
       = DBState {
           dbps_conn :: Either String DB.MySQLConn
           -- ^ Left fileName for writing to a file instead to DB
         , dbps_name :: String
         , dbps_currencyPrecision :: CurrencyPrecisionDigits
         , dbps_semaphore :: MVar ()
         }

db_init :: DBConf -> Maybe String -> CurrencyPrecisionDigits -> IO DBState

db_init dbConf maybeFileName currencyPrecision = do
  conn <-
      case maybeFileName of
        Nothing -> do
          c <- db_openConnection dbConf False
          db_openTransaction c
          return $ Right c
        Just fileName -> return $ Left fileName

  s <- newMVar ()
  let !r = DBState {
             dbps_conn = conn
           , dbps_name = fromByteStringToString $ dbConf_dbName dbConf
           , dbps_currencyPrecision = currencyPrecision
           , dbps_semaphore = s
           }
  return r

-- | Save data and normal error messages.
--   Errors are saved only if the transaction is not aborted.
--   So critical errors (aborting the transaction), must be returned using `throwIO`
--   and normal errors will be discarded.
db_commitTransactionR :: DBState -> IO ()
db_commitTransactionR state = do
  _ <- takeMVar $ dbps_semaphore state
  case dbps_conn state of
    Left _ -> return ()
    Right conn -> db_commitTransaction conn

  putMVar (dbps_semaphore state) ()

db_rollBackTransactionR :: DBState -> IO ()
db_rollBackTransactionR state = do
  _ <- takeMVar $ dbps_semaphore state
  case dbps_conn state of
    Left _ -> return ()
    Right conn -> db_rollBackTransaction conn
  putMVar (dbps_semaphore state) ()

db_releaseResourceR :: Bool -> DBState -> IO ()
db_releaseResourceR  isOk dbState = do
  _ <- takeMVar $ dbps_semaphore dbState
  case dbps_conn dbState of
    Left _ -> return ()
    Right conn -> do
      case isOk of
        True -> db_commitTransaction conn
        False -> db_rollBackTransaction conn
      DB.close conn
  putMVar (dbps_semaphore dbState) ()

-- | Delete old calculated CDRs before calculating new ones.
--   Delete also `ar_cached_grouped_cdrs` and `ar_cached_errors`, because they will be updated
--   from installed DB triggers.
db_deleteCDRS :: DBState -> LocalTime -> IO ()

db_deleteCDRS state fromCallDate = do
  _ <- takeMVar $ dbps_semaphore state
  case dbps_conn state of
    Left _ -> return ()
    Right conn -> do
      let d1 = toDBLocalTime fromCallDate
      let q1 = "DELETE FROM ar_cdr WHERE calldate >= ?"
      DB.execute conn (DB.Query q1) [d1]
      return ()
  putMVar (dbps_semaphore state) ()

db_errorFileName :: String -> String
db_errorFileName dbName = "/var/tmp/errors__" ++ dbName ++ ".csv"

db_bundleFileName :: String -> String
db_bundleFileName dbName = "/var/tmp/bundles__" ++ dbName ++ ".csv"

db_groupedCDRSFileName :: String -> String
db_groupedCDRSFileName dbName = "/var/tmp/grouped_cdrs__" ++ dbName ++ ".csv"

db_groupedErrorsFileName :: String -> String
db_groupedErrorsFileName dbName = "/var/tmp/grouped_errors__" ++ dbName ++ ".csv"

-- | Create a DB file, accessible only from MySQL.
createDBFile :: String -> IO Handle
createDBFile fileName = do
  e <- IO.doesFileExist fileName
  when e (removeFile fileName)
  let mysqlCanRead = Posix.unionFileModes Posix.ownerModes Posix.groupReadMode
  fd <- Posix.createFile (fromStringToByteString fileName) mysqlCanRead
  mysqlUser <- Posix.groupID <$> Posix.getGroupEntryForName "mysql"
  Posix.setOwnerAndGroup (fromStringToByteString fileName) (0 - 1) mysqlUser
  -- NOTE: the file must be accessible from MySQL

  outH <- Posix.fdToHandle fd
  hSetBuffering outH (BlockBuffering Nothing)

  return outH
