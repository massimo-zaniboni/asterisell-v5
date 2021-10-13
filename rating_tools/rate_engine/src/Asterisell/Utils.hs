{-# Language OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

-- | Utils functions used from other modules.
--
module Asterisell.Utils (
  StringDictionary,
  IOStringDictionary,
  d_empty,
  d_get,
  RemoteTimeZone,
  LineNumber,
  Parser,
  CmdParams,
  Chunk,
  OrderedStream,
  Foldl,
  Foldr,
  FoldlWithKey,
  cparams_load,
  cparams_get,
  maybeIf,
  fromAsteriskLastDataToExtension,
  parseMySQLDateTimeToUTC,
  parseMySQLDateTimeToLocalTime,
  parseMySQLDateTimeToMaybeLocalTime,
  parseCSVString,
  parseCSVSharedString,
  parseCSVMaybeSharedString,
  parseCSVMaybeString,
  parseMaybe01AsBool,
  parse01AsBool,
  parseInt,
  parseMaybeInt,
  showLocalTime,
  showLocalTimeUsingMySQLFormat,
  fromMySQLDateTimeAsTextToLocalTime,
  fromMySQLDateTimeToLocalTime,
  fromDateFormat1ToLocalTime,
  fromLocalTimeToMySQLDateTime,
  fromLocalTimeToMySQLDateWith00Time,
  toBeginOfTheDay,
  tomorrow,
  today,
  yesterday,
  fromLazyByteStringToString,
  fromLazyToStrictByteString,
  fromGammaCallDateAndTimeStampToLocalTime,
  fromGammaItemRentalCallDateToLocalTime,
  fromTextToInt,
  fromByteStringToInt,
  fromTextToRational,
  fromTextToRational2,
  fromStringToByteString,
  fromByteStringToString,
  fromByteStringToText,
  fromTextToByteString,
  validCallTime,
  whenM_,
  mathRound,
  fromJust1,
  fromJust2,
  fromRight1,
  tt_parseTests,
  IsApplicationError,
  ConfigurationErrors,
  replacePrefixAndGetPortedTelephoneNumber,
  isAscendingOrder,
  isDescendingOrder,
  fromMySQLResultToText,
  fromTextToMySQLResult,
  skipCSVField,
  fromDateFormat2ToLocalTime,
  MultiMap,
  multiMap_add,
  StringIndexDictionary(..),
  sdict_empty,
  sdict_insert,
  stream_toJust,
  toMySQLCSV_bool,
  toMySQLCSV_text,
  toMySQLCSV_byteString,
  toMySQLCSV_int,
  toMySQLCSV_int32,
  toMySQLCSV_ids,
  toMySQLCSV_localTime,
  toMySQLCSV_maybeLocalTime,
  toMySQLCSV_maybe,
  toMySQLCSV_maybeBool,
  toMySQLCSV_maybeText,
  toMySQLCSV_maybeByteString,
  toMySQLCSV_maybeInt,
  toMySQLCSV_maybeIds,
  toMySQLCSV_escaped,
  withResource,
  withResource',
  withResource'',
  stream_mapM,
  stream_map,
  stream_sequence,
  stream_forceMap,
  stream_toOrderedStream,
  process_initCores,
  process_orderedChunks,
  process_orderedChunksUsingFun,
  orderedStream_put,
  mapAccumLM,
  pError,
  pAssert
) where

import Data.Int
import Control.Applicative ((<$>), (<|>), (<*>))
import Text.Megaparsec as P
import Text.Megaparsec.Char as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text.Read as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as B
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import qualified Data.Map.Strict as Map
import Data.Hashable
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashTable.IO as IMH
import qualified Data.Set as Set
import qualified Test.HUnit as HUnit
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Calendar
import Data.Char
import Control.Monad as M
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.IORef
import Control.Exception.Base (BlockedIndefinitelyOnMVar, BlockedIndefinitelyOnSTM, AssertionFailed(..))
import GHC.Conc (getNumProcessors)
import Control.Concurrent (forkIO, setNumCapabilities, getNumCapabilities)
import qualified Control.Concurrent.Async as C
import qualified Control.DeepSeq as C
import qualified System.IO.Streams as S
import Data.Void
import qualified Data.Csv as CSV
import qualified Data.Vector as V
import Data.Either
import Data.Typeable
import Control.Concurrent.Async
import Control.Exception.Safe
import qualified Control.Monad.Catch as C
import Control.DeepSeq as DeepSeq
import Data.Maybe
import Data.List as L
import Debug.Trace
import System.IO.Unsafe

-- ------------------------------
-- Common types

-- | Fold elements `e` into state `s`.
--   Used for writing more compact types in foldl code.
type Foldl e s = s -> e -> s

-- | Fold elements `e` of a map with key `k`, into state `s`
type FoldlWithKey k e s = s -> k -> e -> s

-- | Fold elements `e` into state `s`
type Foldr e s = e -> s -> s

-- ------------------------------
-- Errors

pError :: [Char] -> a
pError msg
  = unsafePerformIO $ do
      putStrLn msg
      return $ error msg
{-# INLINE pError #-}

pAssert :: String -> Bool -> a -> a
pAssert msg t v = assert f v
 where
  {-# INLINE f #-}
  f = if t
      then True
      else unsafePerformIO $ do
             putStrLn msg
             return False
{-# INLINE pAssert #-}

fromJust1 :: String -> Maybe a -> a
fromJust1 n mA
  = case mA of
      Just a -> a
      Nothing -> pError $ "unexpected error in the application, in point labelled as \"" ++ n ++ "\""
{-# INLINE fromJust1 #-}

fromJust2 :: String -> Maybe (a, b) -> a
fromJust2 n mA
  = case mA of
      Just (a, _) -> a
      Nothing -> pError $ "unexpected error in the application, in point labelled as \"" ++ n ++ "\""
{-# INLINE fromJust2 #-}

fromRight1 :: Show e => String -> Either e a -> a
fromRight1 n mA
  = case mA of
      Right a -> a
      Left err -> pError $ "unexpected error in the application, in point labelled as \"" ++ n ++ "\": " ++ show err
{-# INLINE fromRight1 #-}

type LineNumber = Int

-- | True if the parsing error is application-related, False if it is customer configuration related.
type IsApplicationError = Bool

type ConfigurationErrors = Set.Set String

-- ------------------------------------------
-- Utils

maybeIf :: Bool -> Maybe a -> Maybe a
maybeIf True x = x
maybeIf False _ = Nothing
{-# INLINE maybeIf #-}

-- -------------------------------------------
-- String dictionary

-- | Used for flyweight pattern:
--   reuse the same string without allocating a new String.
--   To use outside the IO Monad.
type StringDictionary = HMap.HashMap T.Text T.Text

data StringIndexDictionary
       = StringIndexDictionary {
           sdict_count :: Int
         , sdict_map :: HMap.HashMap T.Text Int
         }
      deriving(Eq, Ord, Show)

sdict_empty :: StringIndexDictionary
sdict_empty = StringIndexDictionary {
                sdict_count = 0
              , sdict_map = HMap.empty
              }
{-# INLINE sdict_empty #-}

sdict_insert :: StringIndexDictionary -> T.Text -> (StringIndexDictionary, Int)
sdict_insert d1 t
    = case HMap.lookup t (sdict_map d1) of
        Just r -> (d1, r)
        Nothing -> let i = (sdict_count d1) + 1
                       m2 = (HMap.insert t i (sdict_map d1))
                   in  (d1 { sdict_count = i, sdict_map = m2 }, i)
{-# INLINE sdict_insert #-}

-- | The fastest dictionary, to use inside th IO monad.
type IOStringDictionary a = IMH.BasicHashTable a a

d_empty :: (Eq a, Hashable a) => IO (IOStringDictionary a)
d_empty = IMH.new
{-# INLINE d_empty #-}

-- | Return an already stored object, or store it.
d_get :: (Eq a, Hashable a) => IOStringDictionary a -> a -> IO a
d_get h x
    = IMH.mutate
        h
        x
        (\mv -> case mv of
                  Nothing -> (Just x, x)
                  Just y -> (Nothing, y))
{-# INLINE d_get #-}

-- -------------------------------
-- Parsing util functions

type Parser = P.Parsec Void T.Text

maybeQuoted :: Parser a -> Parser a
maybeQuoted p
  = (quoted p) <|> p

 where

  quoted :: Parser a -> Parser a
  quoted p = do
    char '"'
    r <- p
    char '"'
    return r
{-# INLINE maybeQuoted #-}

-- | Parse a field in CSV format, that is a string, and that is not likely to be repeated.
parseCSVString :: Char -> Parser T.Text
parseCSVString fieldSeparator = textField fieldSeparator
{-# INLINE parseCSVString #-}

-- | Skip a CSV field.
skipCSVField :: Char -> Parser ()
skipCSVField fieldSeparator
  = do _ <- textField fieldSeparator
       return ()
{-# INLINE skipCSVField #-}

-- | Parse a field in CSV format, that is a string, and that is not likely to be repeated.
--
parseCSVMaybeString :: Char -> Parser (Maybe T.Text)
parseCSVMaybeString fieldSeparator
  = do t <- textField fieldSeparator
       case t == "\\N" of
         True -> return Nothing
         False -> return $ Just t
{-# INLINE parseCSVMaybeString #-}


-- | Parse a Field in CSV format, using the string dictionary approach.
parseCSVMaybeSharedString :: Char -> StringDictionary -> Parser (Maybe T.Text, StringDictionary)
parseCSVMaybeSharedString fieldSeparator dict1
  = do text1 <- textField fieldSeparator <?> "field"
       case text1 == "\\N" of
         True
           -> return (Nothing, dict1)
         False
           -> case HMap.lookup text1 dict1 of
                Just text2
                  -> return (Just text2, dict1)
                Nothing
                  ->  let dict2 = HMap.insert text1 text1 dict1
                      in return (Just text1, dict2)

-- | Parse a Field in CSV format, using the string dictionary approach.
--
parseCSVSharedString :: Char -> StringDictionary -> Parser (T.Text, StringDictionary)
parseCSVSharedString fieldSeparator dict1
  = do text1 <- textField fieldSeparator <?> "field"
       case HMap.lookup text1 dict1 of
                Just text2
                  -> return (text2, dict1)
                Nothing
                  ->  let dict2 = HMap.insert text1 text1 dict1
                      in return (text1, dict2)


parse01AsBool :: Parser Bool
parse01AsBool = maybeQuoted $
    (do char '0'
        return False
    ) <|> (do char '1'
              return True)
{-# INLINE parse01AsBool #-}

parseMaybe01AsBool :: Parser (Maybe Bool)
parseMaybe01AsBool = maybeQuoted $
    (do char '0'
        return $ Just False
    ) <|> (do char '1'
              return $ Just True
    ) <|> (do (string "\\N")
              return $ Nothing
    )
{-# INLINE parseMaybe01AsBool #-}


parse_UnsignedInt :: Parser Int
parse_UnsignedInt
  = do dd <- P.takeWhileP Nothing isDigit
       case fromTextToInt dd of
         Nothing
           -> fail $ T.unpack dd ++ " is not an integer"
         Just i
           -> return i
{-# INLINE parse_UnsignedInt #-}

parseInt :: Parser Int
parseInt = do
  maybeQuoted dn
 where
   dn :: Parser Int
   dn = do s :: Char <- (P.char '+' <|> P.char '-' <|> return '+')
           d :: T.Text <- takeWhileP Nothing isDigit
           return $ fromJust $ fromTextToInt $ T.cons s d
{-# INLINE parseInt #-}

parseMaybeInt :: Parser (Maybe Int)
parseMaybeInt
  = (do string "\\N"
        return Nothing
    ) <|> (Just <$> parseInt)
{-# INLINE parseMaybeInt #-}

-- | The timezone used on the server for specifying the dates.
--
type RemoteTimeZone = TimeZone

-- | Parse a DateTime in MySQL format.
--
parseMySQLDateTimeToUTC :: Char -> RemoteTimeZone -> Parser UTCTime
parseMySQLDateTimeToUTC fieldSeparator timeZone
  = do str <- textField fieldSeparator
       time :: LocalTime <- case fromMySQLDateTimeAsTextToLocalTime str of
         Just t -> return t
         Nothing -> fail $ "\"" ++ show str ++ "\" is not a valid MySQL DateTime"
       return $ localTimeToUTC timeZone time
{-# INLINE parseMySQLDateTimeToUTC #-}

-- | Parse a DateTime in MySQL format.
--
parseMySQLDateTimeToLocalTime :: Char -> Parser LocalTime
parseMySQLDateTimeToLocalTime fieldSeparator
  = do str <- textField fieldSeparator
       time :: LocalTime <- case fromMySQLDateTimeAsTextToLocalTime str of
         Just t -> return t
         Nothing -> fail $ "\"" ++ show str ++ "\" is not a valid MySQL DateTime"
       return time
{-# INLINE parseMySQLDateTimeToLocalTime #-}

-- | Parse a DateTime in MySQL format.
--
parseMySQLDateTimeToMaybeLocalTime :: Char -> Parser (Maybe LocalTime)
parseMySQLDateTimeToMaybeLocalTime fieldSeparator
  = (do string "\\N"
        return Nothing
    ) <|> (Just <$> parseMySQLDateTimeToLocalTime fieldSeparator)
{-# INLINE parseMySQLDateTimeToMaybeLocalTime #-}


unquotedField :: Char -> Parser T.Text
unquotedField fieldSeparator
  = P.takeWhileP Nothing (\c -> not (c == fieldSeparator || c == '\r' || c == '\n'))
{-# INLINE unquotedField #-}

quotedField :: Parser T.Text
quotedField = do
   _ <- P.char '"'
   ts <- pieces
   _ <- P.char '"'
   return $ T.concat ts
 where

   pieces :: Parser [T.Text]
   pieces = do
     t1 <- P.takeWhileP Nothing (/= '"')
     t2 <- (do _ <- P.try (P.string "\"\"")
               -- this is a double quotation
               t3 <- pieces
               return $ ["\""] ++ t3
           ) <|> (return [])

     return $ t1:t2

textField :: Char -> Parser T.Text
textField fieldSeparator =
   quotedField <|> unquotedField fieldSeparator
{-# INLINE textField #-}

-- ---------------------------------
-- Text conversion functions

fromTextToInt :: T.Text -> Maybe Int
fromTextToInt t
  = case (T.signed T.decimal t) of
      Right (r, "") -> Just r
      _ -> Nothing
{-# INLINE fromTextToInt #-}

fromByteStringToInt :: BS.ByteString -> Maybe Int
fromByteStringToInt bs = fromTextToInt $ decodeUtf8 bs
{-# INLINE fromByteStringToInt #-}

fromStringToByteString :: String -> BS.ByteString
fromStringToByteString s = encodeUtf8 $ T.pack s
{-# INLINE fromStringToByteString #-}

fromByteStringToString :: BS.ByteString -> String
fromByteStringToString bs = T.unpack $ decodeUtf8With strictDecode bs
{-# INLINE fromByteStringToString #-}


fromByteStringToText :: BS.ByteString -> T.Text
fromByteStringToText bs = decodeUtf8 bs
{-# INLINE fromByteStringToText #-}


fromTextToByteString :: T.Text -> BS.ByteString
fromTextToByteString t = encodeUtf8 t
{-# INLINE fromTextToByteString #-}

fromTextToRational :: T.Text -> Maybe Rational
fromTextToRational t
  = case T.rational t of
      Right (r, "") -> Just r
      _ -> Nothing
{-# INLINE fromTextToRational #-}


fromTextToRational2 :: Char-> T.Text -> Maybe Rational
fromTextToRational2 decSeparator t1
  = let t2 = T.map (\c -> if c == decSeparator then '.' else c) t1
    in case T.rational t2 of
         Right (r, "") -> Just r
         _ -> Nothing
{-# INLINE fromTextToRational2 #-}

fromMySQLResultToText :: Maybe BS.ByteString -> T.Text
fromMySQLResultToText bs = decodeUtf8 $ fromJust1 "re-error" bs
{-# INLINE fromMySQLResultToText #-}

fromTextToMySQLResult :: T.Text -> BS.ByteString
fromTextToMySQLResult t = encodeUtf8 t
{-# INLINE fromTextToMySQLResult #-}

fromLazyToStrictByteString :: LBS.ByteString -> BS.ByteString
fromLazyToStrictByteString lb = LBS.toStrict lb
{-# INLINE fromLazyToStrictByteString #-}

fromLazyByteStringToString :: LBS.ByteString -> String
fromLazyByteStringToString s = LText.unpack $ LText.decodeUtf8 s
{-# INLINE fromLazyByteStringToString #-}

-- | Convert from a String in MySQL date-time format to an internal LocalTime.
--   Warning: this function is very slow, use instead `fromMySQLTimeToLocalTime`.
fromMySQLDateTimeToLocalTime :: String -> Maybe LocalTime
fromMySQLDateTimeToLocalTime str
  = fromMySQLDateTimeAsTextToLocalTime (T.pack str)
{-# INLINE fromMySQLDateTimeAsTextToLocalTime #-}

fromGammaCallDateAndTimeStampToLocalTime :: T.Text -> T.Text -> Maybe LocalTime
fromGammaCallDateAndTimeStampToLocalTime ddmmyyyy hhmmss
  = case P.runParser parseDDMMYYYY "" ddmmyyyy of
      Right (year1, month1, day1)
        -> case P.runParser parseHHMMSS "" hhmmss of
             Right (hour1, minutes1, seconds1)
               -> (Just $! LocalTime {
                             localDay = fromGregorian (toInteger year1) month1 day1
                           , localTimeOfDay = TimeOfDay {
                                               todHour = hour1
                                             , todMin = minutes1
                                             , todSec = fromInteger $ toInteger $ seconds1
                                             }
                        })
             _ -> Nothing
      _ -> Nothing
 where

  parseDDMMYYYY
    = do dd <- parse_UnsignedInt
         P.anySingle
         mm <- parse_UnsignedInt
         P.anySingle
         yyyy <- parse_UnsignedInt
         return (yyyy, mm, dd)

  parseHHMMSS
    = do hh <- parse_UnsignedInt
         P.anySingle
         mm <- parse_UnsignedInt
         P.anySingle
         ss <- parse_UnsignedInt
         return (hh, mm, ss)

-- | The calldate and timestamp can be separated from anything other than digits.
--   This function is fast.
fromMySQLDateTimeAsTextToLocalTime :: T.Text -> Maybe LocalTime
fromMySQLDateTimeAsTextToLocalTime !s
  = let (!finalState, _, _, _, !finalYear, !finalMonth, !finalDay, !finalHour, !finalMinutes, !finalSeconds) = T.foldl' process (1, 1000, True, 0, 0, 0, 0, 0, 0, 0) s

    in if (finalState /= 0)
       then (Just $! LocalTime {
                       localDay = fromGregorian (toInteger finalYear) finalMonth finalDay
                     , localTimeOfDay = TimeOfDay {
                                          todHour = finalHour
                                        , todMin = finalMinutes
                                        , todSec = (fromInteger $ toInteger finalSeconds)
                                       }
                     })
       else id $! Nothing

 where

  process :: (Int, Int, Bool, Int, Int, Int, Int, Int, Int, Int) -> Char -> (Int, Int, Bool, Int, Int, Int, Int, Int, Int, Int)
  process !p@(state, mult, mustBeDigit, currNum, year, month, day, hour, minutes, seconds) !c
    = case state of
           0 -> p
                -- error
           7 -> p
                -- not significant second digits
           _ -> case mustBeDigit of
                  False
                    -> case isDigit c of
                         True
                           -> (0, mult, mustBeDigit, currNum, year, month, day, hour, minutes, seconds)
                              -- error: a digit is found, when no digit was expected
                         False
                           -> (state + 1, 10, True, 0, year, month, day, hour, minutes, seconds)
                              -- go to next state, at for sure it is a two digit state (mult = 10)
                  True
                    -> case isDigit c of
                         False
                           -> (0, mult, mustBeDigit, currNum, year, month, day, hour, minutes, seconds)
                              -- error: a digit was not found, when a digit was expected
                         True
                           -> let !v = digitToInt c
                                  !currNum' = currNum + v * mult
                                  !newYear = if state == 1 then currNum' else year
                                  !newMonth = if state == 2 then currNum' else month
                                  !newDay = if state == 3 then currNum' else day
                                  !newHour = if state == 4 then currNum' else hour
                                  !newMinutes = if state == 5 then currNum' else minutes
                                  !newSeconds = if state == 6 then currNum' else seconds

                                  !newState = if mult == 1 && state == 6 then 7 else state

                              in if mult == 1
                                 then (newState, mult, False, 0, newYear, newMonth, newDay, newHour, newMinutes, newSeconds)
                                 else (state, div mult 10, True, currNum', year, month, day, hour, minutes, seconds)

-- | Convert something like "August 2015" to "2015-08-01 00.00.00"
fromGammaItemRentalCallDateToLocalTime :: T.Text -> Maybe LocalTime
fromGammaItemRentalCallDateToLocalTime monthAndYear
  = case P.runParser parseMonthAndYear "" monthAndYear of
      Right (Just (year1, month1))
        -> (Just $! LocalTime {
                             localDay = fromGregorian (toInteger year1) month1 1
                           , localTimeOfDay = TimeOfDay {
                                               todHour = 0
                                             , todMin = 0
                                             , todSec = 0
                                             }
                        })
      _ -> Nothing
 where

  parseMonthAndYear
    = do monthS <- P.takeWhileP Nothing (\w -> not $ isSpace w)
         P.spaceChar
         yyyy <- parse_UnsignedInt
         let monthM
               = case monthS of
                   "January" -> Just 1
                   "February" ->  Just 2
                   "March" ->Just 3
                   "April" ->Just 4
                   "May" ->Just 5
                   "June" ->Just 6
                   "July" ->Just 7
                   "August" ->Just 8
                   "September" ->Just 9
                   "October" ->Just 10
                   "November" ->Just 11
                   "December" ->Just 12
                   _ -> Nothing
         case monthM of
           Nothing -> return Nothing
           Just m -> return $ Just $ (yyyy, m)

-- --------------------------------------
-- Time related

-- | A format like this
--   > 2014/11/05 08.28.54
--   > 2014-11-05 08.28.54
fromDateFormat1ToLocalTime :: T.Text -> Maybe LocalTime
fromDateFormat1ToLocalTime = fromMySQLDateTimeAsTextToLocalTime
{-# INLINE fromDateFormat1ToLocalTime #-}

-- | A italian format like this
 --   > 25/12/2016 08.28.54
fromDateFormat2ToLocalTime :: Char -> Char -> Char -> T.Text -> Maybe LocalTime
fromDateFormat2ToLocalTime dateSep dateAndTimeSep timeSep dateAndTime
  = case P.runParser parseDateAndTime "" dateAndTime of
      Right (year1, month1, day1, hour1, minutes1, seconds1)
          -> (Just $! LocalTime {
                             localDay = fromGregorian (toInteger year1) month1 day1
                           , localTimeOfDay = TimeOfDay {
                                               todHour = hour1
                                             , todMin = minutes1
                                             , todSec = fromInteger $ toInteger $ seconds1
                                             }
                        })
      _ -> Nothing
 where

  parseDateAndTime
    = do dd <- parse_UnsignedInt
         P.char dateSep
         mm <- parse_UnsignedInt
         P.char dateSep
         yyyy <- parse_UnsignedInt
         P.char dateAndTimeSep
         hhh <- parse_UnsignedInt
         P.char timeSep
         mmm <- parse_UnsignedInt
         P.char timeSep
         sss <- parse_UnsignedInt
         return (yyyy, mm, dd, hhh, mmm, sss)

showLocalTime :: LocalTime -> String
showLocalTime = fromLocalTimeToMySQLDateTime
{-# INLINE showLocalTime #-}

fromLocalTimeToMySQLDateTime :: LocalTime -> String
fromLocalTimeToMySQLDateTime t
  = formatTime defaultTimeLocale "%F %T" t
{-# INLINE fromLocalTimeToMySQLDateTime #-}

-- | Convert to a date without the time part.
fromLocalTimeToMySQLDateWith00Time :: LocalTime -> String
fromLocalTimeToMySQLDateWith00Time t
  = formatTime defaultTimeLocale "%F" t
{-# INLINE fromLocalTimeToMySQLDateWith00Time #-}

-- | Return the same LocalTime but at the beginning of the day, e.g. 00:00:00 time.
toBeginOfTheDay :: LocalTime -> LocalTime
toBeginOfTheDay t1 = t1 { localTimeOfDay = midnight }
{-# INLINE toBeginOfTheDay #-}

-- | Return today at midnight.
today :: LocalTime -> LocalTime
today = toBeginOfTheDay
{-# INLINE today #-}

-- | Return yesterday at midnight.
yesterday :: LocalTime -> LocalTime
yesterday t1
    = LocalTime {
        localTimeOfDay = midnight
      , localDay = addDays (-1) (localDay t1)
      }
{-# INLINE yesterday #-}

-- | Return tomorrow at midnight.
tomorrow :: LocalTime -> LocalTime
tomorrow t1
    = LocalTime {
        localTimeOfDay = midnight
      , localDay = addDays 1 (localDay t1)
      }
{-# INLINE tomorrow #-}

showLocalTimeUsingMySQLFormat = showLocalTime
{-# INLINE showLocalTimeUsingMySQLFormat #-}

validCallTime :: Bool -> LocalTime -> LocalTime -> Maybe LocalTime -> Bool
validCallTime isServiceCDR timeToTest fromCallDate toCallDateOrAll
     = case timeToTest >= fromCallDate of
         True -> case toCallDateOrAll of
           Nothing
             -> True
           Just toCallDate
             -> case isServiceCDR of
                  True
                    -> timeToTest <= toCallDate
                  False
                    -> timeToTest < toCallDate
         False
           -> False

whenM_ :: (Monad m) => m Bool -> m a -> m ()
whenM_ p f
  = do b <- p
       case b of
         True -> do f
                    return ()
         False -> return ()

-- | Haskell uses for round the IEEE floating convention instead of the
--   math notation. So 0.5 is rounded to 0, while 1.5 to 2, in order to compensating
--   the rounding "errors". In Asterisell I want use instead the exact notation.
--
mathRound :: Rational -> Integer
mathRound x
  = let (i, f) = properFraction x
    in  if f < 0.5
        then i
        else i + 1

-- | Get the ported telephone number substituting a prefix.
--   Return Nothing if the substitution can not be done.
replacePrefixAndGetPortedTelephoneNumber :: Bool -> String -> String -> String -> Maybe String
replacePrefixAndGetPortedTelephoneNumber onlySafeTWTNumbers sourceNumber sourcePrefix destPrefix
  = let l = length sourcePrefix
        sl = length sourceNumber

    in case (onlySafeTWTNumbers && isSafeTWTNumber sourceNumber && isSafeTWTNumber destPrefix) || (not onlySafeTWTNumbers) of
         False -> Nothing
         True ->
           case l > sl of
             True -> Nothing
             False -> Just $ destPrefix ++ (drop l sourceNumber)

-- | TWT uses sometime malformed numbers.
--   Consider as "safe" a correct Italian mobile number starting with "393".
--   Do not port fixed-line telephone numbers.
isSafeTWTNumber :: String -> Bool
isSafeTWTNumber n = isPrefixOf "393" n
{-# INLINE isSafeTWTNumber #-}

-- ----------------------------------
-- MySQL LOAD DATA (CSV-like) format

toMySQLCSV_bool :: Bool -> B.Builder
toMySQLCSV_bool True = B.charUtf8 '1'
toMySQLCSV_bool False = B.charUtf8 '0'
{-# INLINE toMySQLCSV_bool #-}

toMySQLCSV_text :: T.Text -> B.Builder
toMySQLCSV_text t = B.byteString $ fromTextToByteString $ toMySQLCSV_escaped t
{-# INLINE toMySQLCSV_text #-}

toMySQLCSV_byteString :: BS.ByteString -> B.Builder
toMySQLCSV_byteString t = B.byteString $ fromTextToByteString $ toMySQLCSV_escaped $ fromByteStringToText t
{-# INLINE toMySQLCSV_byteString #-}

toMySQLCSV_int :: Int -> B.Builder
toMySQLCSV_int x = B.intDec x
{-# INLINE toMySQLCSV_int #-}

toMySQLCSV_int32 :: Int32 -> B.Builder
toMySQLCSV_int32 x = B.int32Dec x
{-# INLINE toMySQLCSV_int32 #-}

toMySQLCSV_ids :: [Int] -> B.Builder
toMySQLCSV_ids ids = B.charUtf8 '/' <> (mconcat $ L.map (\i -> B.intDec i <> B.charUtf8 '/') ids)
{-# INLINE toMySQLCSV_ids #-}

toMySQLCSV_maybe :: (a -> B.Builder) -> Maybe a -> B.Builder
toMySQLCSV_maybe _ Nothing = B.byteString "\\N"
toMySQLCSV_maybe b (Just a) = b a
{-# INLINE toMySQLCSV_maybe #-}

toMySQLCSV_maybeBool :: Maybe Bool -> B.Builder
toMySQLCSV_maybeBool mx = toMySQLCSV_maybe toMySQLCSV_bool mx
{-# INLINE toMySQLCSV_maybeBool #-}

toMySQLCSV_maybeText :: Maybe T.Text -> B.Builder
toMySQLCSV_maybeText mx = toMySQLCSV_maybe toMySQLCSV_text mx
{-# INLINE toMySQLCSV_maybeText #-}

toMySQLCSV_maybeByteString :: Maybe BS.ByteString -> B.Builder
toMySQLCSV_maybeByteString mx = toMySQLCSV_maybe toMySQLCSV_byteString mx
{-# INLINE toMySQLCSV_maybeByteString #-}

toMySQLCSV_maybeInt :: Maybe Int -> B.Builder
toMySQLCSV_maybeInt mx = toMySQLCSV_maybe toMySQLCSV_int mx
{-# INLINE toMySQLCSV_maybeInt #-}

toMySQLCSV_maybeIds :: Maybe [Int] -> B.Builder
toMySQLCSV_maybeIds mx = toMySQLCSV_maybe toMySQLCSV_ids mx
{-# INLINE toMySQLCSV_maybeIds #-}

-- | Convert a local-time to a MySQL compatible string.
--   NOTE: this is called a lot of time, so it had to be fast.
toMySQLCSV_localTime :: LocalTime -> B.Builder
toMySQLCSV_localTime t
  =
      ((intDec0000 yearI)
        <> B.charUtf8 '-'
        <> (intDec00 month)
        <> B.charUtf8 '-'
        <> (intDec00 day)
        <> B.charUtf8 ' '
        <> (intDec00 (todHour tod))
        <> B.charUtf8 ':'
        <> (intDec00 (todMin tod))
        <> B.charUtf8 ':'
        <> (intDec00 (round $! todSec tod)))

 where

   !tod = localTimeOfDay t

   (!year, !month, !day) = toGregorian $ localDay t

   !yearI = fromInteger year

   intDec0000 i
     = if i >= 1000
       then B.intDec i
       else (if i >= 100
             then B.charUtf8 '0' <> B.intDec i
             else (if i >= 10
                   then B.stringUtf8 "00" <> B.intDec i
                   else B.stringUtf8 "000" <> B.intDec i))

   intDec00 i
     = if i < 10
       then B.charUtf8 '0' <> B.intDec i
       else B.intDec i

toMySQLCSV_maybeLocalTime :: Maybe LocalTime -> B.Builder
toMySQLCSV_maybeLocalTime ml = toMySQLCSV_maybe toMySQLCSV_localTime ml
{-# INLINE toMySQLCSV_maybeLocalTime #-}

toMySQLCSV_escaped :: T.Text -> T.Text
toMySQLCSV_escaped t
  = case T.any isToEscape t of
      False -> t  -- NOTE: the majority of the times this is the case, so the code is optimized for this
      True -> T.concat $ esc [] t
 where

  isToEscape :: Char -> Bool
  isToEscape c = (c == '\t' || c == '\n' || c == '\r' || c == '\\' || c == '\0')

  toEscape :: Char -> Char
  toEscape '\t' = 't'
  toEscape '\n' = 'n'
  toEscape '\r' = 'r'
  toEscape '\\' = '\\'
  toEscape '\0' = '0'

  esc :: [T.Text] -> T.Text -> [T.Text]
  esc r1 t
    = let (t1, t2) = T.break isToEscape t
          r2 = r1 ++ [t1]
      in  case T.uncons t2 of
            Nothing
                -> r2
            Just (c, t3)
                -> let r3 = r2 ++ [T.singleton '\\', T.singleton $ toEscape c]
                   in  esc r3 t3



-- ----------------------------------
-- Channels

-- | Extract from a string like "IAX2/gw-to-neax/984957777710,300,Tt", the extension "984957777710"
--   An empty string in case the format is not recognized.
fromAsteriskLastDataToExtension :: T.Text -> T.Text
fromAsteriskLastDataToExtension s1
  = case T.splitOn "/" s1 of
      [_, _, s2] -> T.takeWhile (isNot ',') s2
      _ -> ""
 where

   isNot :: Char -> Char -> Bool
   isNot c1 c2 = not $ c1 == c2

-- ---------------------------------------------
-- A map containing a list of associated values

type MultiMap k v = Map.Map k [v]

multiMap_add :: (Ord k) => MultiMap k v -> k -> v -> MultiMap k v
multiMap_add map1 k v
  = Map.insertWith (\newValue oldValue -> newValue ++ oldValue) k [v] map1

-- --------------
-- List utils

mapAccumLM :: (s -> b -> IO (s, c)) -> s -> [b] -> IO (s, [c])
mapAccumLM act s1 bs = mapAccumLM' act (s1, []) bs
 where

  mapAccumLM' _ (s1, cs) [] = return (s1, L.reverse cs)
  mapAccumLM' act (s1, cs) (b:bs) = do
    (!s2, !c2) <- act s1 b
    mapAccumLM' act (s2, c2:cs) bs

-- --------------
-- CmdParams

-- | Read command-like params from a CSV file.
--   Doing so: they are more secrets, respect sending in clear on the command line,
--   and more info can be transferred to the rating engine.
type CmdParams = Map.Map BS.ByteString BS.ByteString

cparams_load :: String -> IO CmdParams
cparams_load fileName = do
  c <- LBS.readFile fileName
  case CSV.decode CSV.NoHeader c of
    Left err -> pError ("Error during decoding of " ++ fileName ++ ": " ++ err)
    Right vv -> return $ V.foldl' (\m1 v -> Map.insert (V.head v) (V.last v) m1) Map.empty vv

cparams_get :: CmdParams -> BS.ByteString -> BS.ByteString
cparams_get pp n = case Map.lookup n pp of
                     Nothing -> pError ("Missing cmd param " ++ fromByteStringToString n ++ " inside " ++ show pp)
                     Just v -> v

-- --------------------------------------------------
-- Multithreading processing of chunks of data

-- | Return the number of available jobs.
--   @require to be called before multi-core processing, otherwise GHC runtime will not use all of them
process_initCores
    :: Int -- ^ 0 for using all available cores
    -> IO Int
process_initCores cores = do
  t <- case cores of
         0 -> do c <- getNumProcessors
                 return $ if c > 2 then (c -1) else 1
                 -- NOTE: leave a CPU for the DBMS and OS, so it is all faster, without thread context switches
         c -> return c

  setNumCapabilities t
  return t

-- | A chunk of data.
--   Data is processed at chunks because usually calcs in each phase are cheaps,
--   and the objective is using data on local CPU cache for a while,
--   before passing to next processing phase.
--   This in case there are less logical threads than cores.
type Chunk a = V.Vector a

-- | A stream of Chunks, following the ordered of `process_orderedChunks`.
--   Nothing when the stream end (EOF)
type OrderedStream a = MVar (Maybe (Chunk a))

orderedStream_put
  :: (NFData a)
  => OrderedStream a -- ^ out channel
  -> Maybe (Chunk a) -- ^ Nothing for closing the stream
  -> IO () -- ^ send a DeepSeq.force chunk on the OrderedStream

orderedStream_put out v = putMVar out $ DeepSeq.force v
{-# INLINE orderedStream_put  #-}

-- | Manage jobs requiring ordered data, ensuring that ordered results will be produced.
--   The idea is having jobs reading data from an input MVar, and writing the result to another MVar.
--   This function will coordinate these jobs, sending data to process in order, and retrieving results in order.
process_orderedChunks
  :: String
     -- ^ process name, for debug reasons
  -> OrderedStream a
  -- ^ input data
  -> V.Vector (OrderedStream a)
  -- ^ where the processing jobs are waiting the income data to process
  --   @ensure they will be called in order, and in loop (i.e. the last job will call the first, restarting the cycle)
  -> V.Vector (OrderedStream b)
  -- ^ where the processing jobs write their result.
  --   @require for each input MVar there is a corresponding out MVar at the same position,
  --   i.e. the same job at position `i`, read from MVar at position `i`, and write
  --   result at the same position.
  --   @require after a job emit EOF (Nothing) no any other data will be emitted from the same job
  --   @require a job emit only one chunk of data, for each input chunk processed, otherwise it is not clear how to produce ordered data
  --   @require a job can emit a chunk of data followed by an EOF only in case it receives an EOF signal,
  --   and the data sent correspond to the data to produce in case of EOF, for closing the work.
  --   @ensure a job can emit empty vector, if at a certain computation pass there is no data to produce
  --   @ensure EOF (Nothing) signal can be used from the job for closing resources and calculating final results,
  --   while its output EOF signal is used from the manager job for knowing when all the jobs terminated
  --   their processing.
  -> OrderedStream b
  -- ^ where the consumer will read the produced data
  -- @ensure it will contain ordered data
  -> Bool
  -- ^ True for propagating the Nothing signal (EOF) to the outChan
  -- @ensure jobs will receive the Nothing signal in any case, this affects only the last outChan
  -> IO (V.Vector (Async ()))
  -- ^ the process to close at the end of the work

process_orderedChunks processName inChan inJobsChan outJobsChan outChan propagateEOF = do
   jobEOF <- newEmptyMVar
   pid1 <- async $ inJob jobEOF 0
   pid2 <- async $ outJob nrOfJobs jobEOF 0
   return $ V.fromList [pid1, pid2]

  where

   nrOfJobs :: Int
   nrOfJobs = pAssert "ERR 10" (V.length inJobsChan == V.length outJobsChan) $ V.length inJobsChan
   {-# INLINE nrOfJobs #-}

   allJobs :: Int -> [Int]
   allJobs startPos
     = [startPos .. (nrOfJobs - 1)] ++ [0 .. (startPos - 1)]
   {-# INLINE allJobs #-}

   nextPos :: Int -> Int
   nextPos i = mod (i + 1) nrOfJobs
   {-# INLINE nextPos #-}

   showPos :: Int -> String
   showPos i = show (i + 1) ++ "/" ++ show nrOfJobs

   -- | A synchronous producers, reading the input data from the unique MVar, and sending to parallel jobs in order.
   inJob jobEOF outPos = do
     mv <- takeMVar inChan
     case mv of
       Just v
         -> do
               -- putStrLn $ processName ++ " inJob: from pos " ++ showPos outPos
               putMVar ((V.!) inJobsChan outPos) (Just v)
               inJob jobEOF (nextPos outPos)
       Nothing
         -> do
               M.mapM_ (\i -> putMVar ((V.!) inJobsChan i) Nothing) (allJobs outPos)
               -- putStrLn $ processName ++ " inJob: sent EOF to " ++ show (allJobs outPos)
               waitForLastInJobEOF jobEOF nrOfJobs

   -- | And indipendent job sending to the unique result MVar the results in order of processing of the parallel jobs.
   --   So this is a consumer.
   --   The producers will wait, if the consumer has not consumed a certain position.
   --   This consumer will wait, if the producers have not filled the MVar with the result.
   outJob activeJobs jobEOF outPos = do
     mv <- takeMVar ((V.!) outJobsChan outPos)
     case mv of
       Just v
         -> do
               putMVar outChan (Just v)
               -- putStrLn $ processName ++ " outJob: processed data " ++ showPos outPos
               outJob activeJobs jobEOF (nextPos outPos)
       Nothing
         -> do
               putMVar jobEOF ()
               -- putStrLn $ processName ++ " outPos: processed EOF " ++ showPos outPos
               case activeJobs > 1 of
                 True -> outJob (activeJobs - 1) jobEOF (nextPos outPos)
                 False -> return ()

   -- | Wait all jobs received and processed the Nothing signal, and then send a unique EOF signal on the outChan.
   waitForLastInJobEOF jobEOF activeJobs
     = case activeJobs == 0 of
         True -> do
                    when propagateEOF (putMVar outChan Nothing)
                    -- putStrLn $ processName ++ " all jobs terminated. Sent EOF to the out channel."
         False -> do
                     eofFromJob <- takeMVar jobEOF
                     -- putStrLn $ processName ++ " waiting termination of left jobs " ++ show activeJobs
                     -- NOTE: start processing EOF signals only after the first job produce an EOF
                     waitForLastInJobEOF jobEOF (activeJobs - 1)

-- | Create a `process_orderedChunks` using a specified function.
process_orderedChunksUsingFun
  :: (NFData b)
  => String
     -- ^ process name for debug reasons
  -> OrderedStream a
  -- ^ the input data
  -> Int
  -- ^ the number of processing jobs
  -> (a -> IO b)
  -- ^ the processing function
  -> OrderedStream b
  -- ^ the final result of the processing chain.
  -- Result will be DeepSeq by default.
  -- @ensure it will contain ordered data
  -> Bool
  -- ^ True for propagating the Nothing signal (EOF) to outChan.
  -- @ensure jobs will receive the Nothing signal in any case, this affects only the last outChan
  -> IO (V.Vector (Async ()))
     -- ^ the pid to close at the end

process_orderedChunksUsingFun processName inChan nrOfJobs procFun outChan propagateEOF = do
  inChans :: V.Vector (OrderedStream a) <- V.fromList <$> M.replicateM nrOfJobs newEmptyMVar
  outChans :: V.Vector (OrderedStream b) <- V.fromList <$> M.replicateM nrOfJobs newEmptyMVar

  jobs1 <- V.mapM (\(inJobChan, outJobChan) -> async $ processAll inJobChan outJobChan) (V.zip inChans outChans)

  jobs2 <- process_orderedChunks processName inChan inChans outChans outChan propagateEOF

  return $ V.concat [jobs1, jobs2]

 where

   processAll inJobChan outJobChan = do
     mv <- takeMVar inJobChan
     case mv of
       Nothing -> do
         putMVar outJobChan Nothing
       Just v -> do

         v' <- V.mapM procFun v
         orderedStream_put outJobChan $ Just v'
         processAll inJobChan outJobChan

-------------------------
-- Streams

-- | A chunked packet contains a sequence of data to process.
--   CDRS are small, so it is better processing them in chunks, reducing communication overhead,
--   and being CPU cache friendly.

stream_toJust :: S.InputStream (Maybe a) -> IO (S.InputStream a)
stream_toJust in1 = S.filter isJust in1 >>= S.map fromJust
{-# INLINE stream_toJust #-}

stream_mapM :: S.InputStream a -> (a -> IO b) -> IO (S.InputStream b)
stream_mapM inS f = S.mapM f inS
{-# INLINE stream_mapM #-}

stream_map :: S.InputStream a -> (a -> b) -> IO (S.InputStream b)
stream_map inS f = S.map f inS
{-# INLINE stream_map #-}

-- | Execute a map, forcing a deepSeq.
stream_forceMap :: (NFData b) => S.InputStream a -> (a -> b) -> IO (S.InputStream b)
stream_forceMap inS f = S.map (\x -> C.force $ f x) inS
{-# INLINE stream_forceMap #-}

-- | Generate values in a lazy/efficient way.
stream_sequence
  :: b
  -- ^ the initial seed nerator
  -> (b -> IO (Maybe (S.InputStream a, b)))
  -- ^ the stream and the next seed to use when the stream will be fully consumed,
  -- or Nothing if the end of the stream sequence is reached
  -> IO (S.InputStream a)
  -- ^ a stream returning all the values of the sequence in an efficient and lazy way

stream_sequence seed0 generator = do
  fakeInitStream <- S.nullInput
  stateR <- newIORef (fakeInitStream, seed0)
  S.makeInputStream (g stateR)

 where

  g stateR = do
    (s, seed1) <- readIORef stateR
    mv <- S.read s
    case mv of
      Nothing
        -> do maybeStream <- generator seed1
              case maybeStream of
                Nothing
                  -> return Nothing
                Just state'
                  -> do modifyIORef' stateR (\_ -> state')
                        -- MAYBE IORef are prone to space-leak, evaluate to use an MVar instead
                        g stateR
      _
        -> return mv

-- | Send all values and return the last value.
stream_toOrderedStream :: NFData a => Bool -> S.InputStream (Chunk a) -> OrderedStream a -> Maybe a -> IO (Maybe a)
stream_toOrderedStream propagateEOF inS outChan !maybeLast = do
  mv <- S.read inS
  case mv of
    Nothing -> do
      when propagateEOF (orderedStream_put outChan Nothing)
      return maybeLast
    Just v -> do
      let v' = DeepSeq.force v
      putMVar outChan (Just v')
      stream_toOrderedStream propagateEOF inS outChan (Just $ V.last v')

-- -----------------------
-- Brackets

-- | Acquire a resource, process, and release the resource in case of synchrounous or asynchronous exceptions.
--   Exceptions are always re-thrown, but only synchronous exceptions (i.e error inside the processing phase)
--   can be enriched with informative messages to the user, while asynchronous exceptions (i.e. requests of interruption from the outside)
--   are not enriched.
--   The code is inspired from `Control.Exception.Safe`
--   See notes on https://haskell-lang.org/tutorial/exception-safety and https://github.com/fpco/safe-exceptions#quickstart.
withResource
  :: (C.MonadCatch m, C.MonadMask m, MonadIO m)
  => m b
  -- ^ acquire the resource
  -> (b -> m a)
  -- ^ process the result
  -> (Bool
      -- ^ True in case of succes, False in case of exception during processing
      -> b
      -- ^ the resource to release
      -> m ())
  -> (SomeException
      -- ^ the original synchronous exception (i.e. an error inside the processing phase).
      -> SomeException
      -- ^ the exception to thrown, meant to contain more informative messages respect the initial exception
     )
  -> m a
  -- ^ the result of the processing, or an exception

withResource acquire process release informativeException = do
    !maybeResource <- C.try acquire
    case maybeResource of
      Left (e1 :: SomeException) -> C.throwM $ if isSyncException e1 then informativeException e1 else e1
      Right !resource -> do
        res1 <- C.try $ process resource
        case res1 of
            Left (e1 :: SomeException) -> do
                _ :: Either SomeException () <- C.try $ C.mask_ $ release False resource
                C.throwM $ if isSyncException e1 then informativeException e1 else e1
            Right y -> do
                C.mask_ $ release True resource
                return y

-- | Like `withResource` but the result is fully evaluated (i.e. DeepSeq)
withResource'
  :: (C.MonadCatch m, C.MonadMask m, MonadIO m, NFData a)
  => m b
  -- ^ acquire the resource
  -> (b -> m a)
  -- ^ process the result
  -> (Bool
      -- ^ True in case of succes, False in case of exception during processing
      -> b
      -- ^ the resource to release
      -> m ())
  -> (SomeException
      -- ^ the original synchronous exception (i.e. an error inside the processing phase)
      -> SomeException
      -- ^ the exception to throw, meant to contain more informative messages respect the initial exception
     )
  -> m a

withResource' acquire process release informativeError
    = withResource acquire (\b -> do !r <- process b; return $ DeepSeq.force r) release informativeError

-- | Like `withResource'` but the resources are closed only in case of error in the computation.
withResource''
  :: (C.MonadCatch m, C.MonadMask m, MonadIO m, NFData a)
  => m b
  -- ^ acquire the resource
  -> (b -> m a)
  -- ^ process the result, and close the resources
  -> (b -- ^ the resource to release in case of error
      -> m ())
  -> (SomeException
      -- ^ the original synchronous exception (i.e. an error inside the processing phase)
      -> SomeException
      -- ^ the exception to throw, meant to contain more informative messages respect the initial exception
     )
  -> m a

withResource'' acquire process releaseOnError informativeError
    = withResource'
        acquire
        (\b -> do !r <- process b; return $ DeepSeq.force r)
        (\isSuccess r -> case isSuccess of
                           True -> return ()
                           False -> releaseOnError r)
        informativeError



-- -----------------------
-- Unit Tests Utils

isDescendingOrder :: (Ord a) => [a] -> Bool
isDescendingOrder [] = True
isDescendingOrder [_] = True
isDescendingOrder (a:b:r) = if a >= b then isDescendingOrder (b:r) else False

isAscendingOrder :: (Ord a) => [a] -> Bool
isAscendingOrder [] = True
isAscendingOrder [a] = True
isAscendingOrder l = not $ isDescendingOrder l

-- ------------------------------
-- Tests
--

tt_parseTests
  = [ testParse "1" "one" (P.string "one") (T.pack "one")
    , testParse "2" "1" (parse01AsBool) (True)
    , testParse "3" "0" (parse01AsBool) (False)
    , testParse "4" "1" (parseMaybe01AsBool) (Just True)
    , testParse "5a" "0" (parseMaybe01AsBool) (Just False)
    , testParse "5b" "\"1\"" (parseMaybe01AsBool) (Just True)
    , testParse "6" "\"0\"" (parseMaybe01AsBool) (Just False)
    , testParse "7" "\\N" (parseMaybe01AsBool) (Nothing)
    , testParse "8a" "hello" (parseCSVMaybeString ',') (Just $ T.pack "hello")
    , testParse "8b" "hello\"\"here" (parseCSVMaybeString ',') (Just $ T.pack "hello\"\"here")
    , testParse "8c" "hello\"here" (parseCSVMaybeString ',') (Just $ T.pack "hello\"here")
    , testParse "9" "\"hello\"" (parseCSVMaybeString ',') (Just $ T.pack "hello")
    , testParse "10" "\\N" (parseCSVMaybeString ',') (Nothing)
    , testParse "11" "1" (parseInt) (1)
    , testParse "12" "\"1\"" (parseInt) (1)
    , testParse "13" "hello world" (textField ',') (T.pack "hello world")
    , testParse "14" "hello;world\"" (textField ',') "hello;world\""
    , testParse "15" "\"hello world\"" (textField ',') "hello world"
    , testParse "16.1" "\"hello,world\"\"here\"\"\"" (textField ',') "hello,world\"here\""
    , testParse "16.2" "\"USALS - Ilizwe(IWN), Non-geographic\"" (textField ',') "USALS - Ilizwe(IWN), Non-geographic"
    , testParse "16.3" "one,two,\"three\",four"  (do v1 <- parseCSVString ','
                                                     _ <- P.char ','
                                                     v2 <- parseCSVString ','
                                                     _ <- P.char ','
                                                     v3 <- parseCSVString ','
                                                     _ <- P.char ','
                                                     v4 <- parseCSVString ','
                                                     return (v1, v2, v3, v4)
                                                 ) (T.pack "one", T.pack "two", T.pack "three", T.pack "four")
    , HUnit.TestCase $ HUnit.assertEqual "17a" (Just "123456") (replacePrefixAndGetPortedTelephoneNumber False "000456" "000" "123")
    , HUnit.TestCase $ HUnit.assertEqual "17b" Nothing (replacePrefixAndGetPortedTelephoneNumber True "000456" "000" "123")
    , HUnit.TestCase $ HUnit.assertEqual "17c" (Just "393056") (replacePrefixAndGetPortedTelephoneNumber True "393456" "3934" "3930")
    , testLocalTimeParsing "18" "2015-01-02 03:04:05"
    , testLocalTimeParsing "19" "2015-11-13 10:12:54"
    , testLocalTimeParsing "20" "2015-2537y-13 10:12:54"
    , testMySQLEscape "abc" "abc"
    , testMySQLEscape "abc,def" "abc,def"
    , testMySQLEscape "a\\tb" "a\tb"
    , testMySQLEscape "a\\\\b" "a\\b"
    , testMySQLEscape "a\\ta\\\\b" "a\ta\\b"
    , testMySQLEscape "\\na\\t\\tab\\rciao" "\na\t\tab\rciao"
    , testMySQLEscape "\\0null\\0" "\0null\0"
    , testMySQLEscape "hello'no-escape'" "hello'no-escape'"
    , testMySQLEscape "hello\\\\'world\\\\'" "hello\\'world\\'"
    ]

 where

  testMySQLEscape :: T.Text -> T.Text -> HUnit.Test
  testMySQLEscape e s = HUnit.TestCase $ HUnit.assertEqual ("mysql escape of " ++ T.unpack s) e (toMySQLCSV_escaped s)

  parseMaybeTime :: TimeLocale -> String -> String -> Maybe LocalTime
  parseMaybeTime tl formatStr inStr
    = parseTimeM False tl formatStr inStr

  testLocalTimeParsing n t1
    = HUnit.TestCase $ HUnit.assertEqual n  (parseMaybeTime defaultTimeLocale "%F %T" (T.unpack t1)) (fromMySQLDateTimeAsTextToLocalTime t1)

  testParse :: (Eq a, Show a) => String -> T.Text -> Parser a -> a -> HUnit.Test
  testParse n s p v
    = let
          completeParser = do
            r <- p
            P.eof
            return r

          parserResult
            = case runParser completeParser "" s of
                Right r -> Right r
                Left err -> Left $ P.errorBundlePretty err

      in HUnit.TestCase $ HUnit.assertEqual n parserResult (Right v)
