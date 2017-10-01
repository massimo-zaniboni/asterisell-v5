{-# Language OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

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


-- | Utils functions used from other modules.
--
module Asterisell.Utils (
  StringDictionary,
  RemoteTimeZone,
  LineNumber,
  fromAsteriskLastDataToExtension,
  parseAllContentTest,
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
  tt_parseTests,
  IsApplicationError,
  ConfigurationErrors,
  replacePrefixAndGetPortedTelephoneNumber,
  isAscendingOrder,
  isDescendingOrder,
  toMySQLCSVString,
  fromMySQLResultToText,
  fromLazyToStrictByteString,
  fromTextToMySQLResult,
  skipCSVField,
  fromDateFormat2ToLocalTime,
) where

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>))
import Data.Attoparsec.Text.Lazy
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Test.HUnit as HUnit
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time.Calendar
import Data.Char
import Data.Maybe
import Control.Monad as M
import Debug.Trace
import qualified Control.Concurrent as C (getNumCapabilities)
import qualified Control.Concurrent.Async as C
import qualified Control.DeepSeq as C
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import qualified System.IO.Streams.List as S
import System.IO.Streams.Attoparsec
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.File as S
import qualified System.IO.Streams.Vector as S
import qualified Data.Vector as V 
import Control.Concurrent.BoundedChan

type LineNumber = Int

fromJust1 :: String -> Maybe a -> a
fromJust1 n mA
  = case mA of
      Just a -> a
      Nothing -> error $ "unexpected error in the application, in point labelled as \"" ++ n ++ "\""

fromJust2 :: String -> Maybe (a, b) -> a
fromJust2 n mA
  = case mA of
      Just (a, _) -> a
      Nothing -> error $ "unexpected error in the application, in point labelled as \"" ++ n ++ "\""


-- | True if the parsing error is application-related, False if it is customer configuration related.
type IsApplicationError = Bool

type ConfigurationErrors = Set.Set String

-- | Used for flyweight pattern:
--   reuse the same string without allocating a new String.
type StringDictionary = Map.Map T.Text T.Text

parseAllContentTest :: Parser a -> Parser a
parseAllContentTest p
  = do r <- p
       endOfInput
       return r

maybeQuoted p
  = (quoted p) <|> p

 where

  quoted p = do
    char '"'
    r <- p
    char '"'
    return r

-- | Parse a field in CSV format, that is a string, and that is not likely to be repeated.
parseCSVString :: Char -> Parser T.Text
parseCSVString fieldSeparator = textField fieldSeparator

-- | Skip a CSV field.
skipCSVField :: Char -> Parser ()
skipCSVField fieldSeparator
  = skipQuotedField <|> skipUnquotedField
 where

  skipUnquotedField
    = skipWhile (\c -> c /= fieldSeparator && c /= '\r' && c /= '\n')

  skipInsideQuotes =
    skipMany (do skipWhile (/= '"')
                 skipMany1 dquotes)

  dquotes = string "\"\"" >> return ()

  skipQuotedField =
   char '"' *> skipInsideQuotes <* char '"'


-- | Parse a field in CSV format, that is a string, and that is not likely to be repeated.
--
parseCSVMaybeString :: Char -> Parser (Maybe T.Text)
parseCSVMaybeString fieldSeparator
  = do t <- textField fieldSeparator
       case t == "\\N" of
         True -> return Nothing
         False -> return $ Just t

-- | Parse a Field in CSV format, using the string dictionary approach.
parseCSVMaybeSharedString :: Char -> StringDictionary -> Parser (Maybe T.Text, StringDictionary)
parseCSVMaybeSharedString fieldSeparator dict1
  = do text1 <- textField fieldSeparator <?> "field"
       case text1 == "\\N" of
         True
           -> return (Nothing, dict1)
         False
           -> case Map.lookup text1 dict1 of
                Just text2
                  -> return (Just text2, dict1)
                Nothing
                  ->  let dict2 = Map.insert text1 text1 dict1
                      in return (Just text1, dict2)

-- | Parse a Field in CSV format, using the string dictionary approach.
--
parseCSVSharedString :: Char -> StringDictionary -> Parser (T.Text, StringDictionary)
parseCSVSharedString fieldSeparator dict1
  = do text1 <- textField fieldSeparator <?> "field"
       case Map.lookup text1 dict1 of
                Just text2
                  -> return (text2, dict1)
                Nothing
                  ->  let dict2 = Map.insert text1 text1 dict1
                      in return (text1, dict2)


parse01AsBool :: Parser Bool
parse01AsBool = maybeQuoted $
    (do char '0'
        return False
    ) <|> (do char '1'
              return True)

parseMaybe01AsBool :: Parser (Maybe Bool)
parseMaybe01AsBool = maybeQuoted $
    (do char '0'
        return $ Just False
    ) <|> (do char '1'
              return $ Just True
    ) <|> (do (string "\\N")
              return $ Nothing
    )

attoParsecUnsignedInt :: P.Parser Int
attoParsecUnsignedInt
  = do dd <- P.takeWhile isDigit
       case fromTextToInt dd of
         Nothing
           -> fail $ T.unpack dd ++ " is not an integer"
         Just i
           -> return i

parseInt :: Parser Int
parseInt = maybeQuoted decimal

parseMaybeInt :: Parser (Maybe Int)
parseMaybeInt
  = (do string "\\N"
        return Nothing
    ) <|> (Just <$> parseInt)

fromTextToInt :: T.Text -> Maybe Int
fromTextToInt t
  = case (T.signed T.decimal t) of
      Right (r, "") -> Just r
      _ -> Nothing

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

-- | Parse a DateTime in MySQL format.
--
parseMySQLDateTimeToLocalTime :: Char -> Parser LocalTime
parseMySQLDateTimeToLocalTime fieldSeparator
  = do str <- textField fieldSeparator
       time :: LocalTime <- case fromMySQLDateTimeAsTextToLocalTime str of
         Just t -> return t
         Nothing -> fail $ "\"" ++ show str ++ "\" is not a valid MySQL DateTime"
       return time

-- | Convert from a String in MySQL date-time format to an internal LocalTime.
--   Warning: this function is very slow, use instead `fromMySQLTimeToLocalTime`.
fromMySQLDateTimeToLocalTime :: String -> Maybe LocalTime
fromMySQLDateTimeToLocalTime str
  = fromMySQLDateTimeAsTextToLocalTime (T.pack str)
{-# INLINE fromMySQLDateTimeAsTextToLocalTime #-}

fromGammaCallDateAndTimeStampToLocalTime :: T.Text -> T.Text -> Maybe LocalTime
fromGammaCallDateAndTimeStampToLocalTime ddmmyyyy hhmmss
  = case P.parseOnly parseDDMMYYYY ddmmyyyy of
      Right (year1, month1, day1)
        -> case P.parseOnly parseHHMMSS hhmmss of
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
    = do dd <- attoParsecUnsignedInt
         P.anyChar
         mm <- attoParsecUnsignedInt
         P.anyChar
         yyyy <- attoParsecUnsignedInt
         return (yyyy, mm, dd)

  parseHHMMSS
    = do hh <- attoParsecUnsignedInt
         anyChar
         mm <- attoParsecUnsignedInt
         anyChar
         ss <- attoParsecUnsignedInt
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
                -- ^ error
           7 -> p
                -- ^ not significant second digits
           _ -> case mustBeDigit of
                  False
                    -> case isDigit c of
                         True
                           -> (0, mult, mustBeDigit, currNum, year, month, day, hour, minutes, seconds)
                              -- ^ error: a digit is found, when no digit was expected
                         False
                           -> (state + 1, 10, True, 0, year, month, day, hour, minutes, seconds)
                              -- ^ go to next state, at for sure it is a two digit state (mult = 10)
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
  = case P.parseOnly parseMonthAndYear monthAndYear of
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
    = do monthS <- P.takeWhile (\w -> not $ isSpace w)
         P.space
         yyyy <- attoParsecUnsignedInt
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

-- | A format like this
--   > 2014/11/05 08.28.54
fromDateFormat1ToLocalTime :: T.Text -> Maybe LocalTime
fromDateFormat1ToLocalTime = fromMySQLDateTimeAsTextToLocalTime
{-# INLINE fromDateFormat1ToLocalTime #-}

-- | A italian format like this
 --   > 25/12/2016 08.28.54
fromDateFormat2ToLocalTime :: Char -> Char -> Char -> T.Text -> Maybe LocalTime
fromDateFormat2ToLocalTime dateSep dateAndTimeSep timeSep dateAndTime
  = case P.parseOnly parseDateAndTime dateAndTime of
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
    = do dd <- attoParsecUnsignedInt
         P.char dateSep
         mm <- attoParsecUnsignedInt
         P.char dateSep
         yyyy <- attoParsecUnsignedInt
         P.char dateAndTimeSep
         hhh <- attoParsecUnsignedInt
         P.char timeSep
         mmm <- attoParsecUnsignedInt
         P.char timeSep
         sss <- attoParsecUnsignedInt
         return (yyyy, mm, dd, hhh, mmm, sss)

-- | Parse a DateTime in MySQL format.
--
parseMySQLDateTimeToMaybeLocalTime :: Char -> Parser (Maybe LocalTime)
parseMySQLDateTimeToMaybeLocalTime fieldSeparator
  = (do string "\\N"
        return Nothing
    ) <|> (Just <$> parseMySQLDateTimeToLocalTime fieldSeparator)

unquotedField :: Char -> Parser T.Text
unquotedField fieldSeparator
  = takeTill (\c -> c == fieldSeparator || c == '\r' || c == '\n')

insideQuotes :: Parser T.Text
insideQuotes =
   T.append <$> takeWhile (/= '"')
            <*> (T.concat <$> many' (T.cons <$> dquotes <*> insideQuotes))
   where

      dquotes =
         string "\"\"" >> return '"'

quotedField :: Parser T.Text
quotedField =
   char '"' *> insideQuotes <* char '"'

textField :: Char -> Parser T.Text
textField fieldSeparator =
   quotedField <|> unquotedField fieldSeparator

showLocalTime :: LocalTime -> String
showLocalTime = fromLocalTimeToMySQLDateTime

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

showLocalTimeUsingMySQLFormat = showLocalTime

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
replacePrefixAndGetPortedTelephoneNumber :: String -> String -> String -> Maybe String
replacePrefixAndGetPortedTelephoneNumber sourceNumber sourcePrefix destPrefix
  = let l = length sourcePrefix
        sl = length sourceNumber

    in case l > sl of
         True
           -> Nothing
         False
           -> Just $ destPrefix ++ (drop l sourceNumber)

-- | Convert a string, into a string that can be parsed from MySQL CSV LOAD DATA instruction.
--   TODO probably this should be converted to Builder form: a lot faster
toMySQLCSVString :: T.Text -> T.Text
toMySQLCSVString source
  = T.snoc (T.foldl' f (T.pack "\"") source) '"'
 where

   f dest c
     = case c of
         '"' -> let dest1 = T.snoc dest '\\'
                    dest2 = T.snoc dest1 '"'
                in dest2
         '\\' -> let dest1 = T.snoc dest '\\'
                     dest2 = T.snoc dest1 '\\'
                in dest2
         '\n' -> let dest1 = T.snoc dest '\\'
                     dest2 = T.snoc dest1 'n'
                 in dest2
         '\r' -> let dest1 = T.snoc dest '\\'
                     dest2 = T.snoc dest1 'r'
                 in dest2
         _ -> T.snoc dest c

-- ----------------------------------
-- Channels
--

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


--
-- Unit Tests Utils
--

isDescendingOrder :: (Ord a) => [a] -> Bool
isDescendingOrder [] = True
isDescendingOrder [_] = True
isDescendingOrder (a:b:r) = if a >= b then isDescendingOrder (b:r) else False

isAscendingOrder :: (Ord a) => [a] -> Bool
isAscendingOrder [] = True
isAscendingOrder [a] = True
isAscendingOrder l = not $ isDescendingOrder l

fromMySQLResultToText :: Maybe BS.ByteString -> T.Text
fromMySQLResultToText bs = decodeUtf8 $ fromJust1 "re-error" bs
{-# INLINE fromMySQLResultToText #-}

fromTextToMySQLResult :: T.Text -> BS.ByteString
fromTextToMySQLResult t = encodeUtf8 t
{-# INLINE fromTextToMySQLResult #-}

fromLazyToStrictByteString :: BSL.ByteString -> BS.ByteString
fromLazyToStrictByteString lb = BSL.toStrict lb
{-# INLINE fromLazyToStrictByteString #-}

-- ------------------------------
-- TESTS
--

tt_parseTests
  = [ testAttoParsec "1" "one" (string "one") (T.pack "one")
    , testAttoParsec "2" "1" (parse01AsBool) (True)
    , testAttoParsec "3" "0" (parse01AsBool) (False)
    , testAttoParsec "4" "1" (parseMaybe01AsBool) (Just True)
    , testAttoParsec "5a" "0" (parseMaybe01AsBool) (Just False)
    , testAttoParsec "5b" "\"1\"" (parseMaybe01AsBool) (Just True)
    , testAttoParsec "6" "\"0\"" (parseMaybe01AsBool) (Just False)
    , testAttoParsec "7" "\\N" (parseMaybe01AsBool) (Nothing)
    , testAttoParsec "8a" "hello" (parseCSVMaybeString ',') (Just $ T.pack "hello")
    , testAttoParsec "8b" "hello\"\"here" (parseCSVMaybeString ',') (Just $ T.pack "hello\"\"here")
    , testAttoParsec "8c" "hello\"here" (parseCSVMaybeString ',') (Just $ T.pack "hello\"here")
    , testAttoParsec "9" "\"hello\"" (parseCSVMaybeString ',') (Just $ T.pack "hello")
    , testAttoParsec "10" "\\N" (parseCSVMaybeString ',') (Nothing)
    , testAttoParsec "11" "1" (parseInt) (1)
    , testAttoParsec "12" "\"1\"" (parseInt) (1)
    , HUnit.TestCase $ HUnit.assertEqual "13" (Just "123456") (replacePrefixAndGetPortedTelephoneNumber "000456" "000" "123")
    , testLocalTimeParsing "14" "2015-01-02 03:04:05"
    , testLocalTimeParsing "15" "2015-11-13 10:12:54"
    , testLocalTimeParsing "15" "2015-2537y-13 10:12:54"
    ]

 where

  parseMaybeTime :: TimeLocale -> String -> String -> Maybe LocalTime
  parseMaybeTime tl formatStr inStr
    = parseTimeM False tl formatStr inStr

  testLocalTimeParsing n t1
    = HUnit.TestCase $ HUnit.assertEqual n  (parseMaybeTime defaultTimeLocale "%F %T" (T.unpack t1)) (fromMySQLDateTimeAsTextToLocalTime t1)

  testAttoParsec n s p v
    = let completeParser = do
            r <- p
            endOfInput
            return r

          parserResult
            = parseOnly completeParser s

      in HUnit.TestCase $ HUnit.assertEqual n parserResult (Right v)
