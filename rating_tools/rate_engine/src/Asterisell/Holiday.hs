{-# Language BangPatterns, OverloadedStrings, ScopedTypeVariables  #-}
{-# LANGUAGE QuasiQuotes, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Import info about holidays, and manage peak and off-peak.
--
module Asterisell.Holiday(
  Holiday(..)
  , Holidays
  , PeakCode
  , PeakCodes
  , holidays_load
  , holiday_match
  , holidays_match
  , holidays_peakCodes
) where


import Asterisell.Error
import Asterisell.DB
import Asterisell.Trie
import Asterisell.Utils

import System.IO
import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), pure)

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List as L
import Data.Time.LocalTime
import Data.Vector as V (length)

import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar

import GHC.Generics
import Control.DeepSeq

import Database.MySQL.Base as DB
import qualified Database.MySQL.Protocol.Escape as DB
import Database.MySQL.Protocol.MySQLValue
import qualified System.IO.Streams as S
import qualified System.IO.Streams.List as S
import qualified System.IO.Streams.Text as S
import qualified System.IO.Streams.Combinators as S
import Text.Heredoc
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException, throwIO, throw, Exception, MonadMask
                              , withException, displayException)

type PeakCode = Text.Text

type PeakCodes = Set.Set PeakCode

data Holiday
       = Holiday {
             h_dayOfMonth :: Maybe Int
             -- ^ Nothing for each day of month
           , h_month :: Maybe Int
             -- ^ Nothing for each month
           , h_year :: Maybe Int
             -- ^ Nothing for each year
           , h_dayOfWeek :: Maybe Int
             -- ^ Nothing for each day of week,
             --   1 Monday, 2 Tuesday, ..., 7 Sunday
           , h_fromHour :: Maybe Int
             -- ^ 0 .. 23
           , h_fromMinutes :: Maybe Int
             -- ^ 0 .. 59
           , h_toHour :: Maybe Int
           , h_toMinutes :: Maybe Int
           , h_peakCode :: PeakCode
                 }
         deriving(Eq, Ord, Show, Generic, NFData)

type Holidays = [Holiday]

holiday_match :: Holiday -> LocalTime -> Bool
holiday_match h lt
  = let (year, month, day) = toGregorian (localDay lt)
        (_, _, dayOfWeek) = toWeekDate (localDay lt)


        isMatch :: Maybe Int -> Int -> Bool
        isMatch Nothing _ = True
        isMatch (Just x) y = x == y

        m f x = isMatch (f h) x

        fromHour = case h_fromHour h of
                     Nothing -> 0
                     Just v -> v

        fromMinutes = case h_fromMinutes h of
                     Nothing -> 0
                     Just v -> v
        toHour = case h_toHour h of
                     Nothing -> 23
                     Just v -> v

        toMinutes = case h_toMinutes h of
                     Nothing -> 59
                     Just v -> v

        t1 = TimeOfDay fromHour fromMinutes 0
        t2 = TimeOfDay toHour toMinutes 60
        t = localTimeOfDay lt

    in m h_dayOfMonth day
         && m h_month month
         && m h_year (fromInteger year)
         && m h_dayOfWeek dayOfWeek
         && t >= t1
         && t <= t2

-- | The PeakCode matched from the local time.
holidays_match :: Holidays -> LocalTime -> PeakCodes
holidays_match hs lt
  = Set.fromList $ L.map h_peakCode $ L.filter (\h -> holiday_match h lt) hs

-- | Extract the few peak codes, from the few holidays data.
holidays_peakCodes :: Holidays -> PeakCodes
holidays_peakCodes hs = Set.fromList $ L.map h_peakCode hs 

holidays_load
  :: DB.MySQLConn
  -> Bool
  -> IO Holidays

holidays_load conn isDebugMode = do
  let q1 = [str| SELECT
               |   day_of_month
               | , month
               | , year
               | , day_of_week
               | , from_hour
               | , from_minutes
               | , to_hour
               | , to_minutes
               | , peak_code
               | FROM ar_holiday
               |]

  (_, inS) <- DB.query_ conn q1
  inS' <- S.map importHoliday inS
  S.toList inS'

 where

   importHoliday
     [ dayOfMonth
     , month'
     , year'
     , dayOfWeek
     , from_hour
     , from_minutes
     , to_hour
     , to_minutes
     , peakCode
     ] = Holiday {
           h_dayOfMonth = fromMaybeDBValue fromDBInt dayOfMonth
           , h_month = fromMaybeDBValue fromDBInt month'
           , h_year = fromMaybeDBValue fromDBInt year'
           , h_dayOfWeek = fromMaybeDBValue fromDBInt dayOfWeek
           , h_fromHour = fromMaybeDBValue fromDBInt from_hour
           , h_fromMinutes = fromMaybeDBValue fromDBInt from_minutes
           , h_toHour = fromMaybeDBValue fromDBInt to_hour
           , h_toMinutes = fromMaybeDBValue fromDBInt to_minutes
           , h_peakCode = fromDBText peakCode
           }
