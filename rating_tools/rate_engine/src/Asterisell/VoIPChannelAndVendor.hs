{-# Language BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

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


-- | Import info about vendors, and channels.
--
module Asterisell.VoIPChannelAndVendor(
  VendorId,
  Vendors,
  VendorCode,
  ChannelTypeId,
  ChannelTypeCode,
  ChannelTypes,
  ChannelDomains,
  IsSuffixChannel, 
  IsPrefixChannel,
  FromTime, 
  ToTime, 
  channelDomains_match,
  parseFileWithVendors,
  parseFileWithChannelDomains,
  parseFileWithChannelTypes
) where

import Asterisell.Trie
import Asterisell.Utils

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as IO
import Data.Map.Strict as Map
import Data.Maybe
import Data.List as List
import Data.Time.LocalTime
import qualified System.IO as SIO

type VendorId = Int
type VendorCode = String

type Vendors = Map.Map VendorCode VendorId

type ChannelTypeId = Int
type ChannelTypeCode = String

type ChannelTypes = Map.Map ChannelTypeCode ChannelTypeId

type IsSuffixChannel = Bool
type IsPrefixChannel = Bool
type FromTime = LocalTime
type ToTime = Maybe LocalTime

type ChannelDomainEntry 
  = (VendorId, ChannelTypeId, IsSuffixChannel, IsPrefixChannel, FromTime, ToTime)

-- | Map a channel domain name, to a list of entries.
--   The suffixes are stored as keys in reverse order.
type ChannelDomains = Trie [ChannelDomainEntry]

parseFileWithInfo :: Bool -> String -> ((s,a) -> Parser (s,a)) -> (s,a) -> IO (Either String s)
parseFileWithInfo isDebugMode fileName p s
  = SIO.withFile fileName SIO.ReadMode $ \handle -> do
       SIO.hSetEncoding handle SIO.utf8_bom
       fileContent <- IO.hGetContents handle   

       let !r = parse (parseMany p s <* endOfInput) fileContent
       case r of
         Fail rst _ msg -> return $ Left ("Unprocessed data is: " ++ (TL.unpack rst) ++ "\nError message: " ++ msg)
         Done rst rs
           -> return $ Right $ fst rs

parseFileWithVendors isDebugMode fileName = parseFileWithInfo isDebugMode fileName addVendorRecord (Map.empty, 1)

parseFileWithChannelTypes = parseFileWithVendors

parseFileWithChannelDomains isDebugMode fileName = parseFileWithInfo isDebugMode fileName addChannelDomainRecord (trie_empty, 1)

parseMany :: (s -> Parser s) -> s -> Parser s
parseMany p state1 
  = (do state2 <- p state1
        parseMany p state2) <|> (return state1)

addVendorRecord :: (Vendors, Int) -> Parser (Vendors, Int)
addVendorRecord (vendors1, ln)
  = (do id <- parseInt
        char ','
        maybeInternalName <- parseCSVMaybeString ','
        (endOfLine <|> endOfInput)
        
        let vendors2 
              = case maybeInternalName of
                  Nothing
                    -> vendors1
                       -- do not store vendors without a symbolic name,
                       -- because they are not referenced in rates
                  Just n
                    -> Map.insert (T.unpack n) id vendors1
        
        return (vendors2, ln + 1)) <?> ("Error at line: " ++ show ln)

addChannelDomainRecord :: (ChannelDomains, LineNumber) -> Parser (ChannelDomains, LineNumber)
addChannelDomainRecord (channels1, ln)
  = (do id <- parseInt
        char ','
        internalName <- parseCSVMaybeString ','
        char ','
        vendorId <- parseInt
        char ','
        channelTypeId <- parseInt
        char ','
        domainName <- parseCSVString ','
        char ','
        isPrefix <- parse01AsBool
        char ','
        isSuffix <- parse01AsBool
        char ','
        fromTimeValue <- parseMySQLDateTimeToLocalTime ','
        char ','
        toTimeValue <- parseMySQLDateTimeToMaybeLocalTime ','
        (endOfLine <|> endOfInput)

        let domainNameS1 = T.unpack domainName

        let domainNameS2 = case isPrefix of
                             True -> domainNameS1 ++ "*"
                             False -> domainNameS1

        case isSuffix of
          False -> return ()
          True ->  fail "Suffix Vendor Domains are not any more supported."

        let value = (vendorId, channelTypeId, isSuffix, isPrefix, fromTimeValue, toTimeValue)
        
        let channels2
              = trie_addExtensionCodeWith channels1 domainNameS2 [value] (++)

        return (channels2, ln + 1)) <?> ("Error at line: " ++ show ln)
  
  
channelDomains_match :: ChannelDomains -> T.Text -> LocalTime -> [(VendorId, ChannelTypeId)]
channelDomains_match trie domain date 
  = candidateResults
 where
 
  domain1 = T.unpack domain
   
  mySearch d
    = case trie_getMatch trie_getMatch_initial trie d of
        Nothing -> []
        Just (_, r) -> r  

  insideDate (_, _, _, _, fromTime, maybeToTime)
    = (date >= fromTime) && (isNothing maybeToTime || date < (fromJust maybeToTime))
   
  toResult (vendorId, channelTypeId, _, _, _, _)
    = (vendorId, channelTypeId)
   
  candidateResults
    =  List.map toResult $ List.filter insideDate (mySearch domain1)

