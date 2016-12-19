{-# Language OverloadedStrings, ScopedTypeVariables #-}

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


-- | Import info about Rate Categories
--
module Asterisell.RateCategories(
    RateCategoryId
  , RateCategoryCode
  , RateCategories
  , rateCategories_load
  , rateCategories_code
  , rateCategories_empty
  , rateCategories_create
) where

import Asterisell.Trie
import Asterisell.Utils

import System.IO
import Control.Monad (void)
import Control.Applicative ((<$>), (<*>), pure)

import qualified Data.Text as Text
import Data.Map.Strict as Map
import Data.Maybe
import Data.List as List
import Data.Time.LocalTime

import Data.Vector as V (length)

import Pipes
import Pipes.ByteString as PB
import qualified Pipes.Prelude as PP
import Pipes.Safe as PS
import qualified Pipes.Safe.Prelude  as PS
import qualified Pipes.Csv as PC
import Pipes.Csv ((.!))

type RateCategoryId = Int

type RateCategoryCode = String

type RateCategories 
  = (Map.Map RateCategoryCode RateCategoryId
    ,Map.Map RateCategoryId RateCategoryCode) 

rateCategories_empty :: RateCategories
rateCategories_empty = (Map.empty, Map.empty)

rateCategories_create :: Map.Map RateCategoryCode RateCategoryId -> RateCategories
rateCategories_create map1
  = let map2 = Map.fromList $ List.map (\(i,j) -> (j,i)) $ Map.toList map1
    in (map1, map2)

rateCategories_code :: RateCategories -> RateCategoryId -> RateCategoryCode
rateCategories_code (_, map2) id
  = fromJust $ Map.lookup id map2

data CSVFormat_RateCategories 
  = CSVFormat_RateCategories {
       rc1_id :: !RateCategoryId
     , rc1_name :: !Text.Text
     , rc1_internalName :: !Text.Text
    } deriving (Show)

instance PC.FromRecord CSVFormat_RateCategories where
     parseRecord v
       = case V.length v == 3 of
           True 
             -> CSVFormat_RateCategories <$>
                  v .! 0 <*>
                  v .! 1 <*>
                  v .! 2
           False
             -> fail $ "There are " ++ show (V.length v) ++ " fields, instead of expected number of fields."

rateCategories_load 
  :: Bool
  -> FilePath 
  -> (PS.SafeT IO) (Either String RateCategories)

rateCategories_load isDebugMode fileName
  = do PS.withFile fileName ReadMode (\handle -> do
         
         -- this is a pipe that convert a file to a stream of bytes
         let sourceFile = PB.fromHandle handle

         -- this is a pipe that convert the stream of bytes to a stream of records.
         let sourceRecords :: Producer (Either String CSVFormat_RateCategories) (PS.SafeT IO) ()
             sourceRecords = PC.decodeWith csvOptions PC.NoHeader sourceFile
    
         PP.fold importRecords (Right Map.empty, 1) makeResult sourceRecords 
                                     )
  
 where

   csvOptions = PC.defaultDecodeOptions

   importRecords (Left err, i) _ 
     = (Left err, i)

   importRecords (Right map1, i) maybeRecord 
     = case maybeRecord of 
         Left err 
           -> (Left $ "Error during parsing of file " ++ fileName ++ ", at line " ++ show i ++ ": " ++ err, i + 1)
         Right record
           -> (Right $ Map.insert (Text.unpack $ rc1_internalName record) (rc1_id record) map1, i + 1)

   makeResult (Left err, _) 
     = Left err

   makeResult (Right map1, _)
     = Right $ rateCategories_create map1
