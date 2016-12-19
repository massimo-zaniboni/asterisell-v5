{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes #-}

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


-- | Import from the PHP world, the list of CDR files to rate.
--
module Asterisell.CdrsToRate (
  SourceCDRsFileToRate(..)
  , sourceCDRsFileToRate_read
) where


import Asterisell.Error
import Asterisell.Cdr
import Asterisell.Utils
import Asterisell.OrganizationHierarchy
import Asterisell.VoIPChannelAndVendor
import Asterisell.TelephonePrefixes
import Asterisell.Trie
import Asterisell.RateCategories

import Numeric

import Data.List as List
import Data.Char
import Data.Either
import Control.Monad.State.Strict
import Data.STRef
import Control.Monad.ST
import Control.Applicative ((<$>), (<*>), pure)
import Data.Ratio
import Data.Ord as Ord
import Data.STRef.Strict

import Data.Default
import qualified Data.Map as Map
import Data.Vector as V (length)

import qualified Data.ByteString.Lazy as BS
import Data.Text.Encoding

import Control.Monad as M
import Control.Monad.IO.Class       (liftIO)
import Data.Word
import System.FilePath.Posix
import Data.Time.LocalTime
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char (ord)

import qualified Test.HUnit as HUnit

import Pipes
import Pipes.ByteString as PB
import qualified Pipes.Prelude as PP
import Pipes.Safe as PS
import qualified Pipes.Safe.Prelude  as PS
import qualified Pipes.Csv as PC
import Pipes.Csv ((.!))

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Prim as Parsec
import Text.Parsec.Prim((<|>), (<?>))
import Text.Parsec.ByteString.Lazy as Parsec
import qualified Text.Parsec.Char as Parsec

import System.IO
-- import Debug.Trace
-- import Data.Maybe
-- import Data.Set as Set

import Data.Hashable

import Text.Heredoc

-----------------------------------
-- EXTRACT LIST OF FILES TO RATE --
-----------------------------------

-- | Records in a list of filet to rate.
--
data SourceCDRsFileToRate
  = SourceCDRsFileToRate {
      loftr_fileName :: !Text.Text
    , loftr_fileType :: !Text.Text
    , loftr_fileFormat :: !Text.Text
    , loftr_cdrProvider :: !Text.Text
    , loftr_minCallDate :: !Text.Text
 } deriving(Show)

instance PC.FromRecord SourceCDRsFileToRate where
     parseRecord v
         | V.length v == 5
              = SourceCDRsFileToRate <$>
                v .! 0 <*>
                v .! 1 <*>
                v .! 2 <*>
                v .! 3 <*>
                v .! 4
         | otherwise = mzero

sourceCDRsFileToRate_read :: FilePath -> (PS.SafeT IO) [SourceCDRsFileToRate]
sourceCDRsFileToRate_read fileName
  = PS.withFile fileName ReadMode (\handle -> do
            
      let sourceFile = PB.fromHandle handle

      let sourceRecords :: Producer (Either String SourceCDRsFileToRate) (PS.SafeT IO) ()  
          sourceRecords = PC.decodeWith PC.defaultDecodeOptions PC.NoHeader sourceFile

      let fun _ (Left err) = error $ "Error in file \"" ++ fileName ++ "\", containing the list of files to rate. " ++ err
      let fun r (Right x) = r ++ [x]

      PP.fold fun [] id sourceRecords
                                  )      
