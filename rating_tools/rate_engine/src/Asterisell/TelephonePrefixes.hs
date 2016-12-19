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


-- | Import Telephone Prefixes tables from the PHP world.
--
--   This code must be keept in synchro with
--   `apps/asterisell/lib/jobs/data_file_processing/ExportConfigurationsToExternalRateEngine.php`.
--
--   A telephone prefix is only used for describing a call in the call report.
--
module Asterisell.TelephonePrefixes (
  parseFileWithTelephonePrefixes,
  TelephonePrefixes,
  TelephonePrefixValue(
      telephonePrefix_id,
      telephonePrefix_geographicLocation, 
      telephonePrefix_operatorType
  )
) where

import Asterisell.Trie
import Asterisell.Utils

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as IO

import qualified System.IO as SIO
import Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List as L

-- | The value associated to a telephone prefix.
--
data TelephonePrefixValue
  = TelephonePrefixValue {
      telephonePrefix_id :: Int,
      telephonePrefix_geographicLocation :: T.Text,
      telephonePrefix_operatorType :: T.Text
    }
 deriving(Eq, Ord, Show)

type TelephonePrefixes = Trie TelephonePrefixValue

type ParserState = (TelephonePrefixes, StringDictionary, ConfigurationErrors, LineNumber)

-- | Parse a file using Attoparsec, and chunk by chunk.
--   So it can be used for big files, without using too much RAM.
parseFileWithTelephonePrefixes :: Bool -> String -> IO (Either (IsApplicationError, String) TelephonePrefixes)
parseFileWithTelephonePrefixes isDebugMode fileName
  = SIO.withFile fileName SIO.ReadMode $ \handle -> do
       SIO.hSetEncoding handle SIO.utf8_bom
       fileContent <- IO.hGetContents handle 
       let !r = parse (telephonePrefixes parserState_empty <* endOfInput) fileContent
       case eitherResult r of
         Left msg
           -> return $ Left (True, msg)
         Right (info, _, errors, _)
           -> case Set.null errors of
                True
                  -> return $ Right info
                False
                  -> return $ Left $ (False
                                     , "There are telephone prefixes with problems. " ++ (L.concatMap (\t -> t ++ "\n") (Set.toList errors))
                                     )

parserState_empty :: ParserState
parserState_empty = (trie_empty, Map.empty, Set.empty, 1)

addTelephonePrefixRecord :: ParserState -> Parser ParserState
addTelephonePrefixRecord state1@(trie1, dict1, errors1, ln)
  = (do id <- parseInt
        char ','
        prefix <- parseCSVString ','
        char ','
        (geographicLocation, dict2) <- parseCSVSharedString ',' dict1
        char ','
        (operatorType, dict3) <- parseCSVSharedString ',' dict2
        (endOfLine <|> endOfInput)
        let value = TelephonePrefixValue {
                         telephonePrefix_id = id,
                         telephonePrefix_geographicLocation = geographicLocation,
                         telephonePrefix_operatorType = operatorType
                    }

        let (trie2, errors2)
              = case trie_addUniqueExtensionCode trie1 (T.unpack prefix) value of
                   Nothing
                     -> (trie1, Set.insert ("The telephone prefix \"" ++ (T.unpack prefix) ++ "\", is in conflict with another prefix.") errors1) 
                   Just r
                     -> let code = T.unpack prefix
                            codeM = extensionCode_toCharsMatch code
                        in case charsMatch_valid codeM of
                             Just err
                               -> (trie1, Set.insert ("Telephone prefix \"" ++ code ++ "\" has an invalid format: " ++ err) errors1)
                             Nothing
                               -> (r, errors1)
                                                          
        return (trie2, dict3, errors2, ln + 1)) <?> ("Error at line: " ++ show ln)

-- | Parse zero or many telephone prefixes,
--   returning the final state.
--
telephonePrefixes :: ParserState-> Parser ParserState
telephonePrefixes state1
  = (do state2 <- addTelephonePrefixRecord state1
        telephonePrefixes state2) <|> (return state1)

