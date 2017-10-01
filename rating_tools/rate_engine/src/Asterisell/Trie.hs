{-# Language OverloadedStrings, ScopedTypeVariables, BangPatterns, DeriveGeneric, DeriveAnyClass #-}

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


module Asterisell.Trie (
  CharMatchStrenght,
  ExtensionCode,
  IsExtensionalMatch,
  ExtensionCodeAsText,
  Trie,
  trie_toKeyValueList,
  trie_getMatch,
  trie_getMatch_initial,
  trie_empty,
  trie_add,
  trie_addWith,
  trie_addExtensionCode,
  trie_addExtensionCodeWith,
  trie_addExtensionCodes,
  trie_addExtensionCodesAsText,
  trie_addExtensionCodeAsText,
  trie_addExtensionCodeAsTextWith,
  tt_trie_test,
  trie_size,
  trie_lookup,
  trie_addUniqueExtensionCode,
  charsMatch_valid,
  extensionCode_toCharsMatch
) where

import Data.List as L
import Data.Char
import Data.Either
import Control.Monad.State.Strict
import Data.Ratio

import Data.Default
import qualified Data.Map.Strict as Map
import qualified Test.HUnit as HUnit
import qualified Data.Text as Text
import GHC.Generics
import Control.DeepSeq

-- | A character that can be matched.
--
data CharMatch
  = CharMatch_Char Char
  | CharMatch_AnyChar
  -- ^ the "X" symbol matching exactly one (any) char
  | CharMatch_AllChars
  -- ^ match zero or more chars.
  -- NOTE: strings without "*" are an exact match, and not a prefix match.
 deriving(Eq, Ord, Generic, NFData)

instance Show CharMatch where
  show (CharMatch_Char c) 
    = case c of
        '?' -> "\\?"
        '*' -> "\\*"
        ',' -> "\\,"
        '\\' -> "\\"
        _ -> show c

  show (CharMatch_AnyChar) = "?"
  show (CharMatch_AllChars) = "*"

type CharsMatch = [CharMatch]

-- | Maps strings to values.
--   The value is returned only if the search string match
--   exactly the trie prefix (takin in account "X" and "X*" special match),
--   and if there is a value associated to the value.
--
data Trie a
  = Trie (Maybe a) (Map.Map CharMatch (Trie a))
 deriving (Eq, Generic, Generic1)

instance (NFData a) => NFData (Trie a)

instance (Show a) => Show (Trie a) where

  show (Trie ma mt)
    = show ma ++ "(" ++ show mt ++ ")"

trie_size :: Trie a -> Int
trie_size (Trie _ map1)
  = 1 + Map.foldl' (\r t -> r + trie_size t) 0 map1

-- | Expand a Trie to a list of key and values.
--
trie_toKeyValueList :: Trie a -> [(CharsMatch, a)]
trie_toKeyValueList trie
  = toList [] trie
 where

  toList keyPrefix (Trie maybeValue nextKeys)
    = let thisResult
            = case maybeValue of
                Just value
                  -> [(keyPrefix, value)]
                Nothing
                  -> []

          explore currentResults nextKey nextTrie
            = currentResults ++ (toList (keyPrefix ++ [nextKey]) nextTrie)

      in thisResult ++ Map.foldlWithKey' explore [] nextKeys

trie_empty :: Trie a
trie_empty = Trie Nothing Map.empty

-- | The main function, inserting the values inside the trie, in the proper format for the search.
trie_addWith :: Trie a -> CharsMatch -> a -> (a -> a -> a) -> Trie a

trie_addWith (Trie Nothing nextTrie) [] value withFun 
  = Trie (Just value) nextTrie

trie_addWith (Trie (Just value0) nextTrie) [] value withFun 
  = Trie (Just $ withFun value value0) nextTrie

trie_addWith (Trie maybeOldValue tries) (c:r) value withFun
  = let nextTrie
          = case Map.lookup c tries of
              Nothing
                -> trie_empty
              Just nextTrieOnC
                -> nextTrieOnC
    in (Trie maybeOldValue (Map.insert c (trie_addWith nextTrie r value withFun) tries))

trie_add :: Trie a -> CharsMatch -> a -> Trie a
trie_add trie match value 
  = let  myReplace v1 v0 = v1 
    in   trie_addWith trie match value myReplace

-- | An extension code where:
--     * "X" stays for the any-char,
--     * "X*" stays for one or more chars,
--     * "*" stays for zero or more chars,
--     * "\X" stays for "X"
--     * "\*" stays for "*"
--     * "1X3" is valid
--     * "1X*3" is not valid.
type ExtensionCode = String

type ExtensionCodeAsText = Text.Text

extensionCode_toCharsMatch :: ExtensionCode -> CharsMatch

extensionCode_toCharsMatch [] 
  = []

extensionCode_toCharsMatch ('\\':c:r) 
  = (CharMatch_Char c):(extensionCode_toCharsMatch r)

extensionCode_toCharsMatch (c:r)
  = let cm = case c of
               'X' -> CharMatch_AnyChar
               '*' -> CharMatch_AllChars
               _ -> CharMatch_Char c
    in cm:(extensionCode_toCharsMatch r)

-- | Nothing if the CharsMatch  is valid, the reason of the problem otherwise.
charsMatch_valid :: CharsMatch -> Maybe String
charsMatch_valid chars
  = let (_, l) = L.break (\c -> (c == CharMatch_AllChars)) chars
    in if L.null l
       then Nothing
       else if (L.null $ L.tail l)
            then Nothing
            else Just $ "After \"*\" there can not be other chars."     

-- | Exact lookup of the associated value.
trie_lookup :: Trie a -> ExtensionCode -> Maybe a
trie_lookup trie code
  = f trie (extensionCode_toCharsMatch code)
 where

  f (Trie Nothing _) [] = Nothing
  f (Trie (Just r) _) [] = Just r
  f (Trie _ (map1)) (c:rest)
    = case Map.lookup c map1 of
        Nothing
          -> Nothing
        Just trie2
          -> f trie2 rest

trie_addExtensionCode :: Trie a -> ExtensionCode -> a -> Trie a
trie_addExtensionCode trie key value
  = trie_add trie (extensionCode_toCharsMatch key) value

trie_addUniqueExtensionCode
  :: Trie a
  -> ExtensionCode
  -- ^ the extension code to add
  -> a
  -- ^ the associated value
  -> Maybe (Trie a)
  -- ^ Nothing if the ExtensionCode is in conflict with an already defined ExtensionCode.
  --   Consider as conflict also "3" and "3*"
 
trie_addUniqueExtensionCode trie key value
  = case trie_lookup trie key of
      Just _
        -> Nothing
      Nothing
        -> case trie_lookup trie (key ++ "*") of
             Just _
               -> Nothing
             Nothing
               -> Just $ trie_addExtensionCode trie key value
                  
trie_addExtensionCodeWith :: Trie a -> ExtensionCode -> a -> (a -> a -> a) -> Trie a
trie_addExtensionCodeWith trie key value withFun
  = trie_addWith trie (extensionCode_toCharsMatch key) value withFun

trie_addExtensionCodes :: Trie a -> [(ExtensionCode, a)] -> Trie a
trie_addExtensionCodes initialTrie codes
  = foldl' f initialTrie codes
 where

   f t (c, v) = trie_addExtensionCode t c v

trie_addExtensionCodeAsText :: Trie a -> ExtensionCodeAsText -> a -> Trie a
trie_addExtensionCodeAsText trie key value
  = trie_addExtensionCode trie (Text.unpack key) value

trie_addExtensionCodeAsTextWith :: Trie a -> ExtensionCodeAsText -> a -> ( a-> a -> a) -> Trie a
trie_addExtensionCodeAsTextWith trie key value withFun
  = trie_addExtensionCodeWith trie (Text.unpack key) value withFun

trie_addExtensionCodesAsText :: Trie a -> [(ExtensionCodeAsText, a)] -> Trie a
trie_addExtensionCodesAsText initialTrie codes
  = foldl' f initialTrie codes
 where

   f t (c, v) = trie_addExtensionCodeAsText t c v

type CharMatchStrenght = Int

-- | True if it is matched a strinng without "*" and "X" wildchars.
type IsExtensionalMatch = Bool

trie_getMatch_initial :: (CharMatchStrenght, IsExtensionalMatch)
trie_getMatch_initial = (0, True)

-- | The matching value inside the trie.
--   Used usually for matching a telephone number with the best matching entry,
--   or for matching an extension with an organization.
--
--   Note that "123" match stronger than "1*".
trie_getMatch :: (CharMatchStrenght, IsExtensionalMatch) -> Trie a -> [Char] -> Maybe ((CharMatchStrenght, IsExtensionalMatch), a)

trie_getMatch (s, ie) (Trie (Just value) _) []
  = Just ((s, ie), value)

trie_getMatch (s, ie) (Trie Nothing tries) []
  = case Map.lookup (CharMatch_AllChars) tries of
      Just (Trie (Just r) _) -> Just ((s, False), r)
      Just (Trie Nothing _) -> Nothing
      Nothing -> Nothing

trie_getMatch (s, ie) (Trie _ tries) (c:r)
  = let path1 
          = case Map.lookup (CharMatch_Char c) tries of
              Just nextTrie
                -> trie_getMatch (s + 1, ie) nextTrie r
                   -- NOTE: this can be Nothing in the end if the path has no success.
              Nothing
                -> Nothing

        path2 
          = case Map.lookup (CharMatch_AnyChar) tries of
              Just nextTrie
                -> trie_getMatch (s + 1, False) nextTrie r
              Nothing
                -> case Map.lookup (CharMatch_AllChars) tries of
                     Just (Trie Nothing _)
                       -> Nothing
                     Just (Trie (Just value) _)
                       -> Just ((s, False), value)
                     Nothing
                       -> Nothing

        bestPath Nothing Nothing = Nothing
        bestPath Nothing (Just r) = Just r
        bestPath (Just r) Nothing = Just r
        bestPath (Just ((s1, ie1), value1)) (Just ((s2, ie2), value2)) 
          = if s1 >= s2
            then Just ((s1, ie1), value1)
            else Just ((s2, ie2), value2)

     in bestPath path1 path2

tt_trie_test
  = tests
 where

  trie1 = trie_addExtensionCodes
            trie_empty
            [("123", 1),
             ("1234*", 2),
             ("125X", 3),
             ("125X6*", 4),
             ("1\\X5\\*\\\\,", 5),
             ("456*", 6),
             ("556*", 7),
             ("5*", 8),
             ("6*", 9),
             ("617*", 10),
             ("623X", 11),
             ("623\\XX*", 12),
             ("1\\\\", 41),    
             ("1\\X", 42),    
             ("1\\X\\*1", 43), 
             ("1X\\*", 44),   

             ("812", 81),
             ("81X", 82),
             ("81X2", 83)
            ]

  name1 = "trie_getMatch"

  tests = [HUnit.TestCase (HUnit.assertEqual
                         "t1"
                         (Just ((3, True), 1))
                         (trie_getMatch trie_getMatch_initial trie1 "123")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t2-1"
                         (Just ((4, False), 2))
                         (trie_getMatch trie_getMatch_initial trie1 "12340")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t2-2"
                         (Just ((4, False), 2))
                         (trie_getMatch trie_getMatch_initial trie1 "1234")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t3"
                         (Just ((4, False), 2))
                         (trie_getMatch trie_getMatch_initial trie1 "1234567")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t4"
                         (Just ((4, False), 3))
                         (trie_getMatch trie_getMatch_initial trie1 "1256")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t5-1"
                         (Just ((4, False), 3))
                         (trie_getMatch trie_getMatch_initial trie1 "1256")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t5-2"
                         (Just ((5, False), 4))
                         (trie_getMatch trie_getMatch_initial trie1 "12566")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t5-3"
                         (Just ((5, False), 4))
                         (trie_getMatch trie_getMatch_initial trie1 "125667")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t6"
                         (Just ((5, False), 4))
                         (trie_getMatch trie_getMatch_initial trie1 "12566353")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t7"
                         (Nothing)
                         (trie_getMatch trie_getMatch_initial trie1 "1238")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t8"
                         (Just ((6, True), 5))
                         (trie_getMatch trie_getMatch_initial trie1 "1X5*\\,")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t9"
                         (Just ((3, False), 6))
                         (trie_getMatch trie_getMatch_initial trie1 "4567")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just ((3, False), 6))
                         (trie_getMatch trie_getMatch_initial trie1 "45678")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just ((1, False), 8))
                         (trie_getMatch trie_getMatch_initial trie1 "51")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just ((3, False), 6))
                         (trie_getMatch trie_getMatch_initial trie1 "4567")
                      )
 
          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just ((3, False), 10))
                         (trie_getMatch trie_getMatch_initial trie1 "6178")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just ((3, False), 10))
                         (trie_getMatch trie_getMatch_initial trie1 "6178666")
                      )
         
          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack"
                         (Just ((3, False), 10))
                         (trie_getMatch trie_getMatch_initial trie1 "617")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack"
                         (Just ((3, False), 10))
                         (trie_getMatch trie_getMatch_initial trie1 "6171")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack"
                         (Just ((4, False), 11))
                         (trie_getMatch trie_getMatch_initial trie1 "6231")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just ((1, False), 9))
                         (trie_getMatch trie_getMatch_initial trie1 "623")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just ((1, False), 9))
                         (trie_getMatch trie_getMatch_initial trie1 "625")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just ((1, False), 9))
                         (trie_getMatch trie_getMatch_initial trie1 "618")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just ((1, False), 9))
                         (trie_getMatch trie_getMatch_initial trie1 "619")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just ((1, False), 9))
                         (trie_getMatch trie_getMatch_initial trie1 "6753")
                      )

         ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just ((1, False), 9))
                         (trie_getMatch trie_getMatch_initial trie1 "62")
                      )

         ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         (Just ((2, True), 41))
                         (trie_getMatch trie_getMatch_initial trie1 "1\\")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         (Just ((2, True), 42))
                         (trie_getMatch trie_getMatch_initial trie1 "1X")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         Nothing
                         (trie_getMatch trie_getMatch_initial trie1 "1B")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         (Just ((4, True), 43))
                         (trie_getMatch trie_getMatch_initial trie1 "1X*1")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         (Just ((3, False), 44))
                         (trie_getMatch trie_getMatch_initial trie1 "1B*")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         Nothing
                         (trie_getMatch trie_getMatch_initial trie1 "1B*A")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes"
                         Nothing
                         (trie_getMatch trie_getMatch_initial trie1 "1XA")
                      )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 1"
                         (Just ((3, True), 81))
                         (trie_getMatch trie_getMatch_initial trie1 "812")
                      )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 2"
                         (Just ((3, False), 82))
                         (trie_getMatch trie_getMatch_initial trie1 "813")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 3"
                         (Just ((4, False), 83))
                         (trie_getMatch trie_getMatch_initial trie1 "8122")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 4"
                         Nothing
                         (trie_getMatch trie_getMatch_initial trie1 "81222")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 5"
                         (Just ((5, False), 12))
                         (trie_getMatch trie_getMatch_initial trie1 "623XA")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 6"
                         (Just ((5, False), 12))
                         (trie_getMatch trie_getMatch_initial trie1 "623XANA")
                       )
          ]
