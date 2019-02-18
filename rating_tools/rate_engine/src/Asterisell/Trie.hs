{-# Language OverloadedStrings, ScopedTypeVariables, BangPatterns, DeriveGeneric, DeriveAnyClass, FlexibleInstances, TypeSynonymInstances #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>


module Asterisell.Trie (
  CharMatchStrenght,
  ExtensionCode,
  Extension,
  ExtensionAsText,
  Trie,
  IsExtensionalMatch,
  trie_match,
  trie_empty,
  trie_insert,
  trie_insertWith,
  trie_insertExtension,
  trie_insertExtensionWith,
  trie_insertExtensions,
  trie_insertExtensionsAsText,
  trie_insertExtensionAsText,
  trie_insertExtensionAsTextWith,
  tt_trie_test,
  trie_lookup,
  trie_insertUnique,
  extension_toExtensionCode
) where

import Asterisell.Utils

import qualified Data.Trie.BigEndianPatricia.Base as Trie
import qualified Data.Trie.BigEndianPatricia.Internal as TrieInternal
import qualified Data.Trie.BigEndianPatricia.Convenience as Trie
import Data.List as L
import Data.Char
import Data.Either
import Control.Monad.State.Strict
import Data.Ratio
import Data.Maybe
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString as BS
import Data.ByteString.Internal as BS (c2w, w2c)
import Data.Default
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Test.HUnit as HUnit
import qualified Data.Text as Text
import GHC.Generics
import Control.DeepSeq

-- | An extension code where:
--     * "X" stays for the any-char,
--     * multiple "XX" are allowed at the end
--     * "*" stays for zero or more chars,
--     * "X" and "*" have special meaning only if used at the end, while if they are used in the middle they are normal literals
--     * "ABX*D" is the exact literal
--     * "ABX*" is "ABX" followed by zero or more chars
--     * "AB*X" is "AB*" followed by any char
--     * "AB**" is "AB*" followed by zero or more chars
type Extension = BS.ByteString

type ExtensionAsText = Text.Text

-- | True if the extension match is not an exact match,
--   but it is matching a generic extension specification.
type IsExtensionalMatch = Bool

-- | The prefix and the generic "X" suffix len, 0 for no other part, or Nothing for "*"
type ExtensionCode = (BS.ByteString, Maybe Int)

-- | Convert an extension to code.
extension_toExtensionCode :: Extension -> ExtensionCode
extension_toExtensionCode e1
  = case BS.null e1 of
      True
        -> (BS.empty, Just 0)
      False
        -> let e2 = BS.reverse e1
               h = BS.head e2
           in if h == c2w '*'
              then (BS.init e1, Nothing)
              else if h == c2w 'X'
                   then let anyChar = BS.takeWhile ((==) (c2w 'X')) e2
                            suffixLen = BS.length anyChar
                        in (BS.take ((BS.length e1) - suffixLen) e1, Just suffixLen)
                   else (e1, Just 0)

-- | Store numbers like: "123", "123XX", "123*"
--   but not like "12X1", "123X*", "12*3"
type Trie a
       = Trie.Trie
           (IntMap.IntMap a
           -- ^ the values associated to the "X" parts, with some exact length
           , Maybe a
           -- ^ the value associated to the "*" part, with variable length,
           -- having less priority of the exact len part
           )

trie_empty :: Trie a
trie_empty = Trie.empty

type CharMatchStrenght = Int

-- | Used for matching a telephone number with the best matching entry,
--   or for matching an extension with an organization.
--   String with exact len, and exact characters are preferred to strings with "*" parts.
trie_match :: Trie a -> BS.ByteString -> Maybe (CharMatchStrenght, IsExtensionalMatch, a)
trie_match trie initialTarget
    = match True initialTarget
 where

  match canUseExactMatch target
    = case Trie.match trie target of
        Nothing
          -> Nothing
        Just (prefix, (exactLenMap, genericLenValue), targetSuffix)
          -> let 
                 genericMatchResult
                   = case genericLenValue of
                       Just v
                         -> Just $ (BS.length prefix, True, v)
                       Nothing
                         -> match False (BS.init prefix)
                            -- NOTE: "35678" is not matched by "356X" but from "35*", so try a shorter version 

                 targetSuffixLen = BS.length targetSuffix

             in case canUseExactMatch of
                  True
                    -> case IntMap.lookup targetSuffixLen exactLenMap of
                         Just v  -> Just $ (BS.length prefix + targetSuffixLen, targetSuffixLen /= 0, v)
                         Nothing -> genericMatchResult
                  False
                    -> genericMatchResult

-- | Exact lookup of the associated value.
trie_lookup :: Trie a -> ExtensionCode -> Maybe a
trie_lookup trie (prefix, suffix)
  = case Trie.lookup prefix trie of
      Nothing -> Nothing
      Just (exactLenMap, genericLenValue)
        -> case suffix of
             Nothing
               -> genericLenValue
             Just l
               -> IntMap.lookup l exactLenMap

trie_insertWith
    :: Trie a
    -> ExtensionCode
    -> a
    -> (a -> a -> a)
    -- ^ how merge the new value with the old value
    -> Trie a

trie_insertWith trie (prefix, maybeSuffix) x withFun
  = Trie.insertWith' mergeWith prefix y trie

 where

   y = case maybeSuffix of
         Nothing -> (IntMap.empty, Just x)
         Just l -> (IntMap.singleton l x, Nothing)

   mergeWith (newExactLenMap, newGenericLenValue) (oldExactLenMap, oldGenericLenValue)
     = (IntMap.unionWith (withFun) newExactLenMap oldExactLenMap
       , case (newGenericLenValue, oldGenericLenValue) of
           (Just newValue, Nothing) -> Just newValue
           (Nothing, Just oldValue) -> Just oldValue
           (Just newValue, Just oldValue) -> Just $ withFun newValue oldValue
           (Nothing, Nothing) -> Nothing
       )

trie_insert :: Trie a -> ExtensionCode -> a -> Trie a
trie_insert trie key x = trie_insertWith trie key x (\newValue oldValue -> newValue)
{-# INLINE trie_insert #-}

trie_insertUnique
  :: (Eq a)
  => Trie a
  -> ExtensionCode
  -- ^ the extension code to add
  -> a
  -- ^ the associated value
  -> Maybe (Trie a)
  -- ^ Nothing if the ExtensionCode is in conflict with an already defined ExtensionCode.
  --   It is a conflict if the extension code is the same, but the value different.
 
trie_insertUnique trie key value
  = case trie_lookup trie key of
      Just oldValue
        -> case value == oldValue of
             True -> Just trie
             False -> Nothing
      Nothing
        -> Just $ trie_insert trie key value 

trie_insertExtension :: Trie a -> Extension -> a -> Trie a
trie_insertExtension trie key value
  = trie_insert trie (extension_toExtensionCode key) value

trie_insertExtensionWith :: Trie a -> Extension -> a -> (a -> a -> a) -> Trie a
trie_insertExtensionWith trie key value withFun
  = trie_insertWith trie (extension_toExtensionCode key) value withFun

trie_insertExtensions :: Trie a -> [(Extension, a)] -> Trie a
trie_insertExtensions initialTrie codes
  = L.foldl' f initialTrie codes
 where
   f t (c, v) = trie_insertExtension t c v

trie_insertExtensionAsText :: Trie a -> Text.Text -> a -> Trie a
trie_insertExtensionAsText trie key value
  = trie_insertExtension trie (fromTextToByteString key) value

trie_insertExtensionAsTextWith :: Trie a -> Text.Text -> a -> ( a-> a -> a) -> Trie a
trie_insertExtensionAsTextWith trie key value withFun
  = trie_insertExtensionWith trie (fromTextToByteString key) value withFun

trie_insertExtensionsAsText :: Trie a -> [(Text.Text, a)] -> Trie a
trie_insertExtensionsAsText initialTrie codes
  = L.foldl' f initialTrie codes
 where

   f t (c, v) = trie_insertExtensionAsText t c v

tt_trie_test
  = tests
 where

  trie1 = trie_insertExtensions
            trie_empty
            [("123", 1),
             ("1234*", 2),
             ("1234", 3),
             ("125X", 3),
             ("456*", 6),
             ("556*", 7),
             ("5*", 8),
             ("6*", 9),
             ("617*", 10),
             ("623X", 11),
             ("623XA", 12),
             ("1X*1", 43),
             ("812", 81),
             ("81X", 82)
            ]

  name1 = "trie_match"

  tests = [
           HUnit.TestCase
             (HUnit.assertEqual
                "code 1"
                ("123", Just 0)
                (extension_toExtensionCode "123"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 2"
                ("123", Just 1)
                (extension_toExtensionCode "123X"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 3"
                ("123", Just 2)
                (extension_toExtensionCode "123XX"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 4"
                ("123", Nothing)
                (extension_toExtensionCode "123*"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 5"
                ("123XA", Just 0)
                (extension_toExtensionCode "123XA"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 7"
                ("123X", Nothing)
                (extension_toExtensionCode "123X*"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 8"
                ("123X11", Just 0)
                (extension_toExtensionCode "123X11"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 9"
                ("123*", Just 1)
                (extension_toExtensionCode "123*X"))
          ,HUnit.TestCase
             (HUnit.assertEqual
                "code 10"
                ("123*1", Just 0)
                (extension_toExtensionCode "123*1"))

          ,HUnit.TestCase (HUnit.assertEqual
                         "t1"
                         (Just (3, False, 1))
                         (trie_match trie1 "123")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t2-1"
                         (Just (4, True, 2))
                         (trie_match trie1 "12340")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t2-2"
                         (Just (4, False, 3))
                         (trie_match trie1 "1234")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t3"
                         (Just (4, True, 2))
                         (trie_match trie1 "1234567")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t4"
                         (Just (4, True, 3))
                         (trie_match trie1 "1256")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t5-1"
                         (Just (4, True, 3))
                         (trie_match trie1 "1256")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t5-2"
                         Nothing
                         (trie_match trie1 "12566")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t5-3"
                         Nothing
                         (trie_match trie1 "125667")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t7"
                         (Nothing)
                         (trie_match trie1 "1238")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "t9"
                         (Just (3, True, 6))
                         (trie_match trie1 "4567")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just (3, True, 6))
                         (trie_match trie1 "45678")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just (1, True, 8))
                         (trie_match trie1 "51")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just (3, True, 6))
                         (trie_match trie1 "4567")
                      )
 
          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just (3, True, 10))
                         (trie_match trie1 "6178")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         name1
                         (Just (3, True, 10))
                         (trie_match trie1 "6178666")
                      )
         
          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack"
                         (Just (3, True, 10))
                         (trie_match trie1 "617")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack"
                         (Just (3, True, 10))
                         (trie_match trie1 "6171")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack"
                         (Just (4, True, 11))
                         (trie_match trie1 "6231")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just (1, True, 9))
                         (trie_match trie1 "623")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just (1, True, 9))
                         (trie_match trie1 "62344")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just (1, True, 9))
                         (trie_match trie1 "618")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just (1, True, 9))
                         (trie_match trie1 "619")
                      )

          ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just (1, True, 9))
                         (trie_match trie1 "6753")
                      )

         ,HUnit.TestCase (HUnit.assertEqual
                         "backtrack with fail"
                         (Just (1, True, 9))
                         (trie_match trie1 "62")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes 3"
                         Nothing
                         (trie_match trie1 "1B")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes 4"
                         (Just (4, False, 43))
                         (trie_match trie1 "1X*1")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes 5"
                         Nothing
                         (trie_match trie1 "1B*")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes 6"
                         Nothing
                         (trie_match trie1 "1B*A")
                      )

        ,HUnit.TestCase (HUnit.assertEqual
                         "recognize quotes 7"
                         Nothing
                         (trie_match trie1 "1XA")
                      )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 1"
                         (Just (3, False, 81))
                         (trie_match trie1 "812")
                      )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 2"
                         (Just (3, True, 82))
                         (trie_match trie1 "813")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 3"
                         Nothing
                         (trie_match trie1 "8122")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 4"
                         Nothing
                         (trie_match trie1 "81222")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 5"
                         (Just (5, False, 12))
                         (trie_match trie1 "623XA")
                       )

       ,HUnit.TestCase (HUnit.assertEqual
                         "priority 6"
                         (Just (4, True, 11))
                         (trie_match trie1 "6231")
                       )
          ]
