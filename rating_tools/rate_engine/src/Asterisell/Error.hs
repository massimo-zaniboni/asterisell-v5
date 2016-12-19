{-# Language OverloadedStrings, ScopedTypeVariables, DeriveGeneric, DeriveDataTypeable #-}

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


-- | Transfer errors to the PHP side.
--
module Asterisell.Error (
  ErrorType(..),
  ErrorDomain(..),
  ErrorResponsible(..),
  AsterisellError (..),
  RuleCaseFor (..),
  ruleCaseFor,
  test_ruleCaseFor,
  asterisellError_empty,
  writeError,
  writeErrorWithoutGarbageCollection,
  createError,
  AsterisellErrorDupKey,
  asterisellError_dupKey,
  AsterisellErrorsDictionary,
  asterisellErrorsDictionary_empty,
  asterisellError_userShow,
  DebugMessage,
  debugMessage_create,
  dbc_error,
  initErrors,
  debugFile_init
) where

import Asterisell.Utils

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import GHC.Generics
import Data.Typeable
import qualified Data.ByteString as BS
import Data.Time.LocalTime
import Database.MySQL.Base as DB
import Data.Hashable
import Data.HashSet as Set
import Data.String
import Control.Monad.Except
import qualified Test.HUnit as HUnit
import Data.Text
import Control.Concurrent.Async
import Control.DeepSeq


data ErrorType = Type_Info | Type_Warning | Type_Error | Type_Critical | Type_InternalLog
  deriving (Eq, Show)

errorType_toPHPCode :: ErrorType -> Int
errorType_toPHPCode t
  = case t of
      Type_Info -> 10
      Type_Warning -> 20
      Type_Error -> 30
      Type_Critical -> 40
      Type_InternalLog -> 50

data ErrorDomain
  =  Domain_VOIP_ACCOUNTS
  |  Domain_RATES
  |  Domain_CALL_FLOW_MERGING_RULES
  |  Domain_APPLICATION
  |  Domain_REPORTS
  |  Domain_CONFIGURATIONS
  |  Domain_SAFETY
  deriving(Eq, Show)


errorDomain_toPHPCode :: ErrorDomain -> Int
errorDomain_toPHPCode t
  = case t of
      Domain_VOIP_ACCOUNTS -> 10
      Domain_RATES -> 20
      Domain_CALL_FLOW_MERGING_RULES -> 30
      Domain_APPLICATION -> 40
      Domain_REPORTS -> 50
      Domain_CONFIGURATIONS -> 60
      Domain_SAFETY -> 70

data ErrorResponsible
   = Responsible_ADMIN
   | Responsible_ASSISTANCE
  deriving(Eq, Show)

errorResponsible_toPHPCode :: ErrorResponsible -> Int
errorResponsible_toPHPCode t
  = case t of
      Responsible_ASSISTANCE -> 20
      Responsible_ADMIN -> 10

-- | An error to show on Asterisell UI.
--
data AsterisellError = AsterisellError {
    asterisellError_type :: ErrorType
  , asterisellError_domain :: ErrorDomain
  , asterisellError_responsible :: ErrorResponsible
  , asterisellError_key :: String
  , asterisellError_description :: String
  , asterisellError_effect :: String
  , asterisellError_proposedSolution :: String
 } deriving (Show, Generic, Typeable)

instance NFData AsterisellError where
  rnf e = seq e ()

instance Eq AsterisellError where
  e1 == e2 = asterisellError_key e1 == asterisellError_key e2

instance Ord AsterisellError where
  compare e1 e2 = compare (asterisellError_key e1) (asterisellError_key e2)

asterisellError_userShow :: AsterisellError -> String
asterisellError_userShow err
  = "Description: " ++ (asterisellError_description err) ++ "\n\nEffect: " ++ (asterisellError_effect err) ++ "\n\nSolution: " ++ (asterisellError_proposedSolution err)

asterisellError_empty :: AsterisellError
asterisellError_empty = AsterisellError {
            asterisellError_type = Type_Error
          , asterisellError_domain = Domain_RATES
          , asterisellError_responsible = Responsible_ADMIN
          , asterisellError_key = ""
          , asterisellError_description = ""
          , asterisellError_effect = ""
          , asterisellError_proposedSolution = ""
          }

initErrors :: DB.Connection -> BS.ByteString -> LocalTime -> LocalTime -> IO AsterisellErrorsDictionary
initErrors db garbageKey fromDate toDate
  = do gc <- DB.escape db garbageKey

       let query :: BS.ByteString
             = BS.concat [
                  "DELETE FROM ar_new_problem WHERE garbage_collection_key = '", gc, "'"
                , " AND garbage_collection_from >= '"
                , (fromString $ fromLocalTimeToMySQLDateTime fromDate), "' "
                , " AND garbage_collection_to <= '"
                , (fromString $ fromLocalTimeToMySQLDateTime toDate), "';"
                ]
       DB.query db query
       return asterisellErrorsDictionary_empty

-- | Add an Asterisell error in the error table, only if it is new.
writeError
  :: Bool
  -> DB.Connection
  -> AsterisellErrorsDictionary
  -> AsterisellError
  -> BS.ByteString
  -> LocalTime
  -> LocalTime
  -> IO (AsterisellErrorDupKey, AsterisellErrorsDictionary)

writeError writeOnDB db dict err garbageKey fromDate toDate
  = writeError1 writeOnDB db dict err (Just (garbageKey, fromDate, toDate))

-- | Add an Asterisell error in the error table, only if it is new.
writeErrorWithoutGarbageCollection
  :: Bool
  -> DB.Connection
  -> AsterisellErrorsDictionary
  -> AsterisellError
  -> IO (AsterisellErrorDupKey, AsterisellErrorsDictionary)

writeErrorWithoutGarbageCollection writeOnDB db dict err
  = writeError1 writeOnDB db dict err Nothing

-- | Add an Asterisell error in the error table, only if it is new.
writeError1
  :: Bool
  -> DB.Connection
  -> AsterisellErrorsDictionary
  -> AsterisellError
  -> Maybe (BS.ByteString, LocalTime, LocalTime)
  -> IO (AsterisellErrorDupKey, AsterisellErrorsDictionary)

writeError1 writeOnDB db dict err maybeGarbageKey
  = do  let dupKey1 = asterisellError_dupKey err
        dupKey <- DB.escape db $ fromTextToMySQLResult dupKey1
        d <- DB.escape db $ fromString $ asterisellError_description err
        e <- DB.escape db $ fromString $ asterisellError_effect err
        f <- DB.escape db $ fromString $ asterisellError_proposedSolution err

        garbageKeyB
          <- case maybeGarbageKey of
               Nothing
                 -> return $ ", NULL, NULL, NULL"
               Just (garbageKey, fromDate, toDate)
                 -> return $ BS.concat [ ", '", garbageKey, "'"
                                       , ", '", fromString $ fromLocalTimeToMySQLDateTime fromDate, "'"
                                       , ", '", fromString $ fromLocalTimeToMySQLDateTime toDate, "'"]


        let query
              = BS.concat [
                  "INSERT INTO ar_new_problem(ar_problem_type_id, ar_problem_domain_id, ar_problem_responsible_id, created_at, duplication_key, garbage_collection_key, garbage_collection_from, garbage_collection_to, description, effect, proposed_solution, signaled_to_admin) VALUES("
                  , fromString $ show $ errorType_toPHPCode $ asterisellError_type err
                  , ", ", fromString $ show $ errorDomain_toPHPCode $ asterisellError_domain err
                  , ", ", fromString $ show $ errorResponsible_toPHPCode $ asterisellError_responsible err
                  , ", NOW()"
                  , ", '", dupKey, "'"
                  , garbageKeyB
                  , ", '", d, "'"
                  , ", '", e, "'"
                  , ", '", f, "'"
                  , ", 0) ON DUPLICATE KEY UPDATE garbage_collection_from = LEAST(garbage_collection_from, VALUES(garbage_collection_from)), garbage_collection_to = GREATEST(garbage_collection_to, VALUES(garbage_collection_to)), ar_problem_type_id = VALUES(ar_problem_type_id), ar_problem_domain_id  = VALUES(ar_problem_domain_id)                   , ar_problem_responsible_id  = VALUES(ar_problem_responsible_id), created_at = VALUES(created_at), garbage_collection_key = VALUES(garbage_collection_key ), description = VALUES(description ), effect = VALUES(effect), proposed_solution = VALUES(proposed_solution), signaled_to_admin  = signaled_to_admin;"
                  ]

        case Set.member dupKey1 dict of
           True ->  do return (dupKey1, dict)
           False -> do when writeOnDB (DB.query db query)
                       return (dupKey1, Set.insert dupKey1 dict)


createError :: ErrorType -> ErrorDomain -> String -> String -> String -> String -> AsterisellError
createError errType errDomain key descr effect solution
     = asterisellError_empty {
         asterisellError_type = errType
       , asterisellError_domain = errDomain
       , asterisellError_key = key
       , asterisellError_description = descr
       , asterisellError_effect = effect
       , asterisellError_proposedSolution = solution
     }

--
-- DEBUG MESSAGES
--

-- | An AsterisellError used ad debug/informative message, associated to a CDR.
type DebugMessage = AsterisellError

debugMessage_create :: String -> String -> AsterisellError
debugMessage_create key description
  = createError Type_InternalLog Domain_RATES key description "This is only an informative/debug message." ""

--
-- ERROR DICTIONARY
--

type AsterisellErrorDupKey = Text

asterisellError_dupKey :: AsterisellError -> AsterisellErrorDupKey
asterisellError_dupKey err
  = let k = asterisellError_key err
        x = hashWithSalt 1 k
        y = hashWithSalt 2 k
    in  fromString $ (show x ++ "-" ++ show y)

-- | Recognize duplicated AsterisellErrors
type AsterisellErrorsDictionary = Set.HashSet AsterisellErrorDupKey

asterisellErrorsDictionary_empty = Set.empty

--
-- MONADPLUS RELATED FUNCTIONS
--

-- | Support MonadPlus and Exceptions.
--   An Exceptions bypass all calls, and return the error.
--   The MonadPlus return the first result, or the first exception.
--
newtype RuleCaseFor a = RuleCaseFor (Maybe (Either String a))

instance Show a => Show (RuleCaseFor a) where
  show (RuleCaseFor x) = show x

instance Eq a => Eq (RuleCaseFor a) where
  (RuleCaseFor x) == (RuleCaseFor y) = x == y

instance Ord a => Ord (RuleCaseFor a) where
  compare (RuleCaseFor x) (RuleCaseFor y) = compare x y

instance Monad RuleCaseFor where
  return x = RuleCaseFor (Just $ Right x)

  (RuleCaseFor (Just (Left err))) >>= _ = RuleCaseFor (Just (Left err))
  (RuleCaseFor Nothing) >>= x = RuleCaseFor Nothing
  (RuleCaseFor (Just (Right x))) >>= f = f x

  fail err = RuleCaseFor $ Just $ Left err

instance Functor RuleCaseFor where
    fmap = liftM

instance Applicative RuleCaseFor where
  pure = return
  (<*>) = ap

instance Alternative RuleCaseFor where
  (<|>) = mplus
  empty = mzero

instance MonadPlus RuleCaseFor where
  mzero = RuleCaseFor Nothing

  mplus (RuleCaseFor (Just (Left err))) _ = RuleCaseFor (Just (Left err))
  mplus (RuleCaseFor Nothing) x = x
  mplus (RuleCaseFor (Just (Right x))) _ = RuleCaseFor (Just (Right x))

ruleCaseErrorMsg :: String -> String
ruleCaseErrorMsg msg =" There is no valid sub-case for rule of type: " ++ msg

-- | Try to apply one rule case, otherwise return an error.
--   So it is mandatory that at least one case is respected,
--   otherwise the named exception (and not the failure) is returned.
ruleCaseFor :: String -> [RuleCaseFor a] -> RuleCaseFor a
ruleCaseFor d rules1
  = let rule2 = fail $ ruleCaseErrorMsg d
        rules2 = rules1 ++ [rule2]
    in  msum rules2

test_ruleCaseFor
  = [ HUnit.TestCase $ HUnit.assertEqual "1" (ti 100) (fun 100)
    , HUnit.TestCase $ HUnit.assertEqual "2" (ti 60) (fun 60)
    , HUnit.TestCase $ HUnit.assertEqual "3" (ti 60) (fun 61)
    , HUnit.TestCase $ HUnit.assertEqual "4" (ti 55) (fun 55)
    , HUnit.TestCase $ HUnit.assertEqual "5" (ti 55) (fun 56)
    , HUnit.TestCase $ HUnit.assertEqual "6" (ti 25) (fun 25)
    , HUnit.TestCase $ HUnit.assertEqual "e1" (isError "50") (fun 50)
    , HUnit.TestCase $ HUnit.assertEqual "e2" (isError "100") (fun 5)
    , HUnit.TestCase $ HUnit.assertEqual "e3" (isFail "51") (fun 51)

    ]

 where

   ti x = RuleCaseFor $ Just $ Right x

   isError x = (RuleCaseFor (Just (Left $ ruleCaseErrorMsg x)))

   isFail x = (RuleCaseFor (Just (Left x)))

   fun  :: Int -> RuleCaseFor Int
   fun  x
     = ruleCaseFor
         "100"
         [(do guard (x >= 100)
              return x)
         ,(do guard (x >= 50)
              ruleCaseFor
                "50"
                [(do guard (x >= 60)
                     return 60)
                ,(do guard (x >= 55)
                     return 55)
                ,(do guard (x == 51)
                     fail "51")
                ])
         ,(do guard (x >= 25)
              return 25)
         ]

-- | Init (maybe) the debug file.
debugFile_init :: Maybe FilePath -> IO ()
debugFile_init Nothing = return ()
debugFile_init (Just f)
  = writeFile f ""

--
-- Design by Contract Errors
--

dbc_error :: String -> String
dbc_error errorCode
  = "There is an error in the application code. The code is not respecting some needed conditions. This is an error in the application. Contact the assistance, communicating the error code \"" ++ errorCode ++ "\""
