{-# Language OverloadedStrings, ScopedTypeVariables #-}

-- Some code producing other code in an interactive way, for not writing repetitive code sequences completely by hand.

module Main where

import Data.List
import Data.Char


tableFields_party 
  = ["id"
    ,"name"
    ,"compact_name"
    ,"external_crm_code"
    ,"vat"
    ,"is_billable"
    ,"legal_address"
    ,"legal_city"
    ,"legal_zipcode"
    ,"legal_state_province"
    ,"legal_country"
    ,"email"
    ,"phone"
    ,"phone2"
    ,"fax"
    ,"max_limit_30"
    ,"last_email_advise_for_max_limit_30"
    ,"is_active"
    ,"is_reseller"
    ,"reseller_code"]

vars1 :: [String]
vars1 
  = [
     "name"
    ,"short_name"
    ,"is_active"
    ,"is_billable"
    ,"email"
    ,"telephone1"
    ,"telephone2"
    ,"is_reseller"
    ,"reseller_short_code"
    ,"VAT"
    ,"crm_code"
    ,"country"
    ,"state_province"
    ,"city"
    ,"address"
    ,"credit_limit"
   ]

tableFields_structure
  = ["ar_organization_unit_id"
    ,"ar_organization_unit_type_id"
    ,"ar_parent_organization_unit_id"
    ,"from"
    ,"exists"
    ,"ar_rate_category_id"
    ,"ar_party_id"
    ,"extension_codes"
    ,"extension_name"
    ,"extension_user_code"
    ]

table2 :: [(String, String)]
table2 
  = [
      ("name","name")
    , ("short_name","")
    , ("is_active","")
    , ("is_billable","")
    , ("email","email")
    , ("telephone1","phone")
    , ("telephone2","phone2")
    , ("is_reseller","")
    , ("reseller_short_code","")
    , ("VAT","vat")
    , ("crm_code","external_crm_code")
    , ("country","legal_country")
    , ("state_province","legal_state_province")
    , ("city","legal_city")
    , ("zip_code","legal_zipcode")
    , ("address","legal_address")
    , ("credit_limit","max_limit_30")
    , ("telephonic_service_migration_field","")
    , ("internet_service_migration_field","")
    ]

{-|
   Transform first letter of 'String' using the function given.
   Will not work on 'Data.Text'.
-}
transformFst :: (Char -> Char) -> String -> String
transformFst _ [] = []
transformFst f (x:xs) = (f x):xs
 
{-|
   Make 'String' begin with a capital letter using 'toUpper' transformation.
-}
capitalize :: String -> String
capitalize = transformFst toUpper

{-|
   Make 'String' not begin with a capital letter
   using 'toLower' transformation.
-}
uncapitalize :: String -> String
uncapitalize = transformFst toLower

{-|
   Split a 'String' into list of Strings.
   It is just a Prelude function 'words', but it accpets a specified
   predicate to determine delimiter.
 
   NOTE: It won't work on 'Data.Text'.
-}
split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
          where
            (w, s'') = break p s'

{-|
   Convert a 'String' to CamelCase.
   First, split it by \"_\" character.
   Then apply 'capitalize' on each subpart.
   Finally, concat.
-}
toCamelCase :: String -> String
toCamelCase = concat . map' . split (== '_')
  where
    map' = map capitalize
-----------------------------------------------------------------------------
 
-----------------------------------------------------------------------------
{-|
   Convert a 'String' to mixedCase.
   Just combine 'uncapitalize' and 'toCamelCase'.
-}
toMixedCase :: String -> String
toMixedCase = uncapitalize . toCamelCase
-----------------------------------------------------------------------------
 
-----------------------------------------------------------------------------
{-|
   Convert a 'String' downcase.
   Just 'map' it 'toLower' function.
-}
downcase :: String -> String
downcase = map toLower
-----------------------------------------------------------------------------
 
-----------------------------------------------------------------------------
{-|
   Split a 'String', by specified predicate, but do not remove matched
   characters from the result.
 
   Recursive implementation inspired by the "Real World Haskell" book.
 
-}
splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case break p s' of
      (b', [])     -> [ m:b' ]
      (b', (x:xs)) -> ( m:b' ) : go x xs
  in case break p s of
    (b,  [])    -> [ b ]
    ([], (h:t)) -> go h t
    (b,  (h:t)) -> b : go h t
-----------------------------------------------------------------------------
 
-----------------------------------------------------------------------------
{-|
   Convert CamelCased or mixedCases 'String' to a 'String' with underscores,
   the \"snake\" 'String'.
 
   It splits an input value to chunks by 'isUpper' predicate,
   then adds underscores to each element except the first.
   Finally concats the result and convers it downcase.
-}
toSnake :: String -> String
toSnake = downcase . concat . underscores . splitR isUpper
  where
    underscores :: [String] -> [String]
    underscores [] = []
    underscores (h:t) = h : map ('_':) t


produceInsertQuery :: String -> [String] -> String
produceInsertQuery tableName fields
  = "INSERT INTO " ++ tableName ++ "(" ++ intercalate ", " fields ++ ") VALUES(" ++ intercalate ", " (replicate (length fields) "?") ++ ");"

produceUpdateQuery :: String -> [String] -> String
produceUpdateQuery tableName fields
  = "UPDATE " ++ tableName ++ " SET " ++ (intercalate ", " $ map (\f -> f ++ " = ? ") fields) ++ ";"

produceCamelCaseFields :: String -> [String] -> [String]
produceCamelCaseFields prefix fields
  = map (\f -> toCamelCase (prefix ++ "_" ++ f)) fields






t1 = produceInsertQuery "ar_party" tableFields_party

t2 = produceUpdateQuery "ar_party" tableFields_party

t3 = intercalate ", " $ produceCamelCaseFields "$party" tableFields_party

t4 
  = do let s = intercalate ", \n" $ map (\v -> "$this->getRequiredValue($partySpec,\"" ++ v ++ "\")") vars1
       putStrLn s


t5 = do putStrLn $ produceInsertQuery "ar_organization_unit_has_structure" tableFields_structure
        putStrLn ""
        putStrLn $ produceUpdateQuery "ar_organization_unit_has_structure" tableFields_structure


t6  
  = do putStrLn $ concatMap (\(y, s) 
                                -> let n = "$rsParty"
                                       v = case s of
                                             [] -> "''"
                                             _ -> n ++ "['" ++ s ++ "']" 
                                   in "\nfwrite($h, indent($scope) . '" ++ y ++ ": ' . " ++ v ++ ");"
                            ) table2
