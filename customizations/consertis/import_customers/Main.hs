{-# Language OverloadedStrings, ScopedTypeVariables, DeriveGeneric, BangPatterns #-}

-- | Load CSV rates, normalize them, and try to find a common structure.
module Main where

import Data.Char
import Data.Text as Text
import Data.Text.IO as Text
import Data.Text.Read as Text
import qualified Data.Text.Read as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LB
import Data.Map as Map
import Data.Set as Set
import Data.Maybe
import Data.List as L
import Data.Ratio 
import Control.Monad
import Text.Parsec.Text as P
import Text.Parsec as P
import Text.Parsec.Char as P
import Data.Maybe
import System.IO as IO
import Debug.Trace
import Data.Csv
import GHC.Generics
import qualified Data.Vector as V

--
-- Extract DID from CDRs
--

type FoldType r v = r -> v -> r

type DID = Text.Text

data RDID = RDID { }

-- | A format like
--
-- > 150194;2016420000000;DAD;DAD;4315132***;01.08.2016;11:26:54;St.Pölten;Colt LOCAL;0274290136***;22;0.01019;0;02.09.2016;EUR;;VO15;P
data CSVFormat_colt
  = CSVFormat_colt {
        colt__providerId :: !Text
      , colt__2 :: !Text
      , colt__3 :: !Text
      , colt__4 :: !Text
      , colt__caller :: !Text
      , colt__date :: !Text
        -- ^ format:  01.08.2016
      , colt__time :: !Text
        -- ^ format: 11:26:54;
      , colt__8 :: !Text
      , colt__9 :: !Text
      , colt__called :: !Text
      , colt__billsec :: !Text
      , colt__cost :: !Text
        -- ^ format: 0.01019
      , colt__13:: !Text
      , colt__14:: !Text
      , colt__15:: !Text
      , colt__16:: !Text
      , colt__17:: !Text
      , colt__18:: !Text
      , colt__19:: !Text
      , colt__20:: !Text
   } deriving(Show, Generic)

instance FromRecord CSVFormat_colt

csvDelimeter1 :: DecodeOptions
csvDelimeter1 = defaultDecodeOptions { decDelimiter = fromIntegral (ord ',') }

csvDelimeter2 :: DecodeOptions
csvDelimeter2 = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

parseColtCDRS :: LB.ByteString -> Set DID
parseColtCDRS content
  = case decodeWith csvDelimeter2 NoHeader content of
      Left err
        -> error err
      Right (records :: V.Vector CSVFormat_colt)
        -> Set.fromList $ L.map (colt__caller) (V.toList $ V.tail records)

parseColtCDRSAndWriteDIDS :: IO ()
parseColtCDRSAndWriteDIDS = do
  let fileName = "COLT_122016"
  fileContent <- LB.readFile fileName
  let dids = parseColtCDRS fileContent
  IO.putStrLn $ "DIDS are: " ++ show (Set.size dids)
  writeDIDS "dids.csv" dids
  return ()

writeDIDS :: String -> Set DID -> IO ()
writeDIDS fileName dids = do 
  handle <- IO.openFile fileName WriteMode
  hSetEncoding handle utf8_bom
  mapM_ (\d -> IO.hPutStr handle ((Text.unpack d) ++ crln)) (Set.toAscList dids)
  IO.hClose handle
  where

    crln :: String
    crln = "\r\n"

--
-- Import Customers
--

type ClientID = Text.Text
type RateID = Text.Text

type CustomerDIDS = Map DID (ClientID, RateID)

data Customer
    = Customer {
        c_clientID :: !ClientID
      , c_mr :: !Text.Text
      , c_title :: !Text.Text
      , c_name :: !Text.Text
      , c_surname :: !Text.Text
      , c_company :: !Text.Text
      , c_address :: !Text.Text
      , c_zipCode :: !Text.Text
      , c_city :: !Text.Text
      , c_bic :: !Text.Text
      , c_iban :: !Text.Text
      , c_email :: !Text.Text
      } deriving (Eq, Show, Generic)

instance FromRecord Customer
instance ToRecord Customer

customer_completeName :: Customer -> Text.Text
customer_completeName c
  = case Text.length (c_company c) of
      0 -> Text.concat [c_mr c, sep c_mr, c_title c, sep c_title, c_name c, " ", c_surname c]
      _ -> (c_company c)
 where

  sep m = case Text.length (m c) of
            0 -> ""
            _ -> " "

type Customers = Map ClientID Customer

-- | Return correct customers, and number of customers with an error.
parseCustomers1 :: LB.ByteString -> (Customers, Int)
parseCustomers1 content
  = case decodeWith csvDelimeter1 NoHeader content of
      Left err -> error err
      Right (records::V.Vector Customer)
        -> L.foldl' process (Map.empty, 0) (V.toList $ V.drop 2 records)

 where

  process :: FoldType (Customers, Int) Customer
  process (map1, ce) c 
      = let cId = c_clientID c
        in  case cId == "#N/A" of
              True -> (map1, ce + 1)
              False -> case Map.lookup cId map1 of
                         Just c2
                           -> case (c_iban c == c_iban c2 && c_bic c == c_bic c2) of
                                True -> (map1, ce)
                                False -> error $ "Repeated customer with id " ++ (Text.unpack cId)
                         Nothing
                           -> (Map.insert cId c map1, ce)

-- | Show customers having a repeated ClientID
showRepeatedCustomers1 :: LB.ByteString -> LB.ByteString
showRepeatedCustomers1 content
  = case decodeWith csvDelimeter1 NoHeader content of
      Left err -> error err
      Right (records1::V.Vector Customer)
        -> let records2 = V.drop 2 records1
               ids1 = countIDS records2
               ids2 = Map.keysSet $ Map.filter (> 1) ids1
           in showIDS ids2 records2 

 where

  sortC :: Customer -> Customer -> Ordering
  sortC c1 c2 = compare (c_clientID c1) (c_clientID c2)

  showIDS :: Set ClientID -> V.Vector Customer -> LB.ByteString
  showIDS ids customers
    = encode $ L.sortBy sortC $ L.filter (\c -> Set.member (c_clientID c) ids) $ V.toList customers 

  countIDS :: V.Vector Customer -> Map ClientID Int
  countIDS records = L.foldl' process Map.empty (V.toList records)

  process :: FoldType (Map ClientID Int) Customer
  process m1 c
      = let cId = c_clientID c
        in Map.insertWith (+) cId 1 m1

data CDids = CDids {
      cdids_clientId :: !Text
    , cdids_priceList :: !Text
    , cdids_did :: !Text
    } deriving(Show, Generic)

instance FromRecord CDids

instance ToRecord CDids

-- | Return correct customer DIDS, and DIDS with errors because
--   repeated with different price category or customer.
parseDIDS1 :: LB.ByteString -> (CustomerDIDS, Set DID)
parseDIDS1 content
  = case decodeWith csvDelimeter1 NoHeader content of
      Left err
          -> error err
      Right (records::V.Vector CDids)
        -> L.foldl' groupByDID (Map.empty, Set.empty) (V.toList $ V.tail records)

 where

  isNotUsedRateID :: Text -> Bool 
  isNotUsedRateID t = t == "59" || t == "60" || t == "61"
                      -- according https://git.asterisell.com/customer/consertis/issues/20
                      -- CDRs from Colt will NEVER use 59+60 or 61.

  groupByDID :: (CustomerDIDS, Set DID) -> CDids -> (CustomerDIDS, Set DID)
  groupByDID (m1, s1) c 
     = let did = Text.concat ["00", did']
           did' = cdids_did c
           customerID = cdids_clientId c
           rateID = cdids_priceList c
       in case Map.lookup did m1 of
            Just (customerID', rateID')
               -> case (customerID' == customerID && rateID' == rateID) of
                    True
                        -> (m1, s1)
                           -- it is the same data
                    False
                        -> case (isNotUsedRateID rateID', isNotUsedRateID rateID) of
                             (True, _)
                               -> (Map.insert did  (customerID, rateID) m1, s1)
                                  -- replace a not used rate id, with a maybe more important rate
                             (False, True)
                               -> (m1, s1)
                                  -- leave the more important rate
                             (False, False)
                               -> (Map.delete did m1, Set.insert did s1)
                                  -- they are both important price lists

            Nothing
              -> (Map.insert did (customerID, rateID) m1, s1)


-- | Tries to match Customer DIDS with CDRS DIDS.
--
-- SPECIFICATION:
-- Every customer has one or more DIDs attached to his Client_ID. You can safely look for the string Cust_DID+* . meaning asterisk after the DID because it MIGHT be that he uses extensions. This string will NEVER result in a DID which another customer have it is impossible to produce an error this way. F.e.: DID in Customer DB: 0043 1 59991 you look for 4315991* and therefore catch all extensions like 43159991216 No other customer will have a number like 431599915. impossible. because client has the block. So it should be rather easy.
--
matchCustomerDIDS
    :: CustomerDIDS
    -> Set DID
       -- ^ CDRS DIDS
    -> (CustomerDIDS
        -- ^ the transformed CustomerDIDS with added `*` suffix when appropiate
       , Set DID
         -- ^ CustomerDIDS with conflicts after the transformation
       , Set DID
         -- ^ CDRS DIDS without a valid match
       )
matchCustomerDIDS custDIDS cdrDIDS
  = let 

        -- | Add new DIDS.
        --   Requires that they are ordered from shorter to longer.
        transformDID :: FoldType (CustomerDIDS, Set DID, Trie (ClientID, RateID)) (DID, (ClientID, RateID))
        transformDID (map1, conflictDIDS1, trie1) (did, (clientId, rateId))
          =
            let did' = Text.concat [did, "*"]

                (conflictDIDS2, isCompatibleDID, addDID)
                  = case trie_lookup trie1 (Text.unpack did) of
                      Nothing
                        -> (conflictDIDS1, True, True)
                      Just (clientId2, rateId2)
                        -> case (clientId2 == clientId && rateId2 == rateId) of
                             True -> (conflictDIDS1, True, False)
                             False -> (Set.insert did conflictDIDS1, False, False)

                trie2 = case addDID of
                          True -> fromJust $ trie_addUniqueExtensionCode trie1 (Text.unpack did') (clientId, rateId)
                          False -> trie1

                map2 = case addDID of
                         True -> Map.insert did' (clientId, rateId) map1
                         False -> map1

            in (map2, conflictDIDS2, trie2)

        (resultCustDIDS, resultConflictDIDS, resultTrie)
            = L.foldl' transformDID (Map.empty, Set.empty, trie_empty) (Map.toAscList custDIDS)

        resultCDRSDidsWithoutMatch 
          = Set.filter
                (\d -> case trie_getMatch 0 resultTrie (Text.unpack d) of
                         Just _ -> False
                         Nothing -> True
                ) cdrDIDS

    in (resultCustDIDS, resultConflictDIDS, resultCDRSDidsWithoutMatch)

type CustomerInfo = Map ClientID (Customer, [(RateID, DID)])

unspecifiedID :: Text
unspecifiedID :: Text = "unspecifid" 

-- | Report in a separate Set the Customer with DIDS, but undefinied,
--   and associate them in the `CustomerInfo` to the `unspecifID` customer.
customerDIDSToInfo :: Customers -> CustomerDIDS -> (CustomerInfo, Set ClientID)
customerDIDSToInfo customers dids
    = let 
          unspecifiedCustomer
              = Customer {
                  c_clientID =  unspecifiedID
                , c_mr = ""
                , c_title = ""
                , c_name = ""
                , c_surname = ""
                , c_company = unspecifiedID
                , c_address = ""
                , c_zipCode = ""
                , c_city = ""
                , c_bic = ""
                , c_iban = ""
                , c_email = ""
                }

          f m1 did (clientId', rateId)
              = let (clientId, customer)
                      = case Map.lookup clientId' customers of
                          Nothing -> (unspecifiedID, unspecifiedCustomer)
                          Just c -> (clientId', c)
                in Map.insertWith (\(c', m') (c'', m'') -> (c', m' ++ m'')) clientId (customer, [(rateId, did)]) m1

          didHasCustomer (did, (clientId, _))
              = isJust $ Map.lookup clientId customers

      in  (Map.foldlWithKey f Map.empty dids
          ,Set.fromList $ L.map (\(_, (x,_)) -> x) $ L.filter (\d -> not (didHasCustomer d)) $ Map.toList dids
          )

-- | Show DIDS with problems in a better way. 
showDIDSWithMissingCustomers1 :: Set ClientID -> CustomerDIDS -> LB.ByteString
showDIDSWithMissingCustomers1 missingClients dids 
  = let dids2 = Map.filter (\(cId, _) -> Set.member cId missingClients) dids
        dids3 :: [CDids]
        dids3 = L.map (\(d, (c, r)) -> CDids c r d) $ Map.toAscList dids2
    in encode dids3

customerYAMLDef :: Customers -> CustomerDIDS -> String
customerYAMLDef customers customerDIDS
    = let (customerInfo, missingClientIDS) = customerDIDSToInfo customers customerDIDS

          customerPaymentTags c
              = case Text.length (c_iban c) of
                  0 -> "[banktransfer]"
                  _ -> "[sepa_debit]"

          yquote :: Text.Text -> Text.Text
          yquote t1 = Text.concat ["\"", Text.replace "\"" "\\\"" t1, "\""] 

          convCustomer indentLevel (customerId, (customer, dids@((firstRateId, _):_)))
              = let ni = indentLevel + 2
                    nni = ni + 1
                in addS indentLevel "-" ""
                     ++ addS (indentLevel + 1) "part:" ""
                     ++ addS ni "id:" "new"
                     ++ addS ni "from_date:" "2016-01-01 00:00:00"
                     ++ addS ni "internal_name:" customerId
                     ++ addS ni "export_code:" ""
                     ++ addS ni "type:" "Customer"
                     ++ addS ni "price_category:" (Text.concat ["r-", firstRateId])
                     ++ addS ni "to_disable:" "false"
                     ++ addS ni "party:" ""
                     ++ addS nni "name:" (yquote (customer_completeName customer))
                     ++ addS nni "short_name:" ""
                     ++ addS nni "is_active:" "true"
                     ++ addS nni "is_billable:" "true"
                     ++ addS nni "tags:" (customerPaymentTags customer)
                     ++ addS nni "email:" (c_email customer)
                     ++ addS nni "telephone1:" ""
                     ++ addS nni "telephone2:" ""
                     ++ addS nni "reseller_short_code:" ""
                     ++ addS nni "VAT:" ""
                     ++ addS nni "crm_code:" (c_clientID customer)
                     ++ addS nni "country:" "Austria"
                     ++ addS nni "state_province:" ""
                     ++ addS nni "city:" (c_city customer)
                     ++ addS nni "zip_code:" (c_zipCode customer)
                     ++ addS nni "address:" (yquote (c_address customer))
                     ++ addS nni "credit_limit:" ""
                     ++ addS nni "telephonic_service_migration_field:" "" 
                     ++ addS nni "internet_service_migration_field:" ""
                     ++ addS nni "payment_iban:" (c_iban customer)
                     ++ addS nni "payment_bic:" (c_bic customer)
                     ++ addS nni "payment_info:" ""
                     ++ addS ni "parts:" ""
                     ++ (L.concatMap (convDids nni) dids)

          convDids ni (rateId, did)
            = let nni = ni + 3
                  nnni = nni + 1

              in addS (ni + 1) "-" ""
                       ++ addS (ni + 2) "part:" ""
                       ++ addS nni "id:" "new"
                       ++ addS nni "from_date:" "2016-01-01 00:00:00"
                       ++ addS nni "internal_name:" "" 
                       ++ addS nni "export_code:" "" 
                       ++ addS nni "type:" "Extension"
                       ++ addS nni "price_category:" (Text.concat ["r-", rateId])
                       ++ addS nni "to_disable:" "false"
                       ++ addS nni "extension:" ""
                       ++ addS nnni "name:" ""
                       ++ addS nnni "short_name:" ""
                       ++ addS nnni "codes:" did

          addS :: Int -> Text -> Text -> String
          addS indentLevel fieldName fieldValue = (L.concat $ L.replicate indentLevel "  ") ++ (Text.unpack fieldName) ++ " " ++ (Text.unpack fieldValue) ++ "\n"

      in  addS 0 "parent_id:" "null"
          ++ addS 0 "parts:" ""
          ++ (L.concatMap (convCustomer 1) (Map.toAscList customerInfo))

--
-- Utility Functions
--

fromTextToRational :: Text.Text -> Maybe Rational
fromTextToRational t
  = case Text.rational t of
      Right (r, "") -> Just r
      _ -> Nothing

-- from http://stackoverflow.com/a/40694661
fromRationalToString :: (Integral i, Show i) => Int -> Ratio i -> String
fromRationalToString len rat
  = withoutDecimalDigits $ (if num < 0 then "-" else "") ++ show ip ++ "." ++ L.take len (go (abs num - ip * den))
 where
    withoutDecimalDigits [] = []
    withoutDecimalDigits n = if L.last n == '.' then L.init n else n
   
    num = numerator rat
    den = denominator rat
    ip  = abs num `quot` den

    go 0 = ""
    go x = shows d (go next)
      where
        (d, next) = (10 * x) `quotRem` den

showSet :: (Show a) => Set a -> String
showSet s = "\n" ++ Set.foldl (\r x -> r ++ show x ++ "\n") "" s

--
-- Tries
--

-- | A character that can be matched.
--
data CharMatch
  = CharMatch_Char Char
  | CharMatch_AnyChar
  -- ^ the "X" symbol matching exactly one (any) char
  | CharMatch_AllChars
  -- ^ match zero or more chars.
  -- NOTE: strings without "*" are an exact match, and not a prefix match.
 deriving(Eq, Ord)

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
  = L.foldl' f initialTrie codes
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
  = L.foldl' f initialTrie codes
 where

   f t (c, v) = trie_addExtensionCodeAsText t c v

type CharMatchStrenght = Int

-- | The matching value inside the trie.
--   Used usually for matching a telephone number with the best matching entry,
--   or for matching an extension with an organization.
--
--   Note that:
--     * there must be a value associated to a valid trie entry, otherwise there is no match.
--     * the user input "X*" is mapped to "X" and "*" because it must match first exactly one char, then zero or more chars. 
--     * "3*" has the value associated to "*", and not "3".
--     * "3X" has the value associated to "X".
trie_getMatch :: CharMatchStrenght -> Trie a -> [Char] -> Maybe (CharMatchStrenght, a)

trie_getMatch s (Trie (Just value) _) []
  = Just (s, value)

trie_getMatch s (Trie Nothing tries) []
  = case Map.lookup (CharMatch_AllChars) tries of
      Just (Trie (Just r) _) -> Just (s, r)
      Just (Trie Nothing _) -> Nothing
      Nothing -> Nothing

trie_getMatch s (Trie _ tries) (c:r)
  = let path1 
          = case Map.lookup (CharMatch_Char c) tries of
              Just nextTrie
                -> trie_getMatch (s + 1) nextTrie r
                   -- NOTE: this can be Nothing in the end if the path has no success.
              Nothing
                -> Nothing

        path2 
          = case Map.lookup (CharMatch_AnyChar) tries of
              Just nextTrie
                -> trie_getMatch (s + 1) nextTrie r
              Nothing
                -> case Map.lookup (CharMatch_AllChars) tries of
                     Just (Trie Nothing _)
                       -> Nothing
                     Just (Trie (Just value) _)
                       -> Just (s, value)
                     Nothing
                       -> Nothing

        bestPath Nothing Nothing = Nothing
        bestPath Nothing (Just r) = Just r
        bestPath (Just r) Nothing = Just r
        bestPath (Just (s1, value1)) (Just (s2, value2)) 
          = case s1 >= s2 of
              True -> Just (s1, value1)
                      -- NOTE: in case of same lenght favour always the path with the exact digit
              False -> Just (s2, value2)

     in bestPath path1 path2


--
-- Match DIDS
--

main = do
  let resultFileName = "info.txt"
  IO.putStrLn $ "\n== Write results on " ++ resultFileName
  outH <- IO.openFile resultFileName WriteMode
  hSetEncoding outH utf8_bom

  IO.hPutStrLn outH "\n== Parse DIDS in CDRS"
  let fileName1 = "COLT_122016.csv"
  fileContent1 <- LB.readFile fileName1
  let !cdrDIDS  =  parseColtCDRS fileContent1

  IO.hPutStrLn outH ("\n== DIDS in CDRS of December are " ++ show (Set.size cdrDIDS))

  IO.hPutStrLn outH ("\n== Parse Customer DIDS")
  let fileName2 = "dids.csv"
  fileContent2 <- LB.readFile fileName2
  let (!customerDIDS, !badDIDS) = parseDIDS1 fileContent2

  IO.hPutStrLn outH ("\n== Parse Customer Info")
  let fileName3 = "customers.csv"
  fileContent3 <- LB.readFile fileName3
  let (!customersInfo, !badCustomers) = parseCustomers1 fileContent3

  let (_, !missingClientIDS) = customerDIDSToInfo customersInfo customerDIDS
  IO.hPutStrLn outH ("\n==!! Customers IDS specified in DID table, but unspecified in customer table, are " ++ show (Set.size missingClientIDS)  ++ "\n")
  LB.hPut outH (showDIDSWithMissingCustomers1 missingClientIDS customerDIDS)
  LB.hPut outH "\n"

  IO.hPutStrLn outH ("\n==!! Customers with a bad format are " ++ show badCustomers)

  IO.hPutStrLn outH ("\n== Correct Customer DIDS are: " ++ show (Map.size customerDIDS))
  IO.hPutStrLn outH ("\n==!! Repeated DIDS with different priceID are " ++ (show (Set.size badDIDS)) ++ "\n" ++ (showSet badDIDS))

  let (!customerDIDS2, !didsWithConflict, !cdrDidsWithNoMatch)
          = matchCustomerDIDS customerDIDS cdrDIDS

  IO.hPutStrLn outH ("\n== Matched Customer DIDS are " ++ show (Map.size customerDIDS2) ++ (showSet $ Map.keysSet customerDIDS2))
  IO.hPutStrLn outH ("\n==!! Customer DIDS with conflict are " ++ (show $ Set.size didsWithConflict) ++ "\n" ++ (showSet didsWithConflict))
  IO.hPutStrLn outH ("\n==!! CDR DIDS without a match are " ++ (show $ Set.size cdrDidsWithNoMatch) ++ "\n" ++ (showSet cdrDidsWithNoMatch))

  let fileName3 = "customers.yaml"
  handle3 <- IO.openFile fileName3 WriteMode
  hSetEncoding handle3 utf8_bom
  IO.hPutStrLn outH ("\n== Created Cutomer Export file " ++ fileName3)
  IO.hPutStr handle3 (customerYAMLDef customersInfo customerDIDS2)
  hClose handle3

  return ()



