{-# LANGUAGE ScopedTypeVariables #-}

-- | Quick hack for comparing the calls of v3 and v5 servers, with daily cached sums. 
--   Make sure that the files are in UTF8 format. 
--   Badly parsed files generates no warnings, but the library stop to parse them, and return partial results.
module Main where

import Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Monad
import Control.Applicative
import Data.Vector as V hiding((++))
import Data.Hashable
import Data.Maybe
import Data.Map as Map
import Data.Set as Set
import Data.List as List
import Data.Csv
import qualified Data.Csv.Streaming as S
import qualified Data.Foldable as S
import qualified Data.ByteString.Lazy as B

{-
The query for v3 is something like

SELECT ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate))), 
       ar_party.name, 
       cdr.destination_type,
       count(cdr.id), 
       sum(cdr.billsec), 
       sum(cdr.income), 
       sum(cdr.cost)
INTO OUTFILE '/tmp/exported_totals_v3.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM cdr 
INNER JOIN ar_asterisk_account ON cdr.ar_asterisk_account_id = ar_asterisk_account.id
INNER JOIN ar_office ON ar_office.id = ar_asterisk_account.ar_office_id
INNER JOIN ar_party ON ar_party.id = ar_office.ar_party_id
WHERE cdr.calldate > '2015-01-01'
GROUP BY YEAR(calldate), DAYOFYEAR(calldate), ar_party.id, cdr.destination_type;

The query for v5 is something like

CREATE TABLE temp_totals AS (
SELECT ANY_VALUE(MAKEDATE(YEAR(calldate), DAYOFYEAR(calldate))), 
       billable_ar_organization_unit_id, 
       ar_cdr.destination_type,
       count(ar_cdr.id), 
       sum(ar_cdr.billsec), 
       sum(ar_cdr.income), 
       sum(ar_cdr.cost)
FROM ar_cdr 
WHERE calldate > '2015-01-01'
GROUP BY YEAR(calldate), DAYOFYEAR(calldate), billable_ar_organization_unit_id, destination_type);

select
t.*,
t.billable_ar_organization_unit_id,
o.id,  
s.id,
s.ar_party_id,
p.name
INTO OUTFILE '/tmp/exported_totals_v5.csv'
FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"'
LINES TERMINATED BY '\n'
FROM temp_totals AS t 
INNER JOIN ar_organization_unit AS o ON o.id = t.billable_ar_organization_unit_id
INNER JOIN ar_organization_unit_has_structure AS s ON s.ar_organization_unit_id = o.id
INNER JOIN ar_party AS p ON p.id = s.ar_party_id
;

Then the produced files are converted to UTF8 format, using recode utility.

-}

type CallDate = String
type Customer = String
type Destination = Int

data Totals 
  = Totals { 
      totals_count :: Int
    , totals_billsec :: Int
    , totals_income :: Int
    , totals_cost :: Int
    } deriving(Show)

class GroupedLine g where
  gl_calldate :: g -> CallDate
  gl_customer :: g -> Customer
  gl_destination :: g -> Destination
  gl_count :: g -> Int
  gl_billsec :: g -> Int
  gl_income :: g -> Int
  gl_cost :: g -> Int

-- | Grouped Totals.
type GT = Map.Map (CallDate, Customer, Destination) Totals

data TV3
  = TV3 {
      tv3_calldate :: CallDate
    , tv3_customer :: Customer
    , tv3_destination :: Destination
    , tv3_count :: Int
    , tv3_billsec :: Int
    , tv3_income :: Int
    , tv3_cost :: Int
  } deriving(Show)

instance FromRecord TV3 where
     parseRecord v =
         let expectedCols = 7
         in case V.length v == expectedCols of
              True
                -> TV3 <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6
              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)

instance GroupedLine TV3 where
  gl_calldate = tv3_calldate
  gl_customer = tv3_customer
  gl_destination = tv3_destination
  gl_count = tv3_count
  gl_billsec = tv3_billsec
  gl_income = tv3_income
  gl_cost = tv3_cost


data TV5
  = TV5 {
      tv5_calldate :: CallDate
    , tv5_ignore1 :: Int
    , tv5_destination :: Int
    , tv5_count :: Int
    , tv5_billsec :: Int
    , tv5_income :: Int
    , tv5_cost :: Int
    , tv5_ignore2 :: Int
    , tv5_ignore3 :: Int
    , tv5_ignore4 :: Int
    , tv5_ignore5 :: Int
    , tv5_customer :: Customer
  } deriving(Show)

instance GroupedLine TV5 where
  gl_calldate = tv5_calldate
  gl_customer = tv5_customer
  gl_destination = tv5_destination
  gl_count = tv5_count
  gl_billsec = tv5_billsec
  gl_income = tv5_income
  gl_cost = tv5_cost


instance FromRecord TV5 where
     parseRecord v =
         let expectedCols = 12
         in case V.length v == expectedCols of
              True
                -> TV5 <$>
                     v .! 0<*>
                     v .! 1<*>
                     v .! 2<*>
                     v .! 3<*>
                     v .! 4<*>
                     v .! 5<*>
                     v .! 6<*>
                     v .! 7<*>
                     v .! 8<*>
                     v .! 9<*>
                     v .! 10<*>
                     v .! 11

              False
                -> fail $ "There are " ++ show (V.length v) ++ " columns instead of the expected " ++ (show expectedCols)


fromGroupedLineToGT 
  :: (GroupedLine g, S.Foldable gs) 
  => gs g
  -> JoinedCustomers
  -> GT
fromGroupedLineToGT rs jc
  = let addToMap m r 
          = let t = Totals (gl_count r) (gl_billsec r) (gl_income r) (gl_count r)
            in  Map.insertWith myAdd (gl_calldate r, normalizeCustomer jc (gl_customer r), gl_destination r) t m

        myAdd1 f t1 t2 = (f t1) + (f t2)
        myAdd t1 t2 = Totals (myAdd1 totals_count t1 t2) (myAdd1 totals_billsec t1 t2) (myAdd1 totals_income t1 t2) (myAdd1 totals_cost t1 t2) 
          
    in  S.foldl' addToMap Map.empty rs 

fromTV3ToGT :: String -> IO GT
fromTV3ToGT fileName
  = do c <- B.readFile fileName
       let rs :: S.Records TV3 = S.decode NoHeader c
       return $ fromGroupedLineToGT rs jc

fromTV5ToGT :: String -> IO GT
fromTV5ToGT fileName
  = do c <- B.readFile fileName
       let rs :: S.Records TV5 = S.decode NoHeader c
       return $ fromGroupedLineToGT rs jc

fileNameV3 = "exported_totals_v3.csv"
fileNameV5 = "exported_totals_v5.csv"
fileNameDiff = "exported_totals_diff.csv"

groupedCustomers :: GT -> Set.Set Customer
groupedCustomers gt
  = Set.fromList $ List.map (\(_, c, _) -> c) $ Map.keys gt 
 
missingCustomers :: GT -> GT -> Set.Set Customer
missingCustomers gt1 gt2
  = let cs1 = groupedCustomers gt1
        cs2 = groupedCustomers gt2
    in  Set.difference cs1 cs2

compareCustomers
  = do v3 <- fromTV3ToGT fileNameV3
       v5 <- fromTV5ToGT fileNameV5
       return $ show $ (missingCustomers v3 v5, missingCustomers v5 v3)


type JoinedCustomers = Map.Map Customer Customer 

-- | Link togheter two different customer names, saying they are the same.
jc :: JoinedCustomers
jc
  = let l = []
    in Map.fromList l

normalizeCustomer :: JoinedCustomers -> Customer -> Customer
normalizeCustomer jc c
  = case Map.lookup c jc of
      Nothing -> c
      Just c2 -> c2

compareGT :: GT -> GT -> GT
compareGT gt1 gt2
  = let unionF = myAdd 
        myAdd1 f t1 t2 = (f t1) - (f t2)
        myAdd t1 t2 = Totals (myAdd1 totals_count t1 t2) (myAdd1 totals_billsec t1 t2) (myAdd1 totals_income t1 t2) (myAdd1 totals_cost t1 t2) 
 
    in  Map.unionWith unionF gt1 gt2 

newtype GTL = GTL ((CallDate, Customer, Destination), Totals)

instance ToRecord GTL where
  toRecord (GTL ((callDate, customer, destination), totals))
    = record [toField callDate
             ,toField customer
             ,toField destination
             ,toField $ totals_count totals
             ,toField $ totals_billsec totals
             ,toField $ totals_income totals
             ,toField $ totals_cost totals
             ]  

compareGTToCSV :: GT -> GT -> B.ByteString
compareGTToCSV gt1 gt2
  = encode $ List.map GTL $ Map.toList $ compareGT gt1 gt2

main 
  = do v3 <- fromTV3ToGT fileNameV3
       v5 <- fromTV5ToGT fileNameV5
       B.writeFile fileNameDiff (compareGTToCSV v3 v5)
       putStrLn $ "Wrote " ++ fileNameDiff

