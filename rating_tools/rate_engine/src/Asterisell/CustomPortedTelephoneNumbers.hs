{-# LANGUAGE ScopedTypeVariables, BangPatterns, OverloadedStrings, QuasiQuotes, DeriveGeneric, DeriveAnyClass  #-}

-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>


-- | Import "ported telephone numbers" from different customer-specific sources.
--   The processing workflow is:
--   * this functions complete the info on the ported telephone number table
--   * the CDR importer assign an initial external-telephone number to use for rating and an optional displayed telephone number
--   * the CDR rating engine use the ported telephone numbers table for converting to a ported telephone number
--   * the CDR rating engine rate the telephone number
module Asterisell.CustomPortedTelephoneNumbers (
  rolf1_processTelcordia
) where

import Asterisell.DB
import Asterisell.Error
import Asterisell.Utils
import Asterisell.Trie
import Asterisell.TelephonePrefixes
import Asterisell.VoIPChannelAndVendor
import Asterisell.RateCategories
import Asterisell.OrganizationHierarchy
import Asterisell.Cdr
import Asterisell.RateEngine

import Data.List as L
import qualified Database.MySQL.Base as DB
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Conversion as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Compression.GZip as GZip
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Maybe
import Data.IORef
import Control.Monad
import Data.Time.LocalTime
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Except (ExceptT, throwError, lift, runExceptT)

import GHC.Generics
import Control.DeepSeq
import Control.Exception.Safe (catch, catchAny, onException, finally, handleAny, bracket
                              , SomeException(..), throwIO, throw, Exception, MonadMask
                              , withException, displayException)

import qualified Xeno.SAX as XML

-- ---------------------------------------
-- Process Telcordia XML files
-- according the needs of Rolf1 project #9

type OperatorDefs
       = [(BS.ByteString
           -- ^ operator human readable name
          , BS.ByteString
           -- ^ operator code to use as prefix of ported/processed telephone numbers
          , [BS.ByteString]
          -- ^ telephone prefixes used from operators
          )]

-- | A telephone number with explicit operator.
zaf_telephoneNumberWithOperator
  :: BS.ByteString
  -- ^ operator code
  -> BS.ByteString
  -- ^ telephone number
  -> BS.ByteString
zaf_telephoneNumberWithOperator opCode tn = BS.append opCode tn

data Rolf1XMLState
       = RXML_Ignore
       | RXML_Activated
       | RXML_IDNumber
       | RXML_MSISDN
       | RXML_Route
       | RXML_Action
       | RXML_RangeFrom
       | RXML_RangeTo
     deriving(Eq, Ord, Show)

fromXMLTagToXMLState :: BS.ByteString -> IO Rolf1XMLState
fromXMLTagToXMLState bs
  = let m = Map.fromList
              [("IDNumber", RXML_IDNumber)
              ,("MSISDN", RXML_MSISDN)
              ,("RNORoute", RXML_Route)
              ,("Action", RXML_Action)
              ,("CRDBData", RXML_Ignore)
              ,("MessageName", RXML_Ignore)
              ,("NumberOfMessages", RXML_Ignore)
              ,("Comments1", RXML_Ignore)
              ,("Comments2", RXML_Ignore)
              ,("ActivatedNumbers", RXML_Activated)
              ,("ActivatedNumber", RXML_Ignore)
              ,("DNRanges", RXML_Ignore)
              ,("DNFrom", RXML_RangeFrom)
              ,("DNTo", RXML_RangeTo)
              ,("?xml", RXML_Ignore)
              ]
    in case Map.lookup bs m of
         Just r -> return r
         Nothing -> throwIO $ AsterisellException $ "Unexpected tag " ++ fromByteStringToString bs

data Rolf1State
       = Rolf1State {
           rs_insertStmtId :: DB.StmtID
         , rs_xml_activated :: Bool
         , rs_countInserts :: Int
         , rs_garbageKey :: BS.ByteString
         , rs_xml_state :: Rolf1XMLState
         , rs_xml_portedFromDate :: LocalTime
         , rs_xml_src :: BS.ByteString
         , rs_xml_operator :: BS.ByteString
         , rs_xml_range_to :: BS.ByteString
                    }

rolf1_processTelcordia
  :: DBConf
  -> String
  -- ^ file name to process
  -> IO Int
  -- ^ return the number of inserted ported telephone numbers

rolf1_processTelcordia dbConf fileName = do
   let state1
         = Rolf1State {
             rs_insertStmtId = fromIntegral 0
           , rs_countInserts = fromIntegral 0
           , rs_garbageKey = BS.concat
                               [ fromStringToByteString "rolf1_processTelcordia "
                               , fromStringToByteString fileName]
           , rs_xml_activated = False
           , rs_xml_portedFromDate = fromJust $ fromMySQLDateTimeAsTextToLocalTime "2000-01-01 00:00:00"
           , rs_xml_src = ""
           , rs_xml_operator = ""
           , rs_xml_state = RXML_Ignore
           , rs_xml_range_to = ""
           }
   state1R <- newIORef state1
   withResource'
    (do conn <- db_openConnection dbConf False
        db_openTransaction conn
        return conn)
    (\conn -> do
       -- NOTE: up to date Xeno does not use Lazy ByteStrings
       xmlContent' <- BS.readFile fileName
       let xmlContent
             = case L.reverse $ L.take 3 $ L.reverse fileName of
                 ".gz" ->  LBS.toStrict $ GZip.decompress $ LBS.fromStrict xmlContent'
                 _ -> xmlContent'

       id1 <- DB.prepareStmt conn "CALL add_ported_telephone_number(?,?,?)"
       modifyIORef' state1R (\s -> s { rs_insertStmtId = id1
                                     })

       state1 <- readIORef state1R
       db_garbagePastErrors conn (rs_garbageKey state1) Nothing Nothing

       XML.process
         (openTagP conn state1R)
         (tagAttrP conn state1R)
         (closeTagAttrP conn state1R)
         (textP conn state1R)
         (closeTagP conn state1R)
         (cdataP conn state1R)
         xmlContent

       state2 <- readIORef state1R
       return $ rs_countInserts state2)
    (db_releaseResource)
    (\exc -> SomeException $ AsterisellException $ "Unrecoverable error during importing of Telecordia ported telephone numbers, from file " ++ fileName  ++ ". Exception: " ++ displayException exc)

 where

   openTagP :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> IO ()
   openTagP conn sr tagName
      = do ts <- fromXMLTagToXMLState tagName
           modifyIORef' sr (\s -> s { rs_xml_state = ts })
           when (ts == RXML_Activated) (modifyIORef' sr (\s -> s { rs_xml_activated = True}))
           -- NOTE activated has no text associated, so we process explicitely here.
           return ()

   tagAttrP :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> BS.ByteString -> IO ()
   tagAttrP conn sr attrName attrValue = return ()

   closeTagAttrP :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> IO ()
   closeTagAttrP conn sr tagName = return ()

   textP :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> IO ()
   textP conn sr bs = do
     s <- readIORef sr
     when (rs_xml_activated s &&  (not $ isEmptyXMLContent bs)) $ do

       case (rs_xml_state s) of
         RXML_Ignore
           -> do return ()

         RXML_Activated
           -> do modifyIORef' sr (\s1 -> s1 { rs_xml_activated = True})
                 return ()

         RXML_IDNumber
           ->
              -- something like "20180123021728..."
              let (yearS, r1) = BS.splitAt 4 bs
                  (monthS, r2) = BS.splitAt 2 r1
                  dayS = BS.take 2 r2
                  localTimeS = BS.concat [yearS, "-", monthS, "-", dayS, " 00:00:00"]
              in do localTime
                      <-case fromMySQLDateTimeAsTextToLocalTime (fromByteStringToText localTimeS) of
                          Just r -> return r
                          Nothing -> do
                                        tz <- getCurrentTimeZone
                                        now <- utcToLocalTime tz <$> getCurrentTime

                                        -- there are few rates that are malformed, so consider today as reference date
                                        _ <- dbErrors_insert
                                               conn
                                               Nothing
                                               (createError
                                                  Type_Warning
                                                  Domain_RATES
                                                  ("missing date for ported telephone number " ++ fromByteStringToString bs ++ " - " ++ show now)
                                                  ("The ported telephone number associated to id " ++ fromByteStringToString bs ++ ", inside file " ++ fileName ++ " has no date from which the number is ported.")
                                                  ("The number is considered ported from the current date of importing (" ++ show now ++ ")")
                                                  ("If the date is not correct, and the number was ported more than one time, then calls to this number (or range of numbers) can be rated wrongly.")
                                               )
                                               (Just (now, now))

                                        return now

                    modifyIORef' sr (\s1 -> s1 { rs_xml_portedFromDate = localTime })

         RXML_MSISDN
           -> do modifyIORef' sr (\s1 -> s1 { rs_xml_src = bs
                                            , rs_xml_range_to = bs})
                 return ()

         RXML_RangeFrom
           -> do modifyIORef' sr (\s1 -> s1 { rs_xml_src = bs
                                            , rs_xml_range_to = bs})
                 return ()

         RXML_RangeTo
           -> do modifyIORef' sr (\s1 -> s1 { rs_xml_range_to = bs})
                 return ()

         RXML_Route
           -> do modifyIORef' sr (\s1 -> s1 { rs_xml_operator = bs})
                 return ()

         RXML_Action
           -> -- every type of action (e.g. Port, PortBack, Reverse) port the telephone number to the indicated operator
              case rs_xml_src s == rs_xml_range_to s of
                True -> do  saveExportedTelephoneNumber conn sr (rs_xml_src s)
                False -> do n1 <- fromBSToInteger (rs_xml_src s)
                            n2 <- fromBSToInteger (rs_xml_range_to s)
                            let ml = 10000
                            -- NOTE: support 123XXXX like ranges
                            when (n2 - n1 > ml)
                                 (throwIO $
                                    AsterisellException $
                                      "From telephone number " ++ show n1
                                      ++ " to " ++ show n2
                                      ++ ", there are more than " ++ show ml ++ " numbers, "
                                      ++ " and this is not supported.")

                            let ns :: [BS.ByteString] = L.map fromLazyToStrictByteString $ L.map BSC.toByteString [n1 .. n2]
                            mapM_ (saveExportedTelephoneNumber conn sr) ns

   closeTagP :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> IO ()
   closeTagP conn sr tagName = return ()

   cdataP :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> IO ()
   cdataP conn sr bs = return ()

   fromBSToInteger :: BS.ByteString -> IO Integer
   fromBSToInteger bs = do
     let mn = BSC.fromByteString bs
     case mn of
       Just n -> return n
       Nothing -> throwIO $ AsterisellException $ "Error parsing number: " ++ fromByteStringToString bs

   saveExportedTelephoneNumber :: DB.MySQLConn -> IORef Rolf1State -> BS.ByteString -> IO ()
   saveExportedTelephoneNumber conn sr tn = do
     s <- readIORef sr
     _ <- DB.executeStmt
            conn
            (rs_insertStmtId s)
            [toDBByteString tn
            ,toDBByteString $ zaf_telephoneNumberWithOperator (rs_xml_operator s) tn
            ,toDBLocalTime $ rs_xml_portedFromDate s
            ]

     modifyIORef'
       sr
       (\s1 -> s1 { rs_countInserts = rs_countInserts s + 1
                  , rs_xml_state = RXML_Ignore
                  }
       )

     return ()

isEmptyXMLContent :: BS.ByteString -> Bool
isEmptyXMLContent bs = BS.all (\c -> c == c2w ' ' || c == c2w '\n' || c == c2w '\t' || c == c2w '\r') bs
