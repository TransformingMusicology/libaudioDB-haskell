-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College
-- Author: richard.lewis@gold.ac.uk

-- This file is part of libaudioDB-haskell.

-- libaudioDB-haskell is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- libaudioDB-haskell is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with libaudioDB-haskell. If not, see <http://www.gnu.org/licenses/>.

module Main where

import qualified Data.Vector.Storable as V
import           Sound.Audio.Database
import           Sound.Audio.Database.Ingest
import           Sound.Audio.Database.Query
import           Sound.Audio.Database.Types
import           Test.HUnit

test0003 :: Test
test0003 = TestCase $ do
  let
    dbDim = 1
    datum =
      ADBDatum { datum_nvectors = 1
               , datum_dim      = dbDim
               , datum_key      = "Test"
               , datum_data     = V.fromList [1.0]
               , datum_power    = Nothing
               , datum_times    = Nothing }
    expRes =
      ADBResult { result_qkey = "Test"
                , result_ikey = "Test"
                , result_qpos = 0
                , result_ipos = 0
                , result_dist = 1.0 }

    testDB Nothing    = assertFailure "Database 0003.adb was not created."
    testDB (Just adb) = do
      ins     <- insertFeatures adb datum
      assertBool "1D insertion" ins
      results <- querySinglePass adb $ mkPointQuery datum inFrames inSeconds 10
      let actRes = head (query_results_results results)
      assertEqual "1D point query" expRes actRes

  withNewL2NormedAudioDB "0003.adb" 0 0 dbDim testDB

test0004 :: Test
test0004 = TestCase $ do
  let
    dbDim = 2
    datum =
      ADBDatum { datum_nvectors = 2
               , datum_dim      = dbDim
               , datum_key      = "Test"
               , datum_data     = V.fromList [0.0, 1.0, 1.0, 0.0]
               , datum_power    = Nothing
               , datum_times    = Nothing }
    query05 =
      ADBDatum { datum_nvectors = 1
               , datum_dim      = dbDim
               , datum_key      = "Query05"
               , datum_data     = V.fromList [0.0, 0.5]
               , datum_power    = Nothing
               , datum_times    = Nothing }
    query50 =
      ADBDatum { datum_nvectors = 1
               , datum_dim      = dbDim
               , datum_key      = "Query50"
               , datum_data     = V.fromList [0.5, 0.0]
               , datum_power    = Nothing
               , datum_times    = Nothing }
    expResQ05np10 =
      ADBQueryResults { query_results_nresults = 2
                      , query_results_results  = [ ADBResult { result_qkey = "Query05"
                                                             , result_ikey = "Test"
                                                             , result_qpos = 0
                                                             , result_ipos = 0
                                                             , result_dist = 0.5 }
                                                 , ADBResult { result_qkey = "Query05"
                                                             , result_ikey = "Test"
                                                             , result_qpos = 0
                                                             , result_ipos = 1
                                                             , result_dist = 0.0 } ] }
    expResQ05np1 =
      ADBQueryResults { query_results_nresults = 1
                      , query_results_results  = [ ADBResult { result_qkey = "Query05"
                                                             , result_ikey = "Test"
                                                             , result_qpos = 0
                                                             , result_ipos = 0
                                                             , result_dist = 0.5 } ] }
    expResQ50np10 =
      ADBQueryResults { query_results_nresults = 2
                      , query_results_results  = [ ADBResult { result_qkey = "Query50"
                                                             , result_ikey = "Test"
                                                             , result_qpos = 0
                                                             , result_ipos = 1
                                                             , result_dist = 0.5 }
                                                 , ADBResult { result_qkey = "Query50"
                                                             , result_ikey = "Test"
                                                             , result_qpos = 0
                                                             , result_ipos = 0
                                                             , result_dist = 0.0 } ] }
    expResQ50np1 =
      ADBQueryResults { query_results_nresults = 1
                      , query_results_results  = [ ADBResult { result_qkey = "Query50"
                                                             , result_ikey = "Test"
                                                             , result_qpos = 0
                                                             , result_ipos = 1
                                                             , result_dist = 0.5 } ] }

    testDB Nothing    = assertFailure "Database 0004.adb was not created."
    testDB (Just adb) = do
      ins     <- insertFeatures adb datum
      assertBool "1D insertion" ins

      actResQ05np10 <- querySinglePass adb $ mkPointQuery query05 inFrames inSeconds 10
      assertEqual "point query: query [0.0, 0.5], np 10" expResQ05np10 (reverseResults actResQ05np10)

      actResQ05np1  <- querySinglePass adb $ mkPointQuery query05 inFrames inSeconds 1
      assertEqual "point query: query [0.0, 0.5], np 1" expResQ05np1 (reverseResults actResQ05np1)

      actResQ50np10 <- querySinglePass adb $ mkPointQuery query50 inFrames inSeconds 10
      assertEqual "point query: query [0.5, 0.0], np 10" expResQ50np10 (reverseResults actResQ50np10)

      actResQ50np1  <- querySinglePass adb $ mkPointQuery query50 inFrames inSeconds 1
      assertEqual "point query: query [0.5, 0.0], np 1" expResQ50np1 (reverseResults actResQ50np1)

  withNewL2NormedAudioDB "0004.adb" 0 0 dbDim testDB

main :: IO ()
main = do
  _ <- runTestTT $ TestList [test0003, test0004]
  return ()
