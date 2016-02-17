-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2014-2016 Richard Lewis, Goldsmiths' College
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

module Sound.Audio.Features.ReadCSV ( readCSVFeaturesTimesPowers
                                    , readCSVFeaturesPowers
                                    , readCSVFeaturesTimes
                                    , readCSVFeaturesOnly ) where

import           AudioDB.API
import           Data.CSV (csvFile)
import qualified Data.Vector.Storable as DV
import           Sound.Audio.Features
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.String (parseFromFile)

readCSVFeaturesTimesPowers :: String -> FilePath -> FilePath -> IO (Maybe ADBDatum)
readCSVFeaturesPowers      :: String -> FilePath -> FilePath -> IO (Maybe ADBDatum)
readCSVFeaturesTimes       :: String -> FilePath -> IO (Maybe ADBDatum)
readCSVFeaturesOnly        :: String -> FilePath -> IO (Maybe ADBDatum)

readCSVFeaturesTimesPowers key featuresFile powersFile = readFeaturesFile key featuresFile parseCSVFeaturesWithTimesFile    (Just (powersFile, parseCSVPowerFeaturesFile))
readCSVFeaturesPowers key featuresFile powersFile      = readFeaturesFile key featuresFile parseCSVFeaturesWithoutTimesFile (Just (powersFile, parseCSVPowerFeaturesFile))
readCSVFeaturesTimes key featuresFile                  = readFeaturesFile key featuresFile parseCSVFeaturesWithTimesFile    Nothing
readCSVFeaturesOnly key featuresFile                   = readFeaturesFile key featuresFile parseCSVFeaturesWithoutTimesFile Nothing

dblHead :: [String] -> Maybe (Double, [String])
dblHead (d:ds) = (Just (readDbl d, ds))
dblHead [] = Nothing

ensureTable :: (Either ParseError [[a]]) -> [[a]]
ensureTable (Left _) = [[]]
ensureTable (Right fs) = fs

takeColumns :: Int -> [[a]] -> [[a]]
takeColumns n (x:xs) = take n x : takeColumns n xs
takeColumns _ [] = []

dropColumns :: Int -> [[a]] -> [[a]]
dropColumns n (x:xs) = drop n x : dropColumns n xs
dropColumns _ [] = []

flattenTakeCols :: Int -> [[a]] -> [a]
flattenTakeCols n = concat . (takeColumns n)

flattenDropCols :: Int -> [[a]] -> [a]
flattenDropCols n = concat . (dropColumns n)

parseCSVFeaturesWithTimesFile :: FeaturesParser
parseCSVFeaturesWithTimesFile fp = do
  o <- parseFromFile csvFile fp
  let features = ensureTable o
      fVec     = DV.unfoldr dblHead (flattenDropCols 1 features)
      tVec     = DV.unfoldr dblHead (flattenTakeCols 1 features)
      nVectors = (length (head features)) - 1

  return (DV.length fVec `div` nVectors, nVectors, fVec, Just tVec)

parseCSVFeaturesWithoutTimesFile :: FeaturesParser
parseCSVFeaturesWithoutTimesFile fp = do
  o <- parseFromFile csvFile fp
  let features = ensureTable o
      fVec     = DV.unfoldr dblHead (concat features)
      nVectors = (length (head features))

  return (DV.length fVec `div` nVectors, nVectors, fVec, Nothing)

parseCSVPowerFeaturesFile :: PowerFeaturesParser
parseCSVPowerFeaturesFile fp = do
  o <- parseFromFile csvFile fp
  let features = ensureTable o
      fVec     = DV.unfoldr dblHead (concat features)

  return (Just fVec)
