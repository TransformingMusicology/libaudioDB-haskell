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

module Sound.Audio.Features ( DatumProperties
                            , FeaturesParser
                            , PowerFeaturesParser
                            , datumSlice
                            , readFeaturesFile
                            , readDbl ) where

import           AudioDB.API
import qualified Data.Vector.Storable as DV
import           Sound.Audio.Database.Types

type DatumProperties     = (Int, Int, DV.Vector Double, Maybe (DV.Vector Double))
type FeaturesParser      = (FilePath -> IO DatumProperties)
type PowerFeaturesParser = (FilePath -> IO (Maybe (DV.Vector Double)))

readFeaturesFile :: String                         -- key
                    -> FilePath                    -- features file
                    -> FeaturesParser              -- features parser
                    -> Maybe (FilePath,
                              PowerFeaturesParser) -- power features file, parser
                    -> IO (Maybe ADBDatum)
readFeaturesFile key featuresFile featuresParser (Just (powersFile, powersParser)) = do
  (n, dim, features, times) <- featuresParser featuresFile
  pFeatures                 <- powersParser powersFile
  let power = pFeatures
      datum = ADBDatum { datum_nvectors = n
                       , datum_dim      = dim
                       , datum_key      = key
                       , datum_data     = features
                       , datum_power    = power
                       , datum_times    = times }
  return (Just datum)

readFeaturesFile key featuresFile featuresParser Nothing = do
  (n, dim, features, times) <- featuresParser featuresFile
  let datum = ADBDatum { datum_nvectors = n
                       , datum_dim      = dim
                       , datum_key      = key
                       , datum_data     = features
                       , datum_power    = Nothing
                       , datum_times    = times }
  return (Just datum)

readDbl :: String -> Double
readDbl d = case d of
  "inf"       -> (read "Infinity")  :: Double
  "Inf"       -> (read "Infinity")  :: Double
  "infinity"  -> (read "Infinity")  :: Double
  "-inf"      -> (read "-Infinity") :: Double
  "-Inf"      -> (read "-Infinity") :: Double
  "-infinity" -> (read "-Infinity") :: Double
  "nan"       -> (read "NaN")       :: Double
  otherwise   -> (read d)           :: Double

datumSlice :: ADBDatum -> FeatureRate -> Seconds -> Seconds -> Either QueryException ADBDatum
datumSlice datum secsToFrames start len
  | secsToFrames start + secsToFrames len > (datum_nvectors datum) =
    Left $ QuerySequenceBoundsException (secsToFrames start) (secsToFrames len) (datum_nvectors datum)
  | otherwise =
    Right $ ADBDatum { datum_nvectors = nVec
                     , datum_dim      = datum_dim datum
                     , datum_key      = key
                     , datum_data     = getSlice (datum_dim datum) (datum_data datum)
                     , datum_power    = getMaybeSlice 1 (datum_power datum)
                     , datum_times    = getMaybeSlice 1 (datum_times datum) }
  where
    getSlice d v = DV.slice (d * secsToFrames start) (d * secsToFrames len) v
    getMaybeSlice _ Nothing  = Nothing
    getMaybeSlice d (Just v) = Just $ getSlice d v

    nVec = DV.length (getSlice (datum_dim datum) (datum_data datum)) `div` (datum_dim datum)

    key = (datum_key datum) ++ "[" ++ show start ++ "->" ++ show (start + len) ++ "]"
