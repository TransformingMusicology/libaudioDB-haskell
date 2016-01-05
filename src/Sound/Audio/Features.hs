-- AudioDB - Haskell bindings to the libaudioDB audio search engine library
--
-- Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
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
                            , readFeaturesFile ) where

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
