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

{-# LANGUAGE OverloadedStrings #-}

module Sound.Audio.Features.ReadTurtle ( readTurtleFeaturesTimesPowers
                                       , readTurtleFeaturesPowers
                                       , readTurtleFeaturesTimes
                                       , readTurtleFeaturesOnly ) where

import           Data.Text (unpack)
import           Data.Text.Lazy.IO (readFile)
import qualified Data.Vector.Storable as DV
import           Prelude hiding (readFile)
import           Sound.Audio.Database (ADBDatum(..))
import           Sound.Audio.Features (DatumProperties, readFeaturesFile)
import           Swish.RDF.Graph
import           Swish.RDF.Parser.N3
import           Swish.RDF.Parser.Turtle
import           Swish.RDF.Query
import           Swish.VarBinding (VarBinding(vbMap))

loadGraph :: FilePath -> IO (Either String RDFGraph)
loadGraph fp = readFile fp >>= return . parseTurtlefromText

readTurtleFeaturesTimesPowers :: String -> FilePath -> FilePath -> IO (Maybe ADBDatum)
readTurtleFeaturesPowers      :: String -> FilePath -> FilePath -> IO (Maybe ADBDatum)
readTurtleFeaturesTimes       :: String -> FilePath -> IO (Maybe ADBDatum)
readTurtleFeaturesOnly        :: String -> FilePath -> IO (Maybe ADBDatum)

readTurtleFeaturesTimesPowers key featuresFile powersFile =
  readFeaturesFile
    key
    featuresFile
    (\f -> loadGraph f >>= parseTurtleFeaturesWithTimesGraph)
    (Just (powersFile, (\f -> loadGraph f >>= parseTurtlePowerFeaturesGraph)))

readTurtleFeaturesPowers key featuresFile powersFile =
  readFeaturesFile
    key
    featuresFile
    (\f -> loadGraph f >>= parseTurtleFeaturesWithoutTimesGraph)
    (Just (powersFile, (\f -> loadGraph f >>= parseTurtlePowerFeaturesGraph)))

readTurtleFeaturesTimes key featuresFile =
  readFeaturesFile
    key
    featuresFile
    (\f -> loadGraph f >>= parseTurtleFeaturesWithTimesGraph)
    Nothing

readTurtleFeaturesOnly key featuresFile =
  readFeaturesFile
    key
    featuresFile
    (\f -> loadGraph f >>= parseTurtleFeaturesWithoutTimesGraph)
    Nothing

parseTurtleFeaturesWithTimesGraph :: (Either String RDFGraph) -> IO DatumProperties
parseTurtleFeaturesWithTimesGraph (Left x) = fail x
parseTurtleFeaturesWithTimesGraph (Right g) = fail "Sonic Annotator turtle files do not include times."

parseTurtleFeaturesWithoutTimesGraph :: (Either String RDFGraph) -> IO DatumProperties
parseTurtleFeaturesWithoutTimesGraph (Left x) = fail x
parseTurtleFeaturesWithoutTimesGraph (Right g) = return (nVectors, dimensions, features, Nothing)
  where
    dimensions = head ((map read $ words dim) :: [Int]) -- FIXME I only want one dimension value, but this serialisation seems to contain multiple numbers. What does that mean?
    features   = DV.fromList ((map read $ words value) :: [Double])
    nVectors   = (DV.length features) `div` dimensions

    dim            = firstDim dimBndgs
    firstDim []    = error "No af:dimensions found (0 bindings)"
    firstDim (b:_) = maybe (error "No af:dimensions found (lookup failed)") getLit $ vbMap b (Var "afdim")

    value            = firstValue valueBndgs
    firstValue []    = error "No af:value found (0 bindings)"
    firstValue (v:_) = maybe (error "No af:value found (lookup failed)") getLit $ vbMap v (Var "afval")

    dimBndgs   = rdfQueryFind afDim g
    valueBndgs = rdfQueryFind afValue g

    getLit (Lit t) = unpack t
    getLit _       = error "Not a literal value"

    parseQuery = either error id . parseN3fromText
    afDim      = parseQuery afDimQry
    afDimQry   = "?signal <http://purl.org/ontology/af/dimensions> ?afdim ."
    afValue    = parseQuery afValQry
    afValQry   = "?signal <http://purl.org/ontology/af/value> ?afval ."

parseTurtlePowerFeaturesGraph :: (Either String RDFGraph) -> IO (Maybe (DV.Vector Double))
parseTurtlePowerFeaturesGraph (Left x) = fail x
parseTurtlePowerFeaturesGraph (Right g) = fail "Sonic Annotator turtle files do not include powers."
