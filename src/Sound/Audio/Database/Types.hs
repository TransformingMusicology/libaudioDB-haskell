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

{-# LANGUAGE DeriveDataTypeable #-}

module Sound.Audio.Database.Types ( Frame
                                  , Seconds
                                  , FrameSize
                                  , FeatureRate
                                  , QueryException(..)
                                  , FeaturesException(..)
                                  , DatabaseException(..)
                                  , inSeconds
                                  , inFrames
                                  , withSeconds
                                  , withFrames ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

type Frame = Int
type Seconds = Double
type FrameSize = (Frame -> Seconds)
type FeatureRate = (Seconds -> Frame)

data QueryException = QuerySequenceBoundsException Int Int Int
                    | QueryDimensionsMismatchException Int Int
                    deriving (Show, Typeable)
instance Exception QueryException

data FeaturesException = FeaturesMissingPowersException
                       | FeaturesDBPowerFlagNotSetException
                       deriving (Show, Typeable)
instance Exception FeaturesException

data DatabaseException = DBStatusException
                       deriving (Show, Typeable)
instance Exception DatabaseException

inSeconds :: FrameSize
inSeconds = fromIntegral

inFrames :: FeatureRate
inFrames = ceiling

withSeconds :: FeatureRate -> FrameSize -> (Seconds -> Seconds) -> Frame -> Frame
withSeconds secToFrames framesToSec f frames = (secToFrames . f . framesToSec) frames

withFrames :: FeatureRate -> FrameSize -> (Frame -> Frame) -> Seconds -> Seconds
withFrames secToFrames framesToSec f seconds = (framesToSec . f . secToFrames) seconds
