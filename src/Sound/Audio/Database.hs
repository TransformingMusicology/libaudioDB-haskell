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

module Sound.Audio.Database ( QueryException(..)
                            , DatabaseException(..)
                            , withExistingROAudioDB
                            , withADBStatus
                            , withMaybeDatumPtr
                            , withNewAudioDB
                            , featuresFromKey
                            , checkDimensions
                            , emptyADBKeyList
                              -- Things re-exported from AudioDB.API
                            , ADB
                            , ADBDatum(..)
                            , ADBDatumPtr
                            , ADBKeyList(..)
                            , ADBQueryID(..)
                            , ADBQueryParameters(..)
                            , ADBQueryRefine(..)
                            , ADBQueryResults(..)
                            , ADBQueryResultsPtr
                            , ADBQuerySpec(..)
                            , ADBQuerySpecPtr
                            , ADBReference(..)
                            , ADBResult(..)
                            , ADBResultPtr
                            , ADBStatus(..)
                            , QueryIDFlag(..)
                            , exhaustiveFlag
                            , allowFalsePositivesFlag
                            , HeaderFlag(..)
                            , l2normFlag
                            , powerFlag
                            , timesFlag
                            , referencesFlag
                            , AccumulationFlag(..)
                            , databaseFlag
                            , perTrackFlag
                            , oneToOneFlag
                            , DistanceFlag(..)
                            , dotProductFlag
                            , euclideanNormedFlag
                            , euclideanFlag
                            , kullbackLeiblerDivergenceFlag
                            , RefinementFlag(..)
                            , includeKeyListFlag
                            , excludeKeyListFlag
                            , radiusFlag
                            , absoluteThresholdFlag
                            , relativeThresholdFlag
                            , durationRatioFlag
                            , hopSizeFlag
                            , audiodb_lib_build_id
                            , audiodb_lib_build_date ) where

import AudioDB.API
import Control.Exception (throw, Exception, bracket)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Foreign (Ptr, peek, nullPtr)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (alloca)
import Sound.Audio.Database.Types
--import System.C.IO

data QueryException = QuerySequenceBoundsException Int Int Int
                    | QueryDimensionsMismatchException Int Int
                    deriving (Show, Typeable)
instance Exception QueryException

data DatabaseException = DBStatusException
                       deriving (Show, Typeable)
instance Exception DatabaseException

openDB :: FilePath -> (Ptr ADB)
openDB = undefined

closeDB :: (Ptr ADB) -> IO ()
closeDB = undefined

createDB :: FilePath -> (Ptr ADB)
createDB = undefined

withAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> (Ptr ADB) -> ADBResult
withAudioDB = undefined

withExistingAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> FilePath -> ADBResult
withExistingAudioDB = undefined

withExistingROAudioDB :: FilePath -> (Maybe (Ptr ADB) -> IO a) -> IO a
withExistingROAudioDB fp f = do
  adbFN  <- newCString fp
  bracket (audiodb_open adbFN 0)--(oflags [O_RDONLY]))
    (\adb -> if adb /= nullPtr then audiodb_close adb else return ())
    (\adb -> f $ if adb /= nullPtr then Just adb else Nothing)

withNewAudioDB :: FilePath -- file name
               -> Int      -- datasize
               -> Int      -- number of tracks
               -> Int      -- dimensions of features
               -> (Maybe (Ptr ADB) -> IO a)
               -> IO a
withNewAudioDB fp datasize ntracks dim f = do
  adbFN  <- newCString fp
  bracket (audiodb_create adbFN (fromIntegral datasize) (fromIntegral ntracks) (fromIntegral dim))
    (\adb -> if adb /= nullPtr then audiodb_close adb else return ())
    (\adb -> f $ if adb /= nullPtr then Just adb else Nothing)

withADBStatus :: (ADBStatus -> IO a) -> (Ptr ADB) -> IO a
withADBStatus f adb = do
  alloca $ \statusPtr -> do
    res     <- audiodb_status adb statusPtr
    when (res /= 0) (throw DBStatusException)
    status  <- peek statusPtr
    (f status)

withMaybeDatumPtr :: (Ptr ADB) -> String -> (Maybe ADBDatumPtr -> a) -> IO a
withMaybeDatumPtr adb key f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then do return $ f Nothing
    else do return $ f (Just datumPtr)

featuresFromKey :: (Ptr ADB) -> String -> IO (Maybe ADBDatumPtr)
featuresFromKey adb key = withMaybeDatumPtr adb key id

checkDimensions :: (Ptr ADB) -> ADBDatum -> IO Bool
checkDimensions adb datum =
  withADBStatus (\s -> if not (checkDim s datum)
                       then throw $ QueryDimensionsMismatchException (status_dim s) (datum_dim datum)
                       else return True) adb
  where checkDim s d = (status_dim s) == (datum_dim d)

emptyADBKeyList :: ADBKeyList
emptyADBKeyList = ADBKeyList { keylist_nkeys = 0, keylist_keys = [] }
