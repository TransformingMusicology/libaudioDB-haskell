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

module Sound.Audio.Database ( QueryException(..)
                            , DatabaseException(..)
                            , FeaturesException(..)
                            , withExistingAudioDB
                            , withExistingROAudioDB
                            , withADBStatus
                            , withDatumAsPtr
                            , withDatumAsPtrIO
                            , withMaybeDatum
                            , withMaybeDatumIO
                            , withMaybeDatumPtr
                            , withMaybeDatumPtrIO
                            , withMaybeDatumSlice
                            , withMaybeDatumSliceIO
                            , withNewAudioDB
                            , withNewL2NormedAudioDB
                            , withNewPoweredAudioDB
                            , withNewL2NormedPoweredAudioDB
                            , isL2Normed
                            , isPowered
                            , isTimes
                            , isReferences
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
import Control.Exception (throw, bracket)
import Control.Monad (when)
import Foreign (Ptr, peek, poke, nullPtr)
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (alloca, malloc, free)
import Sound.Audio.Features (datumSlice)
import Sound.Audio.Database.Types
--import System.C.IO

openDB :: FilePath -> (Ptr ADB)
openDB = undefined

closeDB :: (Ptr ADB) -> IO ()
closeDB = undefined

createDB :: FilePath -> (Ptr ADB)
createDB = undefined

withAudioDB :: (ADBQuerySpec -> ADBResult) -> FeatureRate -> (Ptr ADB) -> ADBResult
withAudioDB = undefined

withExistingAudioDB :: FilePath -> (Maybe (Ptr ADB) -> IO a) -> IO a
withExistingAudioDB fp f = do
  adbFN  <- newCString fp
  bracket (audiodb_open adbFN 2)--(oflags [O_RDWR]))
    (\adb -> if adb /= nullPtr then audiodb_close adb else return ())
    (\adb -> f $ if adb /= nullPtr then Just adb else Nothing)

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
               -> Bool     -- L2 normed
               -> Bool     -- with powers
               -> (Maybe (Ptr ADB) -> IO a)
               -> IO a
withNewAudioDB fp datasize ntracks dim l2norm power f = do
  adbFN  <- newCString fp
  bracket (audiodb_create adbFN (fromIntegral datasize) (fromIntegral ntracks) (fromIntegral dim) >>= setupDB l2norm power)
    (\adb -> if adb /= nullPtr then audiodb_close adb else return ())
    (\adb -> f $ if adb /= nullPtr then Just adb else Nothing)
  where setupDB True True adb
          | adb == nullPtr = return nullPtr
          | otherwise = do
              l <- setADBL2Normed adb
              p <- setADBPowered adb
              return $ if l && p then adb else nullPtr
        setupDB True False adb
          | adb == nullPtr = return nullPtr
          | otherwise = do
              l <- setADBL2Normed adb
              return $ if l then adb else nullPtr
        setupDB False True adb
          | adb == nullPtr = return nullPtr
          | otherwise = do
              p <- setADBPowered adb
              return $ if p then adb else nullPtr
        setupDB False False adb = return adb

withNewL2NormedAudioDB        :: FilePath -> Int -> Int -> Int -> (Maybe (Ptr ADB) -> IO a) -> IO a
withNewPoweredAudioDB         :: FilePath -> Int -> Int -> Int -> (Maybe (Ptr ADB) -> IO a) -> IO a
withNewL2NormedPoweredAudioDB :: FilePath -> Int -> Int -> Int -> (Maybe (Ptr ADB) -> IO a) -> IO a

withNewL2NormedAudioDB        fp datasize ntracks dim f = withNewAudioDB fp datasize ntracks dim True False f
withNewPoweredAudioDB         fp datasize ntracks dim f = withNewAudioDB fp datasize ntracks dim False True f
withNewL2NormedPoweredAudioDB fp datasize ntracks dim f = withNewAudioDB fp datasize ntracks dim True True f

setADBL2Normed :: (Ptr ADB) -> IO Bool
setADBL2Normed adb = do
  res <- audiodb_l2norm adb
  return $ res == 0

setADBPowered :: (Ptr ADB) -> IO Bool
setADBPowered adb = do
  res <- audiodb_power adb
  return $ res == 0

withADBStatus :: (ADBStatus -> IO a) -> (Ptr ADB) -> IO a
withADBStatus f adb = do
  alloca $ \statusPtr -> do
    res     <- audiodb_status adb statusPtr
    when (res /= 0) (throw DBStatusException)
    status  <- peek statusPtr
    (f status)

isHeaderFlag :: HeaderFlag -> ADBStatus -> Bool
isHeaderFlag flag s = elem flag (status_flags s)

isL2Normed :: ADBStatus -> Bool
isL2Normed = isHeaderFlag l2normFlag

isPowered :: ADBStatus -> Bool
isPowered = isHeaderFlag powerFlag

isTimes :: ADBStatus -> Bool
isTimes = isHeaderFlag timesFlag

isReferences :: ADBStatus -> Bool
isReferences = isHeaderFlag referencesFlag

-- FIXME For documentation: emphasise that clients should not return
-- the ADBDatumPtr and attempt to use it
withDatumAsPtr :: ADBDatum -> (ADBDatumPtr -> a) -> IO a
withDatumAsPtr datum f = alloca pokeAndDo
  where
    pokeAndDo datumPtr = do
      poke datumPtr datum
      return $ f datumPtr

withDatumAsPtrIO :: ADBDatum -> (ADBDatumPtr -> IO a) -> IO a
withDatumAsPtrIO datum f = alloca pokeAndDo
  where
    pokeAndDo datumPtr = do
      poke datumPtr datum
      f datumPtr

-- FIXME For documentation: emphasise that clients should not return
-- the ADBDatumPtr and attempt to use it
withMaybeDatumPtr :: (Ptr ADB) -> String -> (Maybe ADBDatumPtr -> a) -> IO a
withMaybeDatumPtr adb key f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then do return $ f Nothing
    else do return $ f (Just datumPtr)

withMaybeDatumPtrIO :: (Ptr ADB) -> String -> (Maybe ADBDatumPtr -> IO a) -> IO a
withMaybeDatumPtrIO adb key f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then f Nothing
    else f (Just datumPtr)

withMaybeDatum :: (Ptr ADB) -> String -> (Maybe ADBDatum -> a) -> IO a
withMaybeDatum adb key f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then do return $ f Nothing
    else do { d <- peek datumPtr; return $ f (Just d) }

withMaybeDatumIO :: (Ptr ADB) -> String -> (Maybe ADBDatum -> IO a) -> IO a
withMaybeDatumIO adb key f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then f Nothing
    else do { d <- peek datumPtr; f (Just d) }

-- FIXME DRY here: perhaps define datum slicing separately and then define these in terms of withMaybeDatum
withMaybeDatumSlice :: (Ptr ADB) -> String -> FeatureRate -> Seconds -> Seconds -> (Maybe ADBDatum -> a) -> IO a
withMaybeDatumSlice adb key featureRate start len f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then do return $ f Nothing
    else do
      d <- peek datumPtr
      case datumSlice d featureRate start len of
        Right datum -> return $ f (Just datum)
        Left x      -> throw x

withMaybeDatumSliceIO :: (Ptr ADB) -> String -> FeatureRate -> Seconds -> Seconds -> (Maybe ADBDatum -> IO a) -> IO a
withMaybeDatumSliceIO adb key featureRate start len f = alloca $ \datumPtr -> do
  key'  <- newCString key
  res   <- audiodb_retrieve_datum adb key' datumPtr
  if res /= 0
    then f Nothing
    else do
      d <- peek datumPtr
      case datumSlice d featureRate start len of
        Right datum -> f (Just datum)
        Left x      -> throw x

checkDimensions :: (Ptr ADB) -> ADBDatum -> IO Bool
checkDimensions adb datum =
  withADBStatus (\s -> if not (checkDim s datum)
                       then throw $ QueryDimensionsMismatchException (status_dim s) (datum_dim datum)
                       else return True) adb
  where checkDim s d = (status_dim s) == (datum_dim d)

emptyADBKeyList :: ADBKeyList
emptyADBKeyList = ADBKeyList { keylist_nkeys = 0, keylist_keys = [] }
