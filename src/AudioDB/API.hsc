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

{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module AudioDB.API ( ADB
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
                   , audiodb_lib_build_date
                   , audiodb_open
                   , audiodb_create
                   , audiodb_close
                   , audiodb_l2norm
                   , audiodb_power
                   , audiodb_insert_datum
                   , audiodb_insert_reference
                   , audiodb_query_spec
                   , audiodb_query_spec_given_sofar
                   , audiodb_query_free_results
                   , audiodb_status
                   , audiodb_retrieve_datum
                   , audiodb_free_datum
                   , audiodb_dump
                   , audiodb_liszt
                   , audiodb_liszt_free_results
                   , audiodb_sample_spec
                   , audiodb_insert
                   , audiodb_batchinsert ) where

import           Data.Char (chr)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes)
import qualified Data.Vector.Storable as DV
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String

#include "audioDB/audioDB_API.h"

data ADB = ADB (Ptr ADB)

-- The ADB* ADTs are fairly literal mappings of each of the structs in
-- the API
data ADBDatum = ADBDatum {
  datum_nvectors :: Int,
  datum_dim      :: Int,
  datum_key      :: String,
  datum_data     :: DV.Vector Double,
  datum_power    :: Maybe (DV.Vector Double),
  datum_times    :: Maybe (DV.Vector Double) } deriving (Eq, Show)
type ADBDatumPtr = Ptr (ADBDatum)

data ADBReference = ADBReference {
  reference_features :: String,
  reference_power    :: String,
  reference_key      :: String,
  reference_times    :: String } deriving (Eq, Show)
type ADBReferencePtr = Ptr (ADBReference)

data ADBStatus = ADBStatus {
  status_numFiles         :: Int,
  status_dim              :: Int,
  status_dudCount         :: Int,
  status_nullCount        :: Int,
  status_flags            :: [HeaderFlag],
  status_length           :: Int,
  status_data_region_size :: Int } deriving (Eq, Show)
type ADBStatusPtr = Ptr (ADBStatus)

data ADBResult = ADBResult {
  result_qkey   :: String,
  result_ikey   :: String,
  result_qpos   :: Int,
  result_ipos   :: Int,
  result_dist   :: Double }
type ADBResultPtr = Ptr (ADBResult)

instance Eq ADBResult where
  (==) (ADBResult { result_qkey = aQKey
                  , result_ikey = aIKey
                  , result_qpos = aQPos
                  , result_ipos = aIPos
                  , result_dist = aDist })
    (ADBResult { result_qkey = bQKey
               , result_ikey = bIKey
               , result_qpos = bQPos
               , result_ipos = bIPos
               , result_dist = bDist })
    | aQKey /= bQKey = False
    | aIKey /= bIKey = False
    | aQPos /= bQPos = False
    | aIPos /= bIPos = False
    | aDist `inRange` (bDist, threshold) = True
    | otherwise = False
    where threshold = 1e-4
          inRange x (y, t) = x >= y - t && x <= y + t

data ADBKeyList = ADBKeyList {
  keylist_nkeys  :: Int,
  keylist_keys   :: [String] } deriving (Eq, Show)
type ADBKeyListPtr = Ptr (ADBKeyList)

data ADBQueryRefine = ADBQueryRefine {
  query_refine_flags              :: [RefinementFlag],
  query_refine_include            :: ADBKeyList,
  query_refine_exclude            :: ADBKeyList,
  query_refine_radius             :: Double,
  query_refine_absolute_threshold :: Double,
  query_refine_relative_threshold :: Double,
  query_refine_duration_ratio     :: Double,
  query_refine_qhopsize           :: Int,
  query_refine_ihopsize           :: Int } deriving (Eq, Show)
type ADBQueryRefinePtr = Ptr (ADBQueryRefine)

data ADBQueryParameters = ADBQueryParameters {
  query_parameters_accumulation  :: [AccumulationFlag],
  query_parameters_distance      :: [DistanceFlag],
  query_parameters_npoints       :: Int,
  query_parameters_ntracks       :: Int } deriving (Eq, Show)
type ADBQueryParametersPtr = Ptr (ADBQueryParameters)

data ADBQueryResults = ADBQueryResults {
  query_results_nresults :: Int,
  query_results_results  :: [ADBResult] } deriving (Eq)
type ADBQueryResultsPtr = Ptr (ADBQueryResults)

data ADBQueryID = ADBQueryID {
  queryid_datum           :: ADBDatumPtr,
  queryid_sequence_length :: Int,
  queryid_flags           :: [QueryIDFlag],
  queryid_sequence_start  :: Int } deriving (Eq, Show)
type ADBQueryIDPtr = Ptr (ADBQueryID)

data ADBQuerySpec = ADBQuerySpec {
  query_spec_qid      :: ADBQueryID,
  query_spec_params   :: ADBQueryParameters,
  query_spec_refine   :: ADBQueryRefine } deriving (Eq, Show)
type ADBQuerySpecPtr = Ptr (ADBQuerySpec)

data ADBTrackEntry = ADBTrackEntry {
  track_entry_nvectors :: Int,
  track_entry_key      :: String } deriving (Eq, Show)
type ADBTrackEntryPtr = Ptr (ADBTrackEntry)

data ADBLisztResults = ADBLisztResults {
  liszt_results_nresults :: Int,
  liszt_results_entries  :: [ADBTrackEntry] } deriving (Eq, Show)
type ADBLisztResultsPtr = Ptr (ADBLisztResults)

-- Storable instances for all the structs allowing us to marshall them
-- in and out of the library
instance Storable ADBDatum where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_datum_t}

  peek d = do
    nv' <- fmap fromIntegral (((#peek adb_datum_t, nvectors) d) :: IO CUInt)

    dim' <- fmap fromIntegral (((#peek adb_datum_t, dim) d) :: IO CUInt)

    key' <- (#peek adb_datum_t, key) d >>= peekCString

    dataField <- ((#peek adb_datum_t, data) d) :: IO (Ptr Double)
    dataPtr   <- newForeignPtr_ dataField
    let data' = DV.unsafeFromForeignPtr0 dataPtr (nv' * dim')

    powerField <- ((#peek adb_datum_t, power) d) :: IO (Ptr Double)
    powerPtr   <- newForeignPtr_ powerField
    let power' = if powerField /= nullPtr then Just (DV.unsafeFromForeignPtr0 powerPtr nv') else Nothing

    timesField <- ((#peek adb_datum_t, times) d) :: IO (Ptr Double)
    timesPtr   <- newForeignPtr_ timesField
    let times' = if timesField /= nullPtr then Just (DV.unsafeFromForeignPtr0 timesPtr nv') else Nothing

    return ADBDatum { datum_nvectors = nv',
                      datum_dim      = dim',
                      datum_key      = key',
                      datum_data     = data',
                      datum_power    = power',
                      datum_times    = times' }

  poke d (ADBDatum nv' dim' key' data' power' times') = do
    (#poke adb_datum_t, nvectors) d nv'
    (#poke adb_datum_t, dim) d dim'

    newCString key' >>= (#poke adb_datum_t, key) d

    DV.unsafeWith data' (\ptrData -> (#poke adb_datum_t, data) d ptrData)

    maybe ((#poke adb_datum_t, power) d nullPtr) (\pow -> DV.unsafeWith pow (\ptrPower -> (#poke adb_datum_t, power) d ptrPower)) power'
    maybe ((#poke adb_datum_t, times) d nullPtr) (\tim -> DV.unsafeWith tim (\ptrTimes -> (#poke adb_datum_t, times) d ptrTimes)) times'

instance Storable ADBReference where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_reference_t}

  peek r = do
    features' <- (#peek adb_reference_t, features) r >>= peekCString
    power'    <- (#peek adb_reference_t, power) r    >>= peekCString
    key'      <- (#peek adb_reference_t, key) r      >>= peekCString
    times'    <- (#peek adb_reference_t, times) r    >>= peekCString

    return ADBReference { reference_features = features',
                          reference_power    = power',
                          reference_key      = key',
                          reference_times    = times' }

  poke r (ADBReference features' power' key' times') = do
    newCString features' >>= (#poke adb_reference_t, features) r
    newCString power'    >>= (#poke adb_reference_t, power) r
    newCString key'      >>= (#poke adb_reference_t, key) r
    newCString times'    >>= (#poke adb_reference_t, times) r

instance Storable ADBStatus where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_status_t}

  peek s = do
    numFiles'         <- fmap fromIntegral (((#peek adb_status_t, numFiles) s) :: IO CUInt)
    dim'              <- fmap fromIntegral (((#peek adb_status_t, dim) s) :: IO CUInt)
    dudCount'         <- fmap fromIntegral (((#peek adb_status_t, dudCount) s) :: IO CUInt)
    nullCount'        <- fmap fromIntegral (((#peek adb_status_t, nullCount) s) :: IO CUInt)
    flags''           <- ((#peek adb_status_t, flags) s) :: IO CInt
    let flags'        = splitHeaderFlags HeaderFlag { unHeaderFlag = flags'' }
    length'           <- fmap fromIntegral (((#peek adb_status_t, length) s) :: IO CULong)
    data_region_size' <- fmap fromIntegral (((#peek adb_status_t, data_region_size) s) :: IO CULong)
    return ADBStatus { status_numFiles         = numFiles',
                       status_dim              = dim',
                       status_dudCount         = dudCount',
                       status_nullCount        = nullCount',
                       status_flags            = flags',
                       status_length           = length',
                       status_data_region_size = data_region_size' }

  poke s (ADBStatus numFiles' dim' dudCount' nullCount' flags' length' data_region_size') = do
    (#poke adb_status_t, numFiles) s numFiles'
    (#poke adb_status_t, dim) s dim'
    (#poke adb_status_t, dudCount) s dudCount'
    (#poke adb_status_t, nullCount) s nullCount'
    (#poke adb_status_t, flags) s (unHeaderFlag $ combineHeaderFlags flags')
    (#poke adb_status_t, length) s length'
    (#poke adb_status_t, data_region_size) s data_region_size'

instance Storable ADBResult where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_result_t}

  peek r = do
    qkey' <- (#peek adb_result_t, qkey) r >>= peekCString
    ikey' <- (#peek adb_result_t, ikey) r >>= peekCString

    qpos' <- fmap fromIntegral (((#peek adb_result_t, qpos) r) :: IO CUInt)
    ipos' <- fmap fromIntegral (((#peek adb_result_t, ipos) r) :: IO CUInt)
    dist' <- fmap realToFrac (((#peek adb_result_t, dist) r) :: IO CDouble)
    return ADBResult { result_qkey = qkey',
                       result_ikey = ikey',
                       result_qpos = qpos',
                       result_ipos = ipos',
                       result_dist = dist' }

  poke r (ADBResult qkey' ikey' qpos' ipos' dist') = do
    newCString qkey' >>= (#poke adb_result_t, qkey) r
    newCString ikey' >>= (#poke adb_result_t, ikey) r

    (#poke adb_result_t, qpos) r qpos'
    (#poke adb_result_t, ipos) r ipos'
    (#poke adb_result_t, dist) r dist'

instance Show ADBResult where
  show r =
    q ++ " (@ " ++ (show qp) ++ ") is in track " ++ k ++ " @ " ++ (show pos) ++ "; distance is " ++ (show dist)
    where
      q    = (result_qkey r)
      qp   = (result_qpos r)
      k    = (result_ikey r)
      pos  = (result_ipos r)
      dist = (result_dist r)

splitCStrL :: Ptr CChar -> Int -> IO [String]
splitCStrL p n = splt p []
  where
    splt :: Ptr CChar -> [String] -> IO [String]
    splt ptr acc = do
      s <- peekCString ptr
      l <- if (length acc) < n then splt (ptr `plusPtr` (length s)) (s : acc) else return acc
      return (reverse l)

joinCStrL :: [String] -> IO CString
joinCStrL ss = do
  let s = intercalate [(chr 0)] ss
  cs <- newCString s
  return cs

instance Storable ADBKeyList where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_keylist_t}

  peek kl = do
    nkeys' <- fmap fromIntegral (((#peek adb_keylist_t, nkeys) kl) :: IO CUInt)

    keys' <- (#peek adb_keylist_t, keys) kl >>= (\c -> splitCStrL c nkeys')

    return ADBKeyList { keylist_nkeys = nkeys',
                        keylist_keys  = keys' }

  poke kl (ADBKeyList nkeys' keys') = do
    (#poke adb_keylist_t, nkeys) kl nkeys'
    joinCStrL keys' >>= (#poke adb_keylist_t, keys) kl

instance Storable ADBQueryRefine where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_refine_t}

  peek qr = do
    flags''             <- ((#peek adb_query_refine_t, flags) qr) :: IO CUInt
    let flags'          = splitRefinementFlags RefinementFlag { unRefinementFlag = flags'' }
    include'            <- (#peek adb_query_refine_t, include) qr
    exclude'            <- (#peek adb_query_refine_t, exclude) qr
    radius'             <- fmap realToFrac (((#peek adb_query_refine_t, radius) qr) :: IO CDouble)
    absolute_threshold' <- fmap realToFrac (((#peek adb_query_refine_t, absolute_threshold) qr) :: IO CDouble)
    relative_threshold' <- fmap realToFrac (((#peek adb_query_refine_t, relative_threshold) qr) :: IO CDouble)
    duration_ratio'     <- fmap realToFrac (((#peek adb_query_refine_t, duration_ratio) qr) :: IO CDouble)
    qhopsize'           <- fmap fromIntegral (((#peek adb_query_refine_t, qhopsize) qr) :: IO CUInt)
    ihopsize'           <- fmap fromIntegral (((#peek adb_query_refine_t, ihopsize) qr) :: IO CUInt)
    return ADBQueryRefine { query_refine_flags              = flags',
                            query_refine_include            = include',
                            query_refine_exclude            = exclude',
                            query_refine_radius             = radius',
                            query_refine_absolute_threshold = absolute_threshold',
                            query_refine_relative_threshold = relative_threshold',
                            query_refine_duration_ratio     = duration_ratio',
                            query_refine_qhopsize           = qhopsize',
                            query_refine_ihopsize           = ihopsize' }

  poke qr (ADBQueryRefine flags' include' exclude' radius' absolute_threshold' relative_threshold' duration_ratio' qhopsize' ihopsize') = do
    (#poke adb_query_refine_t, flags) qr (unRefinementFlag $ combineRefinementFlags flags')
    (#poke adb_query_refine_t, include) qr include'
    (#poke adb_query_refine_t, exclude) qr exclude'
    (#poke adb_query_refine_t, radius) qr radius'
    (#poke adb_query_refine_t, absolute_threshold) qr absolute_threshold'
    (#poke adb_query_refine_t, relative_threshold) qr relative_threshold'
    (#poke adb_query_refine_t, duration_ratio) qr duration_ratio'
    (#poke adb_query_refine_t, qhopsize) qr qhopsize'
    (#poke adb_query_refine_t, ihopsize) qr ihopsize'

instance Storable ADBQueryParameters where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_parameters_t}

  peek qp = do
    accFlags''     <- ((#peek adb_query_parameters_t, accumulation) qp) :: IO CUInt
    let accFlags'  = splitAccumulationFlags AccumulationFlag { unAccumulationFlag = accFlags'' }

    distFlags''    <- ((#peek adb_query_parameters_t, distance) qp) :: IO CUInt
    let distFlags' = DistanceFlag { unDistanceFlag = distFlags'' }
    let distFlags'  = splitDistanceFlags DistanceFlag { unDistanceFlag = distFlags'' }

    npoints'       <- fmap fromIntegral (((#peek adb_query_parameters_t, npoints) qp) :: IO CUInt)
    ntracks'       <- fmap fromIntegral (((#peek adb_query_parameters_t, ntracks) qp) :: IO CUInt)
    return ADBQueryParameters { query_parameters_accumulation = accFlags',
                                query_parameters_distance     = distFlags',
                                query_parameters_npoints      = npoints',
                                query_parameters_ntracks      = ntracks' }

  poke qp (ADBQueryParameters accumulation' distance' npoints' ntracks') = do
    (#poke adb_query_parameters_t, accumulation) qp (unAccumulationFlag $ combineAccumulationFlags accumulation')
    (#poke adb_query_parameters_t, distance) qp (unDistanceFlag $ combineDistanceFlags distance')
    (#poke adb_query_parameters_t, npoints) qp npoints'
    (#poke adb_query_parameters_t, ntracks) qp ntracks'

instance Storable ADBQueryResults where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_results_t}

  peek qr = do
    nresults' <- fmap fromIntegral (((#peek adb_query_results_t, nresults) qr) :: IO CUInt)
    results'  <- (#peek adb_query_results_t, results) qr >>= peekArray nresults'
    return ADBQueryResults { query_results_nresults = nresults',
                             query_results_results  = results' }

  poke = error "ADBQueryResults is read-only"

instance Show ADBQueryResults where
  show r =
    (show n) ++ " hits:\n" ++ unlines (map show results)
    where
      n       = (query_results_nresults r)
      results = (query_results_results r)

instance Storable ADBQueryID where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_id_t}

  peek qid = do
    datum'           <- (#peek adb_query_id_t, datum) qid
    sequence_length' <- fmap fromIntegral (((#peek adb_query_id_t, sequence_length) qid) :: IO CUInt)
    flags''          <- ((#peek adb_query_id_t, flags) qid) :: IO CUInt
    let flags'       = splitQueryIDFlags QueryIDFlag { unQueryIDFlag = flags'' }
    sequence_start'  <- fmap fromIntegral (((#peek adb_query_id_t, sequence_start) qid) :: IO CUInt)
    return ADBQueryID { queryid_datum           = datum',
                        queryid_sequence_length = sequence_length',
                        queryid_flags           = flags',
                        queryid_sequence_start  = sequence_start' }

  poke qid (ADBQueryID datum' sequence_length' flags' sequence_start') = do
    (#poke adb_query_id_t, datum) qid datum'
    (#poke adb_query_id_t, sequence_length) qid sequence_length'
    (#poke adb_query_id_t, flags) qid (unQueryIDFlag $ combineQueryIDFlags flags')
    (#poke adb_query_id_t, sequence_start) qid sequence_start'

instance Storable ADBQuerySpec where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_query_spec_t}

  peek qs = do
    qid'    <- (#peek adb_query_spec_t, qid) qs
    params' <- (#peek adb_query_spec_t, params) qs
    refine' <- (#peek adb_query_spec_t, refine) qs
    return ADBQuerySpec { query_spec_qid    = qid',
                          query_spec_params = params',
                          query_spec_refine = refine' }

  poke qs (ADBQuerySpec qid' params' refine') = do
    (#poke adb_query_spec_t, qid) qs qid'
    (#poke adb_query_spec_t, params) qs params'
    (#poke adb_query_spec_t, refine) qs refine'

instance Storable ADBTrackEntry where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_track_entry_t}

  peek te = do
    nvectors' <- fmap fromIntegral (((#peek adb_track_entry_t, nvectors) te) :: IO CUInt)

    key' <- (#peek adb_track_entry_t, key) te >>= peekCString

    return ADBTrackEntry { track_entry_nvectors = nvectors',
                           track_entry_key      = key' }

  poke te (ADBTrackEntry nvectors' key') = do
    (#poke adb_track_entry_t, nvectors) te nvectors'

    newCString key' >>= (#poke adb_track_entry_t, key) te

instance Storable ADBLisztResults where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #{size adb_liszt_results_t}

  peek lr = do
    nresults' <- fmap fromIntegral (((#peek adb_liszt_results_t, nresults) lr) :: IO CUInt)
    entries'  <- (#peek adb_liszt_results_t, entries) lr >>= peekArray nresults'
    return ADBLisztResults { liszt_results_nresults = nresults',
                             liszt_results_entries  = entries' }

  poke = error "ADBLisztResults is read-only"

-- Importing the ADB_ flags

newtype QueryIDFlag = QueryIDFlag { unQueryIDFlag :: CUInt }
                    deriving (Eq, Show)

#{enum QueryIDFlag, QueryIDFlag
 , exhaustiveFlag          = ADB_QID_FLAG_EXHAUSTIVE
 , allowFalsePositivesFlag = ADB_QID_FLAG_ALLOW_FALSE_POSITIVES
 }

combineQueryIDFlags :: [QueryIDFlag] -> QueryIDFlag
combineQueryIDFlags = QueryIDFlag . foldr ((.|.) . unQueryIDFlag) 0

splitQueryIDFlags :: QueryIDFlag -> [QueryIDFlag]
splitQueryIDFlags flags = catMaybes $ map (\flag -> if (unQueryIDFlag flag) .&. (unQueryIDFlag flags) /= (0 :: CUInt)
                                                    then Just flag
                                                    else Nothing)
                          [exhaustiveFlag, allowFalsePositivesFlag]

-- These constants are defined in audioDB-internals.h which is a
-- private header
#define ADB_HEADER_FLAG_L2NORM		(0x1U)
#define ADB_HEADER_FLAG_POWER		(0x4U)
#define ADB_HEADER_FLAG_TIMES		(0x20U)
#define ADB_HEADER_FLAG_REFERENCES	(0x40U)

newtype HeaderFlag = HeaderFlag { unHeaderFlag :: CInt } deriving (Eq, Show)

#{enum HeaderFlag, HeaderFlag
 , l2normFlag     = ADB_HEADER_FLAG_L2NORM
 , powerFlag      = ADB_HEADER_FLAG_POWER
 , timesFlag      = ADB_HEADER_FLAG_TIMES
 , referencesFlag = ADB_HEADER_FLAG_REFERENCES
 }

combineHeaderFlags :: [HeaderFlag] -> HeaderFlag
combineHeaderFlags = HeaderFlag . foldr ((.|.) . unHeaderFlag) 0

splitHeaderFlags :: HeaderFlag -> [HeaderFlag]
splitHeaderFlags flags = catMaybes $ map (\flag -> if (unHeaderFlag flag) .&. (unHeaderFlag flags) /= (0 :: CInt)
                                                   then Just flag
                                                   else Nothing)
                         [l2normFlag, powerFlag, timesFlag, referencesFlag]

newtype AccumulationFlag = AccumulationFlag { unAccumulationFlag :: CUInt } deriving (Eq, Show)

#{enum AccumulationFlag, AccumulationFlag
 , databaseFlag = ADB_ACCUMULATION_DB
 , perTrackFlag = ADB_ACCUMULATION_PER_TRACK
 , oneToOneFlag = ADB_ACCUMULATION_ONE_TO_ONE
 }

combineAccumulationFlags :: [AccumulationFlag] -> AccumulationFlag
combineAccumulationFlags = AccumulationFlag . foldr ((.|.) . unAccumulationFlag) 0

splitAccumulationFlags :: AccumulationFlag -> [AccumulationFlag]
splitAccumulationFlags flags = catMaybes $ map (\flag -> if (unAccumulationFlag flag) .&. (unAccumulationFlag flags) /= (0 :: CUInt)
                                                         then Just flag
                                                         else Nothing)
                               [databaseFlag, perTrackFlag, oneToOneFlag]

newtype DistanceFlag = DistanceFlag { unDistanceFlag :: CUInt } deriving (Eq, Show)

#{enum DistanceFlag, DistanceFlag
 , dotProductFlag                = ADB_DISTANCE_DOT_PRODUCT
 , euclideanNormedFlag           = ADB_DISTANCE_EUCLIDEAN_NORMED
 , euclideanFlag                 = ADB_DISTANCE_EUCLIDEAN
 , kullbackLeiblerDivergenceFlag = ADB_DISTANCE_KULLBACK_LEIBLER_DIVERGENCE
 }

combineDistanceFlags :: [DistanceFlag] -> DistanceFlag
combineDistanceFlags = DistanceFlag . foldr ((.|.) . unDistanceFlag) 0

splitDistanceFlags :: DistanceFlag -> [DistanceFlag]
splitDistanceFlags flags = catMaybes $ map (\flag -> if (unDistanceFlag flag) .&. (unDistanceFlag flags) /= (0 :: CUInt)
                                                     then Just flag
                                                     else Nothing)
                           [dotProductFlag, euclideanNormedFlag, euclideanFlag, kullbackLeiblerDivergenceFlag]

newtype RefinementFlag = RefinementFlag { unRefinementFlag :: CUInt } deriving (Eq, Show)

#{enum RefinementFlag, RefinementFlag
 , includeKeyListFlag    = ADB_REFINE_INCLUDE_KEYLIST
 , excludeKeyListFlag    = ADB_REFINE_EXCLUDE_KEYLIST
 , radiusFlag            = ADB_REFINE_RADIUS
 , absoluteThresholdFlag = ADB_REFINE_ABSOLUTE_THRESHOLD
 , relativeThresholdFlag = ADB_REFINE_RELATIVE_THRESHOLD
 , durationRatioFlag     = ADB_REFINE_DURATION_RATIO
 , hopSizeFlag           = ADB_REFINE_HOP_SIZE
 }

combineRefinementFlags :: [RefinementFlag] -> RefinementFlag
combineRefinementFlags = RefinementFlag . foldr ((.|.) . unRefinementFlag) 0

splitRefinementFlags :: RefinementFlag -> [RefinementFlag]
splitRefinementFlags flags = catMaybes $ map (\flag -> if (unRefinementFlag flag) .&. (unRefinementFlag flags) /= (0 :: CUInt)
                                                       then Just flag
                                                       else Nothing)
                             [includeKeyListFlag, excludeKeyListFlag, radiusFlag, absoluteThresholdFlag, relativeThresholdFlag, durationRatioFlag, hopSizeFlag]

-- Importing the foreign functions

foreign import ccall unsafe "audioDB_API.h audiodb_lib_build_id"
  audiodb_lib_build_id :: IO CString

foreign import ccall unsafe "audioDB_API.h audiodb_lib_build_date"
  audiodb_lib_build_date :: IO CString

foreign import ccall unsafe "audioDB_API.h audiodb_open"
  audiodb_open :: CString -> CInt -> IO (Ptr ADB)

foreign import ccall unsafe "audioDB_API.h audiodb_create"
  audiodb_create :: CString -> CUInt -> CUInt -> CUInt -> IO (Ptr ADB)

foreign import ccall unsafe "audioDB_API.h audiodb_close"
  audiodb_close :: (Ptr ADB) -> IO ()

foreign import ccall unsafe "audioDB_API.h audiodb_l2norm"
  audiodb_l2norm :: (Ptr ADB) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_power"
  audiodb_power :: (Ptr ADB) -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_insert_datum"
  audiodb_insert_datum :: (Ptr ADB) -> ADBDatumPtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_insert_reference"
  audiodb_insert_reference :: (Ptr ADB) -> ADBReferencePtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_query_spec"
  audiodb_query_spec :: (Ptr ADB) -> ADBQuerySpecPtr -> IO ADBQueryResultsPtr

foreign import ccall unsafe "audioDB_API.h audiodb_query_spec_given_sofar"
  audiodb_query_spec_given_sofar :: (Ptr ADB) -> ADBQuerySpecPtr -> ADBQueryResultsPtr -> IO ADBQueryResultsPtr

foreign import ccall unsafe "audioDB_API.h audiodb_query_free_results"
  audiodb_query_free_results :: (Ptr ADB) -> ADBQuerySpecPtr -> ADBQueryResultsPtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_status"
  audiodb_status :: (Ptr ADB) -> ADBStatusPtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_retrieve_datum"
  audiodb_retrieve_datum :: (Ptr ADB) -> CString -> ADBDatumPtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_free_datum"
  audiodb_free_datum :: (Ptr ADB) -> ADBDatumPtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_dump"
  audiodb_dump :: (Ptr ADB) -> CString -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_liszt"
  audiodb_liszt :: (Ptr ADB) -> IO ADBLisztResultsPtr

foreign import ccall unsafe "audioDB_API.h audiodb_liszt_free_results"
  audiodb_liszt_free_results :: (Ptr ADB) -> ADBLisztResultsPtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_sample_spec"
  audiodb_sample_spec :: (Ptr ADB) -> ADBQuerySpecPtr -> IO ADBQueryResultsPtr

foreign import ccall unsafe "audioDB_API.h audiodb_insert"
  audiodb_insert :: (Ptr ADB) -> ADBReferencePtr -> IO CInt

foreign import ccall unsafe "audioDB_API.h audiodb_batchinsert"
  audiodb_batchinsert :: (Ptr ADB) -> ADBReferencePtr -> CUInt -> IO CInt
