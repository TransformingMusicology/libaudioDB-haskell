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

module Sound.Audio.Database.Query ( QueryAllocator
                                  , QueryTransformer
                                  , QueryCallback
                                  , QueryComplete
                                  , querySinglePass
                                  , querySinglePassPtr
                                  , withQuery
                                  , unsafeWithQuery
                                  , applyQuery
                                  , unsafeApplyQuery
                                  , withResults
                                  , reverseResults
                                  , reverseResultsPtr
                                  , applyResults
                                  , queryWithCallback
                                  , queryWithTransform
                                  , queryWithCallbacksAndTransform
                                  , query
                                  , mkPointQuery
                                  , mkTrackQuery
                                  , mkSequenceQuery
                                  , execSequenceQuery
                                  , mkSequencePerTrackQuery
                                  , execSequencePerTrackQuery
                                  , transformSequencePerTrackQuery
                                  , mkNSequenceQuery
                                  , execNSequenceQuery
                                  , mkSequenceQueryWithRotation
                                  , execSequenceQueryWithRotation
                                  , mkSequencePerTrackQueryDeltaNTracks
                                  , mkSequencePerTrackQueryWithRotation
                                  , execSequencePerTrackQueryWithRotation
                                  , mkNSequenceQueryWithRotation
                                  , execNSequenceQueryWithRotation ) where

import           AudioDB.API
import           Control.Exception (throw)
import           Control.Monad (when)
import qualified Data.Vector.Storable as DV
import           Data.Maybe (catMaybes)
import           Foreign (Ptr, peek, poke)
import           Foreign.Marshal.Alloc (alloca, malloc, free)
import           Foreign.Ptr (nullPtr)
import           Sound.Audio.Features (datumSlice)
import           Sound.Audio.Database
import           Sound.Audio.Database.Types

(|||) :: Maybe a -> b -> Maybe b
Just _  ||| b = Just b
Nothing ||| _ = Nothing

(||-||) :: (Maybe a, Maybe a) -> b -> Maybe b
(Nothing, Nothing) ||-|| _ = Nothing
_                  ||-|| b = Just b

(//) :: Maybe a -> a -> a
Just x  // _ = x
Nothing // y = y

type QueryAllocator   = (ADBQuerySpecPtr -> IO ())
type QueryTransformer = (Int -> ADBQueryResultsPtr -> QueryAllocator -> QueryAllocator)
type QueryCallback a  = (Int -> ADBQueryResultsPtr -> IO a)
type QueryComplete    = (Int -> QueryAllocator -> ADBQueryResultsPtr -> IO Bool)

type RefinementParams = (Maybe ADBKeyList, Maybe ADBKeyList, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Seconds, Maybe Seconds)

catJustRfnParams :: RefinementParams -> [RefinementFlag]
catJustRfnParams (incl, excl, rad, absThrsh, relThrsh, durRat, qHopSz, iHopSz) =
  catMaybes [(incl     ||| includeKeyListFlag),
             (excl     ||| excludeKeyListFlag),
             (rad      ||| radiusFlag),
             (absThrsh ||| absoluteThresholdFlag),
             (relThrsh ||| relativeThresholdFlag),
             (durRat   ||| durationRatioFlag),
             ((qHopSz, iHopSz) ||-|| hopSizeFlag)]

mkQuery :: ADBDatum   -- query datum
           -> Maybe FeatureRate
           -> Maybe Seconds    -- sequence start
           -> Maybe Seconds    -- sequence length
           -> Maybe [QueryIDFlag]
           -> Maybe [AccumulationFlag]
           -> Maybe [DistanceFlag]
           -> Maybe Int        -- number of point nearest neighbours
           -> Maybe Int        -- number of tracks
           -> Maybe ADBKeyList -- include
           -> Maybe ADBKeyList -- exclude
           -> Maybe Double     -- radius
           -> Maybe Double     -- absoluate threshold
           -> Maybe Double     -- relative threshold
           -> Maybe Double     -- duration ratio
           -> Maybe Seconds    -- query hop size
           -> Maybe Seconds    -- instance hop size
           -> ADBQuerySpecPtr
           -> IO ()

mkQuery datum secToFrames sqStart sqLen qidFlgs acc dist ptsNN resultLen incl excl rad absThrsh relThrsh durRat qHopSz iHopSz qPtr = do
  datumPtr <- malloc
  poke datumPtr datum

  let fr = (secToFrames // inFrames)
      qid = ADBQueryID {
        queryid_datum           = datumPtr,
        queryid_sequence_length = fr (sqLen // 16),
        queryid_flags           = (qidFlgs // [allowFalsePositivesFlag]),
        queryid_sequence_start  = fr (sqStart // 0) }

      params = ADBQueryParameters {
        query_parameters_accumulation = (acc // [databaseFlag]),
        query_parameters_distance     = (dist // [dotProductFlag]),
        query_parameters_npoints      = (ptsNN // 10),
        query_parameters_ntracks      = (resultLen // 10) }

      rfnFlgs = catJustRfnParams (incl, excl, rad, absThrsh, relThrsh, durRat, qHopSz, iHopSz)
      refine = ADBQueryRefine {
        query_refine_flags              = rfnFlgs,
        query_refine_include            = (incl // emptyADBKeyList),
        query_refine_exclude            = (excl // emptyADBKeyList),
        query_refine_radius             = (rad // 1.0),
        query_refine_absolute_threshold = (absThrsh // 0),
        query_refine_relative_threshold = (relThrsh // 0),
        query_refine_duration_ratio     = (durRat // 0),
        query_refine_qhopsize           = fr (qHopSz // 1.0),
        query_refine_ihopsize           = fr (iHopSz // 1.0) }
      querySpec = ADBQuerySpec {
        query_spec_qid    = qid,
        query_spec_params = params,
        query_spec_refine = refine }

  if (queryid_sequence_start qid) + (queryid_sequence_length qid) <= (datum_nvectors datum)
    then poke qPtr querySpec
    else throw $ QuerySequenceBoundsException (queryid_sequence_start qid) (queryid_sequence_length qid) (datum_nvectors datum)

withQueryPtr :: (Ptr ADB) -> QueryAllocator -> (ADBQuerySpecPtr -> IO a) -> IO a
withQueryPtr adb allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             datum <- peek qPtr >>= return . queryid_datum . query_spec_qid >>= peek
             dimOk <- checkDimensions adb datum
             (f qPtr))

unsafeWithQueryPtr :: (Ptr ADB) -> QueryAllocator -> (ADBQuerySpecPtr -> IO a) -> IO a
unsafeWithQueryPtr adb allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             (f qPtr))

applyQueryPtr :: (Ptr ADB) -> (ADBQuerySpecPtr -> IO a) -> QueryAllocator -> IO a
applyQueryPtr adb f allocQuery = withQueryPtr adb allocQuery f

unsafeApplyQueryPtr :: (Ptr ADB) -> (ADBQuerySpecPtr -> IO a) -> QueryAllocator -> IO a
unsafeApplyQueryPtr adb f allocQuery = unsafeWithQueryPtr adb allocQuery f

withQuery :: (Ptr ADB) -> QueryAllocator -> (ADBQuerySpec -> IO a) -> IO a
withQuery adb allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             q <- peek qPtr
             datum <- (return . queryid_datum . query_spec_qid) q >>= peek
             dimOk <- checkDimensions adb datum
             (f q))

unsafeWithQuery :: (Ptr ADB) -> QueryAllocator -> (ADBQuerySpec -> IO a) -> IO a
unsafeWithQuery adb allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             q <- peek qPtr
             (f q))

applyQuery :: (Ptr ADB) -> (ADBQuerySpec -> IO a) -> QueryAllocator -> IO a
applyQuery adb f allocQuery = withQuery adb allocQuery f

unsafeApplyQuery :: (Ptr ADB) -> (ADBQuerySpec -> IO a) -> QueryAllocator -> IO a
unsafeApplyQuery adb f allocQuery = unsafeWithQuery adb allocQuery f

-- A 'detached query' is a query that's not associated with an
-- ADB. It's used for query manipulation.
withDetachedQueryPtr :: QueryAllocator -> (ADBQuerySpecPtr -> IO a) -> IO a
withDetachedQueryPtr allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             (f qPtr))

applyDetachedQueryPtr :: (ADBQuerySpecPtr -> IO a) -> QueryAllocator -> IO a
applyDetachedQueryPtr f allocQuery = withDetachedQueryPtr allocQuery f

withDetachedQuery :: QueryAllocator -> (ADBQuerySpec -> IO a) -> IO a
withDetachedQuery allocQuery f =
  alloca (\qPtr -> do
             allocQuery qPtr
             q <- peek qPtr
             (f q))

applyDetachedQuery :: (ADBQuerySpec -> IO a) -> QueryAllocator -> IO a
applyDetachedQuery f allocQuery = withDetachedQuery allocQuery f

freeDatum :: ADBDatumPtr -> IO ()
freeDatum datumPtr = when (datumPtr /= nullPtr) $ free datumPtr

freeQSpecDatum :: ADBQuerySpecPtr -> IO ()
freeQSpecDatum qSpecPtr = do
  qSpec    <- peek qSpecPtr
  freeDatum $ (queryid_datum . query_spec_qid) qSpec

freeDatumFromAllocator :: QueryAllocator -> IO ()
freeDatumFromAllocator qAlloc = withDetachedQueryPtr qAlloc freeQSpecDatum

peekDatum :: ADBDatumPtr -> IO ADBDatum
peekDatum datumPtr = do
  d <- peek datumPtr
  return d

saveDatum :: ADBDatumPtr -> IO ADBDatum
saveDatum datumPtr = do
  d <- peek datumPtr
  freeDatum datumPtr
  return d

querySinglePass :: (Ptr ADB) -> QueryAllocator -> IO ADBQueryResults
querySinglePass adb allocQuery =
  unsafeWithQueryPtr adb allocQuery $ \qPtr -> do
    r <- audiodb_query_spec adb qPtr
    freeQSpecDatum qPtr
    peek r >>= return

querySinglePassPtr :: (Ptr ADB) -> QueryAllocator -> IO ADBQueryResultsPtr
querySinglePassPtr adb allocQuery =
  unsafeWithQueryPtr adb allocQuery $ \qPtr -> do
    r <- audiodb_query_spec adb qPtr
    freeQSpecDatum qPtr
    return r

withResults :: ADBQueryResultsPtr -> (ADBQueryResults -> IO a) -> IO a
withResults rPtr f = do
  r <- peek rPtr
  (f r)

applyResults :: (ADBQueryResults -> IO a) -> ADBQueryResultsPtr -> IO a
applyResults f rPtr = withResults rPtr f

reverseResults :: ADBQueryResults -> ADBQueryResults
reverseResults r = r { query_results_results = reverse $ query_results_results r }

reverseResultsPtr :: ADBQueryResultsPtr -> IO ()
reverseResultsPtr rPtr = do
  r <- peek rPtr
  poke rPtr $ reverseResults r

queryStart :: (Ptr ADB) -> ADBQuerySpecPtr -> IO ADBQueryResultsPtr
queryStart adb qPtr = audiodb_query_spec adb qPtr

queryStep :: (Ptr ADB) -> ADBQuerySpecPtr -> ADBQueryResultsPtr -> IO ADBQueryResultsPtr
queryStep adb qPtr res = audiodb_query_spec_given_sofar adb qPtr res

thenElseIfM :: (Monad m) => m a -> m a -> Bool -> m a
thenElseIfM t f p = if p then t else f

-- NOTE: This collection of multi-pass query execution functions make
-- the single call to initQ (which is always the queryStart function)
-- with i = 0; then the *first* call to iterQ will be made with i =
-- 1. So you never get a call to iterQ with i = 0. Consequently, when
-- you want to use the step argument to a QueryAllocator,
-- QueryTransformer, or QueryComplete function, you need to bear in
-- mind that you start from 1, at the second iteration.

queryWithCallbackPtr :: (Ptr ADB) -> QueryAllocator -> QueryCallback a -> QueryComplete -> IO ADBQueryResultsPtr
queryWithCallbackPtr adb alloc callback isFinished =
  unsafeWithQueryPtr adb alloc (\qPtr -> do
                             let iteration = 0
                                 initQ _   = queryStart adb qPtr
                                 stepQ i r = callback i r >> queryStep adb qPtr r >>= iterQ (i + 1)
                                 iterQ i r = isFinished i alloc r >>= thenElseIfM (do { freeQSpecDatum qPtr; return r }) (stepQ i r)
                             r0 <- initQ iteration
                             iterQ (iteration + 1) r0)

queryWithCallback :: (Ptr ADB) -> QueryAllocator -> QueryCallback a -> QueryComplete -> IO ADBQueryResults
queryWithCallback adb alloc callback isFinished =
  queryWithCallbackPtr adb alloc callback isFinished >>= peek

queryWithTransformPtr :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryComplete -> IO ADBQueryResultsPtr
queryWithTransformPtr adb alloc transform complete = do
  let iteration   = 0
      initQ _     = unsafeWithQueryPtr adb alloc (\qPtr -> queryStart adb qPtr)
      stepQ i a r = unsafeWithQueryPtr adb a (\qPtr -> queryStep adb qPtr r) >>= iterQ (i + 1) a
      iterQ i a r = complete i a r >>= thenElseIfM (do { freeDatumFromAllocator a; return r }) (stepQ i (transform i r a) r)
  r0 <- initQ iteration
  iterQ (iteration + 1) alloc r0

queryWithTransform :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryComplete -> IO ADBQueryResults
queryWithTransform adb alloc transform complete =
  queryWithTransformPtr adb alloc transform complete >>= peek

queryWithCallbacksAndTransformPtr :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryCallback a -> QueryComplete -> IO ADBQueryResultsPtr
queryWithCallbacksAndTransformPtr adb alloc transform callback complete = do
  let iteration   = 0
      initQ _     = unsafeWithQueryPtr adb alloc (\qPtr -> queryStart adb qPtr)
      stepQ i a r = callback i r >> unsafeWithQueryPtr adb a (\qPtr -> queryStep adb qPtr r) >>= iterQ (i + 1) a
      iterQ i a r = complete i a r >>= thenElseIfM (do { freeDatumFromAllocator a; return r }) (stepQ i (transform i r a) r)
  r0 <- initQ iteration
  iterQ (iteration + 1) alloc r0

queryWithCallbacksAndTransform :: (Ptr ADB) -> QueryAllocator -> QueryTransformer -> QueryCallback a -> QueryComplete -> IO ADBQueryResults
queryWithCallbacksAndTransform adb alloc transform callback complete =
  queryWithCallbacksAndTransformPtr adb alloc transform callback complete >>= peek

queryPtr :: (Ptr ADB) -> QueryAllocator -> Maybe QueryTransformer -> Maybe (QueryCallback a) -> Maybe QueryComplete -> IO ADBQueryResultsPtr
queryPtr adb alloc Nothing          Nothing         Nothing           = querySinglePassPtr adb alloc
queryPtr adb alloc (Just transform) Nothing         (Just isFinished) = queryWithTransformPtr adb alloc transform isFinished
queryPtr adb alloc Nothing          (Just callback) (Just isFinished) = queryWithCallbackPtr adb alloc callback isFinished
queryPtr adb alloc (Just transform) (Just callback) (Just isFinished) = queryWithCallbacksAndTransformPtr adb alloc transform callback isFinished
queryPtr adb alloc Nothing          Nothing         (Just isFinished) = error "QueryComplete requires QueryTransformer and/or QueryCallback"
queryPtr adb alloc (Just transform) Nothing         Nothing           = error "QueryTransform requires QueryComplete"
queryPtr adb alloc Nothing          (Just callback) Nothing           = error "QueryCallback requires QueryComplete"
queryPtr adb alloc (Just transform) (Just callback) Nothing           = error "QueryTransform and QueryCallback requires QueryComplete"

query :: (Ptr ADB) -> QueryAllocator -> Maybe QueryTransformer -> Maybe (QueryCallback a) -> Maybe QueryComplete -> IO ADBQueryResults
query adb alloc transform callback isFinished =
  queryPtr adb alloc transform callback isFinished >>= peek

mkPointQuery :: ADBDatum   -- query features
                -> FeatureRate
                -> FrameSize
                -> Int         -- number of point nearest neighbours
                -> ADBQuerySpecPtr
                -> IO ()
mkPointQuery datum secToFrames framesToSec ptsNN =
  mkQuery datum (Just secToFrames) (Just 0) (Just datumLen) (Just []) (Just [databaseFlag]) (Just [dotProductFlag]) (Just ptsNN) (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  where datumLen = framesToSec (datum_nvectors datum)

mkTrackQuery :: ADBDatum    -- query features
                -> Int         -- number of tracks
                -> ADBQuerySpecPtr
                -> IO ()
mkTrackQuery = undefined

mkSequenceQuery :: ADBDatum  -- query features
                   -> FeatureRate
                   -> Int         -- number of point nearest neighbours
                   -> Int         -- number of tracks
                   -> Seconds     -- sequence start
                   -> Seconds     -- sequence length
                   -> Maybe [DistanceFlag]
                   -> Maybe Double -- absolute power threshold
                   -> Seconds      -- query hop size
                   -> Seconds      -- instance hop size
                   -> ADBQuerySpecPtr
                   -> IO ()
mkSequenceQuery datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qHopSize iHopSize qPtr =
  mkQuery datum (Just secToFrames) (Just sqStart) (Just sqLen) (Just [exhaustiveFlag]) (Just [perTrackFlag]) (dist ||| [euclideanNormedFlag]) (Just ptsNN) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing (Just qHopSize) (Just iHopSize) qPtr

execSequenceQuery :: (Ptr ADB)
                     -> ADBDatum -- query features
                     -> FeatureRate
                     -> Int         -- number of point nearest neighbours
                     -> Int         -- number of tracks
                     -> Seconds     -- sequence start
                     -> Seconds     -- sequence length
                     -> Maybe [DistanceFlag]
                     -> Maybe Double -- absolute power threshold
                     -> Seconds      -- query hop size
                     -> Seconds      -- instance hop size
                     -> IO ADBQueryResults
execSequenceQuery adb datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qHopSize iHopSize =
  querySinglePass adb (mkSequenceQuery slicedDatum secToFrames ptsNN resultLen 0 sqLen dist absThrsh qHopSize iHopSize)
  where
    slicedDatum = case datumSlice datum secToFrames sqStart sqLen of
      Right d -> d
      Left x  -> throw x

mkSequencePerTrackQuery :: ADBDatum    -- query features
                           -> FeatureRate
                           -> Int         -- number of tracks
                           -> Seconds     -- sequence start
                           -> Seconds     -- sequence length
                           -> Maybe [DistanceFlag]
                           -> Maybe Double -- absolute power threshold
                           -> ADBQuerySpecPtr
                           -> IO ()
mkSequencePerTrackQuery datum secToFrames resultLen sqStart sqLen dist absThrsh qPtr =
  mkQuery datum (Just secToFrames) (Just sqStart) (Just sqLen) Nothing (Just [perTrackFlag]) (dist ||| [euclideanNormedFlag]) (Just 1) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing Nothing Nothing qPtr

execSequencePerTrackQuery :: (Ptr ADB)
                             -> ADBDatum -- query features
                             -> FeatureRate
                             -> Int         -- number of tracks
                             -> Seconds     -- sequence start
                             -> Seconds     -- sequence length
                             -> Maybe [DistanceFlag]
                             -> Maybe Double -- absolute power threshold
                             -> IO ADBQueryResults
execSequencePerTrackQuery adb datum secToFrames resultLen sqStart sqLen dist absThrsh =
  querySinglePass adb (mkSequencePerTrackQuery datum secToFrames resultLen sqStart sqLen dist absThrsh)

transformSequencePerTrackQuery :: (ADBDatum -> ADBDatum)     -- query features
                                  -> FeatureRate
                                  -> FrameSize
                                  -> (Int -> Int)                     -- number of tracks
                                  -> (Seconds -> Seconds)             -- sequence start
                                  -> (Seconds -> Seconds)             -- sequence length
                                  -> (Maybe [DistanceFlag] -> Maybe [DistanceFlag])
                                  -> (Maybe Double -> Maybe Double)   -- absolute power threshold)
                                  -> ADBQueryResultsPtr
                                  -> QueryAllocator
                                  -> ADBQuerySpecPtr
                                  -> IO ()
transformSequencePerTrackQuery tDatum secToFrames framesToSec tResultLen tSqStart tSqLen tDist tAbsThrsh resPtr fromAlloc = newAlloc
  where
    newAlloc toPtr = withDetachedQueryPtr fromAlloc $ \fromPtr -> do
      q <- peek fromPtr
      let datumPtr = (queryid_datum . query_spec_qid) q
      datum <- peek datumPtr

      let datum'    = tDatum datum
          resultLen = tResultLen $ (query_parameters_ntracks . query_spec_params) q
          sqStart   = (withSeconds secToFrames framesToSec tSqStart ((queryid_sequence_start . query_spec_qid) q))
          sqLen     = (withSeconds secToFrames framesToSec tSqLen ((queryid_sequence_length . query_spec_qid) q))
          dist      = tDist      $ Just $ (query_parameters_distance . query_spec_params) q
          absThrsh  = tAbsThrsh  $ Just $ (query_refine_absolute_threshold . query_spec_refine) q
      putStrLn "transformSequencePerTrackQuery calling mkSequencePerTrackQuery"
      mkSequencePerTrackQuery datum' secToFrames resultLen (framesToSec sqStart) (framesToSec sqLen) dist absThrsh toPtr

mkNSequenceQuery :: ADBDatum  -- query features
                    -> FeatureRate
                    -> Int         -- number of point nearest neighbours
                    -> Int         -- number of tracks
                    -> Seconds     -- sequence length
                    -> Maybe [DistanceFlag]
                    -> Maybe Double -- absolute power threshold
                    -> Seconds      -- query hop size
                    -> Seconds      -- instance hop size
                    -> ADBQuerySpecPtr
                    -> IO ()
mkNSequenceQuery datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize qPtr =
  mkQuery datum (Just secToFrames) (Just 0) (Just sqLen) (Just [exhaustiveFlag]) (Just [perTrackFlag]) (dist ||| [euclideanNormedFlag]) (Just ptsNN) (Just resultLen) Nothing Nothing Nothing (absThrsh ||| 0) Nothing Nothing (Just qHopSize) (Just iHopSize) qPtr

execNSequenceQuery :: (Ptr ADB)
                      -> ADBDatum -- query features
                      -> FeatureRate
                      -> Int         -- number of point nearest neighbours
                      -> Int         -- number of tracks
                      -> Seconds     -- sequence length
                      -> Maybe [DistanceFlag]
                      -> Maybe Double -- absolute power threshold
                      -> Seconds      -- query hop size
                      -> Seconds      -- instance hop size
                      -> IO ADBQueryResults
execNSequenceQuery adb datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize =
  querySinglePass adb (mkNSequenceQuery datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize)

mkOneToOneSequenceQuery :: ADBDatum  -- query features
                           -> ADBQuerySpecPtr
                           -> IO ()
mkOneToOneSequenceQuery = undefined

mkSequencePerTrackQueryDeltaNTracks :: FeatureRate
                                       -> FrameSize
                                       -> (Int -> Int)
                                       -> ADBQueryResultsPtr
                                       -> QueryAllocator
                                       -> ADBQuerySpecPtr
                                       -> IO ()
mkSequencePerTrackQueryDeltaNTracks secToFrames frameToSecs delta = transformSequencePerTrackQuery id secToFrames frameToSecs delta id id id id

rotateVector :: (DV.Storable a) => Int -> DV.Vector a -> DV.Vector a
rotateVector delta v = (DV.++) back front
  where (front, back) = DV.splitAt delta v

mapSlices :: (DV.Storable a) => (DV.Vector a -> DV.Vector a) -> Int -> DV.Vector a -> [DV.Vector a]
mapSlices f sliceLen values = mapSlice 0
  where mapSlice start
          | start + sliceLen <= (DV.length values) = (f (DV.slice start sliceLen values)) : mapSlice (start + sliceLen)
          | otherwise                              = []

rotateDatumPtr :: Int -> ADBDatumPtr -> IO ()
rotateDatumPtr delta datumPtr = do
  datum  <- peek datumPtr

  let values    = datum_data datum
      rotValues = DV.concat $ mapSlices (rotateVector delta) (datum_dim datum) values
      rotDatum  = datum { datum_data = rotValues }

  poke datumPtr rotDatum

rotateDatum :: Int -> ADBDatum -> ADBDatum
rotateDatum delta datum = datum { datum_data = rotValues }
  where
    values    = datum_data datum
    rotValues = DV.concat $ mapSlices (rotateVector delta) (datum_dim datum) values

mkSequenceQueryWithRotation :: ADBDatum        -- query features
                               -> FeatureRate
                               -> Int          -- number of point nearest neighbours
                               -> Int          -- number of tracks
                               -> Seconds      -- sequence start
                               -> Seconds      -- sequence length
                               -> Maybe [DistanceFlag]
                               -> Maybe Double -- absolute power threshold
                               -> Seconds      -- query hop size
                               -> Seconds      -- instance hop size
                               -> [Int]        -- rotations
                               -> (QueryAllocator, QueryTransformer, QueryComplete)
mkSequenceQueryWithRotation datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qHopSize iHopSize rotations = (alloc, transform, isFinished)
  where
    alloc            = mkSequenceQuery slicedDatum secToFrames ptsNN resultLen 0 sqLen dist absThrsh qHopSize iHopSize
    transform i r a  = mkSequenceQuery (rotateDatum (rotations!!(i - 1)) slicedDatum) secToFrames ptsNN resultLen 0 sqLen dist absThrsh qHopSize iHopSize
    isFinished i _ r = return $ i > (length rotations)

    slicedDatum = case datumSlice datum secToFrames sqStart sqLen of
      Right d -> d
      Left x  -> throw x


execSequenceQueryWithRotation :: (Ptr ADB)
                                 -> ADBDatum     -- query features
                                 -> FeatureRate
                                 -> Int          -- number of point nearest neighbours
                                 -> Int          -- number of tracks
                                 -> Seconds      -- sequence length
                                 -> Seconds      -- sequence length
                                 -> Maybe [DistanceFlag]
                                 -> Maybe Double -- absolute power threshold
                                 -> Seconds      -- query hop size
                                 -> Seconds      -- instance hop size
                                 -> [Int]        -- rotations
                                 -> IO ADBQueryResults
execSequenceQueryWithRotation adb datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qHopSize iHopSize rotations =
  queryWithTransform adb alloc transform isFinished
  where (alloc, transform, isFinished) = mkSequenceQueryWithRotation datum secToFrames ptsNN resultLen sqStart sqLen dist absThrsh qHopSize iHopSize rotations

mkSequencePerTrackQueryWithRotation :: ADBDatum  -- query features
                                       -> FeatureRate
                                       -> Int          -- number of tracks
                                       -> Seconds      -- sequence start
                                       -> Seconds      -- sequence length
                                       -> Maybe [DistanceFlag]
                                       -> Maybe Double -- absolute power threshold
                                       -> [Int]        -- rotations
                                       -> (QueryAllocator, QueryTransformer, QueryComplete)
mkSequencePerTrackQueryWithRotation datum secToFrames resultLen sqStart sqLen dist absThrsh rotations = (alloc, transform, isFinished)
  where
    alloc            = mkSequencePerTrackQuery datum secToFrames resultLen sqStart sqLen dist absThrsh
    transform i r a  = mkSequencePerTrackQuery (rotateDatum (rotations!!(i - 1)) datum) secToFrames resultLen sqStart sqLen dist absThrsh
    isFinished i _ r = return $ i > (length rotations)

execSequencePerTrackQueryWithRotation :: (Ptr ADB)
                                         -> ADBDatum  -- query features
                                         -> FeatureRate
                                         -> Int          -- number of tracks
                                         -> Seconds      -- sequence start
                                         -> Seconds      -- sequence length
                                         -> Maybe [DistanceFlag]
                                         -> Maybe Double -- absolute power threshold
                                         -> [Int]        -- rotations
                                         -> IO ADBQueryResults
execSequencePerTrackQueryWithRotation adb datum secToFrames resultLen sqStart sqLen dist absThrsh rotations =
  queryWithTransform adb alloc transform isFinished
  where (alloc, transform, isFinished) = mkSequencePerTrackQueryWithRotation datum secToFrames resultLen sqStart sqLen dist absThrsh rotations

mkNSequenceQueryWithRotation :: ADBDatum        -- query features
                                -> FeatureRate
                                -> Int          -- number of point nearest neighbours
                                -> Int          -- number of tracks
                                -> Seconds      -- sequence length
                                -> Maybe [DistanceFlag]
                                -> Maybe Double -- absolute power threshold
                                -> Seconds      -- query hop size
                                -> Seconds      -- instance hop size
                                -> [Int]        -- rotations
                                -> (QueryAllocator, QueryTransformer, QueryComplete)
mkNSequenceQueryWithRotation datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize rotations = (alloc, transform, isFinished)
  where
    alloc            = mkNSequenceQuery datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize
    transform i r a  = mkNSequenceQuery (rotateDatum (rotations!!(i - 1)) datum) secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize
    isFinished i _ r = return $ i > (length rotations)

execNSequenceQueryWithRotation :: (Ptr ADB)
                                  -> ADBDatum     -- query features
                                  -> FeatureRate
                                  -> Int          -- number of point nearest neighbours
                                  -> Int          -- number of tracks
                                  -> Seconds      -- sequence length
                                  -> Maybe [DistanceFlag]
                                  -> Maybe Double -- absolute power threshold
                                  -> Seconds      -- query hop size
                                  -> Seconds      -- instance hop size
                                  -> [Int]        -- rotations
                                  -> IO ADBQueryResults
execNSequenceQueryWithRotation adb datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize rotations =
  queryWithTransform adb alloc transform isFinished
  where (alloc, transform, isFinished) = mkNSequenceQueryWithRotation datum secToFrames ptsNN resultLen sqLen dist absThrsh qHopSize iHopSize rotations
