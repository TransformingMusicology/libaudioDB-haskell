module Main where

import qualified Data.Vector.Storable as V
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Numeric
import           Sound.Audio.Database
import           Sound.Audio.Database.Ingest
import           Sound.Audio.Database.Query
import           Sound.Audio.Database.Types
import           Sound.Audio.Features.ReadCSV
import           Sound.Audio.Features.ReadTurtle

showDatum :: ADBDatum -> String
showDatum datum =
  show "Key: " ++ (show (datum_key datum)) ++
  "; nVectors: " ++ (show (datum_nvectors datum)) ++
  "; dim: " ++ (show (datum_dim datum)) ++
  "; 100 features: " ++ (show (V.take 100 (datum_data datum))) ++
  "; 100 times: " ++ (show (maybe (V.fromList [0]) (\t -> (V.take 100 t)) (datum_times datum)))

test_readCSVFeatures :: String -> FilePath -> IO ()
test_readCSVFeatures key fp = do
  datum <- readCSVFeaturesTimes key fp
  maybe
    (putStrLn "Could not parse.")
    (\d -> putStrLn $ showDatum d)
    datum

test_readTurtleFeatures :: String -> FilePath -> IO ()
test_readTurtleFeatures key fp = do
  datum <- readTurtleFeaturesOnly key fp
  maybe
    (putStrLn "Could not parse.")
    (\d -> putStrLn $ showDatum d)
    datum

sample_rate :: Int
sample_rate = 44100

step_size :: Int
step_size = 2048

framesPerSecond :: Double
framesPerSecond = sr / ss
  where sr = fromIntegral sample_rate
        ss = fromIntegral step_size

framesToSeconds :: FrameSize
framesToSeconds f = (fromIntegral f) / framesPerSecond

showResults :: ADBQueryResults -> String
showResults r =
  (show n) ++ " hits:\n" ++ unlines (map showResult results)
  where
    n       = (query_results_nresults r)
    results = (query_results_results r)

showResult :: ADBResult -> String
showResult r =
  q ++ " (@ " ++ (qp ") is in track ") ++ k ++ " @ " ++ (pos "; distance is ") ++ (dist "")
  where
    q    = (result_qkey r)
    qp   = showFFloat nd (framesToSeconds (result_qpos r))
    k    = (result_ikey r)
    pos  = showFFloat nd (framesToSeconds (result_ipos r))
    dist = showFFloat nd ((result_dist r))
    nd   = Just 2

test_slice_datum :: FilePath -> String -> Seconds -> Seconds -> IO ()
test_slice_datum adbFN key start len =
  withExistingROAudioDB adbFN runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFN)
    runTestOnDB (Just adb) = withMaybeDatumSliceIO adb key featureRate start len printDatum

    printDatum Nothing      = putStrLn "Could not slice datum"
    printDatum (Just datum) = putStrLn $ showDatum datum

    featureRate = floor . (* framesPerSecond)

test_create_insert_synthetic :: FilePath -> IO ()
test_create_insert_synthetic adbFN =
  withNewL2NormedPoweredAudioDB adbFN 0 0 1 testDB
  where
    featureKey = "Test"
    features   = V.fromList [1.0]
    power      = Just (V.fromList [0.0])
    times      = Nothing
    datum      = ADBDatum { datum_nvectors = 1,
                            datum_dim      = 1,
                            datum_key      = featureKey,
                            datum_data     = features,
                            datum_power    = power,
                            datum_times    = times }
    testDB Nothing    = putStrLn $ "Could not create database: " ++ adbFN
    testDB (Just adb) = do
      inserted <- insertFeatures adb datum
      putStrLn $ "Inserted '" ++ featureKey ++ "': " ++ (show inserted)

test_create_insert :: FilePath -> FilePath -> String -> Int -> IO ()
test_create_insert adbFN featureFN featureKey dbDim =
  withNewAudioDB adbFN 0 0 dbDim False False testDB
  where
    testDB Nothing    = putStrLn $ "Could not create database: " ++ adbFN
    testDB (Just adb) = do
      datum    <- readCSVFeaturesTimes featureKey featureFN
      inserted <- insertMaybeFeatures adb datum
      putStrLn $ "Inserted '" ++ featureKey ++ "': " ++ (show inserted)

test_insert :: FilePath -> FilePath -> String -> Int -> IO ()
test_insert adbFN featureFN featureKey dbDim = do
  withNewAudioDB adbFN 0 0 dbDim False False checkCreated
  withExistingAudioDB adbFN testDB
  where
    checkCreated Nothing = putStrLn $ "Could not create database: " ++ adbFN
    checkCreated _       = (putStrLn $ "Created new database: " ++ adbFN) >> return ()

    testDB Nothing    = putStrLn $ "Could not open database: " ++ adbFN
    testDB (Just adb) = do
      datum    <- readCSVFeaturesTimes featureKey featureFN
      inserted <- insertMaybeFeatures adb datum
      putStrLn $ "Inserted '" ++ featureKey ++ "': " ++ (show inserted)

test_create_insert_slice :: FilePath -> FilePath -> String -> Int -> Seconds -> Seconds -> IO ()
test_create_insert_slice adbFN featureFN featureKey dbDim from to =
  withNewAudioDB adbFN 0 0 dbDim False False testDB
  where
    testDB Nothing    = putStrLn $ "Could not create database: " ++ adbFN
    testDB (Just adb) = do
      datum    <- readCSVFeaturesTimes featureKey featureFN
      inserted <- insertMaybeFeaturesSlice adb datum featureRate from (to - from)
      putStrLn $ "Inserted '" ++ featureKey ++ "': " ++ (show inserted)
    featureRate = floor . (* framesPerSecond)

test_insert_slice :: FilePath -> FilePath -> String -> Int -> Seconds -> Seconds -> IO ()
test_insert_slice adbFN featureFN featureKey dbDim from to =
  withExistingAudioDB adbFN testDB
  where
    testDB Nothing    = putStrLn $ "Could not open database: " ++ adbFN
    testDB (Just adb) = do
      datum    <- readCSVFeaturesTimes featureKey featureFN
      inserted <- insertMaybeFeaturesSlice adb datum featureRate from (to - from)
      putStrLn $ "Inserted '" ++ featureKey ++ "': " ++ (show inserted)
    featureRate = floor . (* framesPerSecond)

test_sequence_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> Int -> Int -> Seconds -> IO ()
test_sequence_query adbFile queryFile qPowersFile start len numTracks pointsPerTrack hopSize = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      res <- execSequenceQuery adb datum (floor . (* framesPerSecond)) pointsPerTrack numTracks start len (Just [euclideanNormedFlag]) query_abs_power_thrsh hopSize hopSize
      putStrLn (showResults (reverseResults res))

test_sequencepertrack_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_sequencepertrack_query adbFile queryFile qPowersFile start len =
  withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      res <- execSequencePerTrackQuery adb datum (floor . (* framesPerSecond)) 25 start len (Just [euclideanNormedFlag]) Nothing
      putStrLn (showResults (reverseResults res))

test_nsequence_query :: FilePath -> FilePath -> FilePath -> Seconds -> Int -> Int -> Seconds -> IO ()
test_nsequence_query adbFile queryFile qPowersFile len numTracks pointsPerTrack hopSize = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      res <- execNSequenceQuery adb datum (floor . (* framesPerSecond)) pointsPerTrack numTracks len (Just [euclideanNormedFlag]) query_abs_power_thrsh hopSize hopSize
      putStrLn (showResults (reverseResults res))

test_callback_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_callback_query adbFile queryFile qPowersFile start len = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      let ntracks          = query_parameters_ntracks . query_spec_params
          qAlloc           = mkSequencePerTrackQuery datum (floor . (* framesPerSecond)) 5 start len (Just [euclideanNormedFlag]) Nothing
          isFinished _ _ _ = putStrLn "isFinished..." >> return False -- withQueryPtr adb a (\qPtr -> do { q <- peek qPtr; return $ ntracks q >= 25 })
          callback i r     = do
            res <- peek r
            n <- return $ query_results_nresults res
            putStrLn $ "Callback #" ++ (show i) ++ " says: " ++ (show n)
            putStrLn (showResults (reverseResults res))
            return n

      _ <- queryWithCallback adb qAlloc callback isFinished
      return ()

test_transform_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_transform_query adbFile queryFile qPowersFile start len = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      let ntracks          = query_parameters_ntracks . query_spec_params
          qAlloc           = mkSequencePerTrackQuery datum (floor . (* framesPerSecond)) 5 start len (Just [euclideanNormedFlag]) Nothing
          isFinished _ _ r = withResults r (\res -> return $ (query_results_nresults res) >= 20)
          transform _ r a  = mkSequencePerTrackQueryDeltaNTracks (floor . (* framesPerSecond)) framesToSeconds (\x -> x + 5) r a

      res <- queryWithTransform adb qAlloc transform isFinished
      putStrLn "Final results:"
      putStrLn $ showResults (reverseResults res)

test_callbacktransform_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> IO ()
test_callbacktransform_query adbFile queryFile qPowersFile start len = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      let ntracks          = query_parameters_ntracks . query_spec_params
          qAlloc           = mkSequencePerTrackQuery datum (floor . (* framesPerSecond)) 5 start len (Just [euclideanNormedFlag]) Nothing
          isFinished _ a _ = withQuery adb a (\q -> do { return $ (ntracks q) >= 20 }) --withResults r (\res -> return $ (query_results_nresults res) >= 20)
          callback i r     = withResults r (\res -> do { putStrLn $ "#" ++ (show i) ++ ": " ++ (showResults res); return $ (query_results_nresults res) })
          transform _ r a  = mkSequencePerTrackQueryDeltaNTracks (floor . (* framesPerSecond)) framesToSeconds (\x -> x + 5) r a

      res <- queryWithCallbacksAndTransform adb qAlloc transform callback isFinished
      putStrLn "Final results:"
      putStrLn $ showResults (reverseResults res)

test_seq_rotation_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> Int -> Int -> Seconds -> [Int] -> IO ()
test_seq_rotation_query adbFile queryFile qPowersFile start len numTracks pointsPerTrack hopSize rotations = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      res <- execSequenceQueryWithRotation adb datum (floor . (* framesPerSecond)) pointsPerTrack numTracks start len (Just [euclideanNormedFlag]) query_abs_power_thrsh hopSize hopSize rotations
      putStrLn (showResults (reverseResults res))

test_seqpertrack_rotation_query :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> [Int] -> IO ()
test_seqpertrack_rotation_query adbFile queryFile qPowersFile start len rotations = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      res <- execSequencePerTrackQueryWithRotation adb datum (floor . (* framesPerSecond)) 25 start len (Just [euclideanNormedFlag]) Nothing rotations
      putStrLn (showResults (reverseResults res))

test_nseq_rotation_query :: FilePath -> FilePath -> FilePath -> Seconds -> Int -> Int -> Seconds -> [Int] -> IO ()
test_nseq_rotation_query adbFile queryFile qPowersFile len numTracks pointsPerTrack hopSize rotations = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      res <- execNSequenceQueryWithRotation adb datum (floor . (* framesPerSecond)) pointsPerTrack numTracks len (Just [euclideanNormedFlag]) query_abs_power_thrsh hopSize hopSize rotations
      putStrLn (showResults (reverseResults res))

test_polymorphic_seqpertrack_query_with_rotations :: FilePath -> FilePath -> FilePath -> Seconds -> Seconds -> [Int] -> IO ()
test_polymorphic_seqpertrack_query_with_rotations adbFile queryFile qPowersFile start len rotations = withExistingROAudioDB adbFile runTestOnDB
  where
    runTestOnDB Nothing    = putStrLn $ "Could not open " ++ (show adbFile)
    runTestOnDB (Just adb) = readCSVFeaturesTimesPowers test_features_name queryFile qPowersFile >>= testQuery adb

    testQuery _ Nothing = putStrLn $ "Could not parse " ++ queryFile
    testQuery adb (Just datum) = do
      let (qAlloc, qTransform, qComplete) = mkSequencePerTrackQueryWithRotation datum (floor . (* framesPerSecond)) 25 start len (Just [euclideanNormedFlag]) Nothing rotations
      res <- query adb qAlloc (Just qTransform) Nothing (Just qComplete)
      putStrLn $ showResults res

new_db_file :: String
new_db_file = undefined

db_file :: String
db_file = undefined

test_features_name :: String
test_features_name = undefined

test_features_file :: String
test_features_file = undefined

test_features_dim :: Int
test_features_dim = undefined

test_power_features_file :: String
test_power_features_file = undefined

query_seq_start :: Seconds
query_seq_start = undefined

query_seq_length :: Seconds
query_seq_length = undefined

query_hop_size :: Seconds
query_hop_size = undefined

query_abs_power_thrsh :: Maybe Double
query_abs_power_thrsh = Nothing

main :: IO ()
main = do
  -- test_readCSVFeatures test_features_name test_features_file
  -- test_readTurtleFeatures test_features_name test_features_file
  -- test_slice_datum db_file query_key query_seq_start query_seq_length

  -- test_create_insert_synthetic new_db_file
  -- test_create_insert new_db_file test_features_file test_features_name test_features_dim
  -- test_insert new_db_file test_features_file test_features_name test_features_dim
  -- test_create_insert_slice new_db_file test_features_file test_features_name test_features_dim 10.0 20.0
  -- test_insert_slice new_db_file test_features_file test_features_name test_features_dim 250.0 350.0
  -- test_sequence_query db_file test_features_file test_power_features_file query_seq_start query_seq_length 25 20 query_hop_size
  -- test_sequencepertrack_query db_file test_features_file test_power_features_file query_seq_start query_seq_length
  -- test_nsequence_query db_file test_features_file test_power_features_file query_seq_length 25 20 query_hop_size
  -- test_transform_query db_file test_features_file test_power_features_file query_seq_start query_seq_length
  -- test_callbacktransform_query db_file test_features_file test_power_features_file query_seq_start query_seq_length
  -- test_seq_rotation_query db_file test_features_file test_power_features_file query_seq_start query_seq_length 7 30 query_hop_size [1..11]
  -- test_seqpertrack_rotation_query db_file test_features_file test_power_features_file query_seq_start query_seq_length [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  -- test_nseq_rotation_query db_file test_features_file test_power_features_file query_seq_length 38 30 query_hop_size [1..11]
  -- test_polymorphic_seqpertrack_query_with_rotations db_file test_features_file test_power_features_file query_seq_start query_seq_length [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  -- test_nseq_rotation_query_from_key_slice db_file query_key query_seq_start query_seq_length 7 30 query_seq_length [1..11]

  putStrLn "Done."
