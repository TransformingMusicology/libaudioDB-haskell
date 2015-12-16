## Haskell Bindings to libaudioDB

[audioDB](https://github.com/TransformingMusicology/audiodb) is a
feature vector database. It allows high dimensional features extracted
from media content (such as audio) to be stored and searched using an
approximate nearest neighbour algorithm, locality sensitive
hashing. This package provides low-level, and slightly higher-level
Haskell bindings to the `libaudioDB` library.

## Installation

1. Install
   [`libaudioDB`](https://github.com/TransformingMusicology/libaudiodb). On
   GNU/Linux, this is most likely a simple case of:

        $ make
        $ sudo make PREFIX=/usr/local install

   But will require that you have the headers for
   [`libgsl`](http://www.gnu.org/software/gsl/) installed (known to
   compile with versions 1.16 and 2.1).

   **WARNING**: Do *not* run the `make test` on MacOS! That operating
     system's default filesystem, HFS+, does not support sparse
     files. `libaudioDB` makes use of the sparse file feature of other
     filesystems (including the ext2-4 and NTFS filesystems) to allow
     its 4GB or more database files to occupy less actual disk space
     when they contain portions of null data. On MacOS, however, these
     files will really use 4GB of disk space. And the test suite
     creates 20 or 30 such files.

2. Install GHC and cabal-install. On operating systems that don't have
   proper software package management and curated archives, it's
   recommended to install the
   [Haskell Platform](https://www.haskell.org/platform/) as a
   convenient way of getting GHC and cabal-install. For Debian and
   derivatives, you should get away with:

        $ sudo apt-get install ghc cabal-install

3. Retrieve the `libaudioDB-haskell` source:

        $ git clone https://github.com/TransformingMusicology/libaudioDB-haskell.git

4. In the source directory, set up a Cabal sandbox:

        $ cabal sandbox init

5. Ensure your `libaudioDB` shared library and includes are visible to
   your C++ compiler. If you've installed them in a location in which
   your compiler looks by default (e.g. `/usr/include` for includes
   and `/usr/lib` for the shared library) then you shouldn't need to
   do anything. Otherwise, you need to edit the `AudioDB.cabal` file
   adding the lines:

        include-dirs: /home/you/.local/include
        extra-lib-dirs: /home/you/.local/lib

   (for example) to each of the targets (library and two executables).

6. Install the dependent Haskell libraries in the sandbox:

        $ cabal configure
        $ cabal install --only-dependencies

7. Build the AudioDB library:

        $ cabal build

8. Run the simple API test:

        $ cabal run api-test

   This runs the code in `tests/APITest.hs`. It will create a 2GB file
   called `test.adb` which you can delete afterwards. If you get to
   this point and it doesn't crash or complain that the library is
   missing then the gods are surely smiling on you and you should send
   me an [email](mailto:richard.lewis@gold.ac.uk).

## Creating a database

`libaudioDB` works with audio *features* (rather than raw audio). So
in order to build a database you will need to have extracted some
features from your audio files.

`libaudioDB-haskell` can read features stored in CSV files. The format
of such files is as follows. Each row represents one extracted feature
from a time window in the original audio file. The first column should
be a time stamp in seconds for that feature, encoded as a string
representation of a `double`. The subsequent columns are string
representations of `double`s which encode the value of each bin of the
feature. So for example, the
[NNLS Chroma](http://isophonics.net/nnls-chroma) feature (which we use
frequently in our research), by default gives you 12 bins for each
feature. So the CSV file would have 13 columns (one time stamp, and 12
feature bin values).

You can generate appropriate CSV feature files using the
[`sonic-annotator`](http://www.vamp-plugins.org/sonic-annotator/) tool
distributed by Queen Mary University of London as part of their Vamp
feature extraction toolkit. From the Vamp pages you can also find a
list of [plugins](http://www.vamp-plugins.org/download.html) which
implement a variety of audio features.

(There is some on-going work on a Haskell library which works as a
Vamp host, [HVamp](https://github.com/TransformingMusicology/HVamp), a
software component capable of executing Vamp plugins. If this ever
comes to fruition it would be possible to do the feature extraction
from Haskell and dispense with `sonic-annotator`.)

Once you have some feature files, you're ready to create a database
and insert your features. The follow expressions in an `IO` function
are *almost* what we need:

    datumPtr <- readCSVFeaturesTimes "Track01" "Track01.chroma.csv"
    inserted <- insertMaybeFeatures adb datumPtr

The function `readCSVFeaturesTimes :: String -> FilePath -> IO (Maybe
ADBDatumPtr)` takes a "key" for the feature file in the database
(i.e. a unique string to identify this feature file), and a filename,
and returns a `Maybe ADBDatumPtr` (in the `IO` monad). And then
`insertMaybeFeatures :: (Ptr ADB) -> (Maybe ADBDatumPtr) -> IO Bool`
takes a pointer to an `ADB`, a `Maybe ADBDatumPtr` (that's the thing
that `readCSVFeaturesTimes` gave us) and returns a `Bool` (in the `IO`
monad) indicating success or failure. The `ADBDatum` type is a record
type which collects together a `Storable` `Vector` of `Double`s
representing the features (actually concatenated together into a
single `Vector`) and some properties of those features including the
key, the dimensionality, and a `Storable` `Vector` of `Double`s
representing the time offsets. (NOTE: not discussed here is the option
to include so-called "power" features [effectively loudness] in a
database.)

The missing piece of the puzzle in the above code fragment is the
question of where does the `Ptr ADB` come from?  The
`Sound.Audio.Database` module exports a family of `with*AudioDB`
functions which have signatures along the lines of `FilePath -> (Maybe
(Ptr ADB) -> IO a) -> IO a`. The idea is that you supply the
`FilePath` of a database, and then an `IO` function which takes a
`Maybe (Ptr ADB)`, does something with that (`Maybe`) database handle
(if it can't be opened for some reason you get `Nothing`), and puts
some value of type `a` into the `IO` monad. The `with*AudioDB` then
returns that `a`-type value.

So to create a new database and insert some features, we can do:

    module Main where
    
    import Sound.Audio.Database
    import Sound.Audio.Database.Ingest
    import Sound.Audio.Features.ReadCSV
    
    main :: IO ()
    main = do
      withNewL2NormedAudioDB adbFN 0 0 dbDim testDB
        where
            adbFN      = "test.chroma.adb"
            dbDim      = 12
            featureKey = "Track01"
            featureFN  = "Track01.chroma.csv"
            testDB Nothing    = putStrLn $ "Could not create database: " ++ adbFN
            testDB (Just adb) = do
              datumPtr <- readCSVFeaturesTimes featureKey featureFN
              inserted <- insertMaybeFeatures adb datumPtr
              putStrLn $ "Inserted '" ++ featureKey ++ "': " ++ (show inserted)

(as can be seen in `tests/AudioDBTests.hs`).

## Running the "tests"

The `AudioDB.cabal` file includes two executable targets: `api-test`
and `audiodb-test`. The `api-test` is a simple couple of functions
which demonstrate (rather than rigorously *test*) some of the
functions from the (non-exported) `AudioDB.API` module. (Note that
these are *not* the tests that I warned you not to run on MacOS; this
code [probably] won't eat all your disk space.)

Similarly, `audiodb-test` comprises basic
proof-of-not-completely-brokenness of some of the functions from the
`Sound.Audio.Database.*` modules. In the version that's in the repo,
all of the test function calls in `main` are commented out. So to run
any of these "tests" you need to uncomment the ones you want to try,
and also supply some values to the constants:

    new_db_file :: String
    new_db_file = undefined
    
    test_features_name :: String
    test_features_name = undefined
    
    test_features_file :: String
    test_features_file = undefined
    
    test_features_dim :: Int
    test_features_dim = undefined
    
    test_power_features_file :: String
    test_power_features_file = undefined

These are sufficient for executing the `test_readCSVFeatures` and
`test_create_insert` functions.

    db_file :: String
    db_file = undefined
    
    query_seq_start :: Seconds
    query_seq_start = undefined
    
    query_seq_length :: Seconds
    query_seq_length = undefined
    
    query_hop_size :: Int
    query_hop_size = undefined

And these are required to execute any of the `test_*_query` functions.

## Doing some queries

TODO

## License

Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College

Author: richard.lewis@gold.ac.uk

libaudioDB-haskell is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

libaudioDB-haskell is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with libaudioDB-haskell. If not, see <http://www.gnu.org/licenses/>.
