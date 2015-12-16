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

module Sound.Audio.Database.Ingest ( insertFeatures
                                   , insertFeaturesPtr
                                   , insertMaybeFeatures
                                   , insertMaybeFeaturesPtr ) where

import AudioDB.API
import Control.Exception (throwIO)
import Data.Maybe (isJust)
import Foreign (Ptr, peek, poke)
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Sound.Audio.Database

insertFeaturesPtr :: (Ptr ADB) -> ADBDatumPtr -> IO Bool
insertFeaturesPtr adb datumPtr =
  withADBStatus doInsert adb
  where
    doInsert status = do
      datum <- peek datumPtr
      res   <- if isPowered status == isJust (datum_power datum)
               then audiodb_insert_datum adb datumPtr
               else throwIO FeaturesDBPowerFlagNotSetException
      let success = res == (0 :: CInt)
      return success

insertFeatures :: (Ptr ADB) -> ADBDatum -> IO Bool
insertFeatures adb datum =
  withADBStatus doInsert adb
  where
    doInsert status = do
      res <- if isPowered status == isJust (datum_power datum)
             then alloca (\datumPtr -> do
                             poke datumPtr datum
                             audiodb_insert_datum adb datumPtr)
             else throwIO FeaturesDBPowerFlagNotSetException
      let success = res == (0 :: CInt)
      return success

insertMaybeFeaturesPtr :: (Ptr ADB) -> Maybe ADBDatumPtr -> IO Bool
insertMaybeFeaturesPtr adb (Just datumPtr) = insertFeaturesPtr adb datumPtr
insertMaybeFeaturesPtr abd Nothing         = return False

insertMaybeFeatures :: (Ptr ADB) -> Maybe ADBDatum -> IO Bool
insertMaybeFeatures adb (Just datum) = insertFeatures adb datum
insertMaybeFeatures adb Nothing      = return False
