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
                                   , insertMaybeFeatures) where

import AudioDB.API
import Data.Maybe (isJust)
import Foreign (Ptr, peek)
import Foreign.C.Types
import Sound.Audio.Database

insertFeatures :: (Ptr ADB) -> ADBDatumPtr -> IO Bool
insertFeatures adb datumPtr =
  withADBStatus (\status -> do
                    datum       <- peek datumPtr
                    res         <- if isPowered status == isJust (datum_power datum)
                                   then audiodb_insert_datum adb datumPtr
                                   else return (1 :: CInt)
                    return (res == (0 :: CInt)))
  adb

insertMaybeFeatures :: (Ptr ADB) -> (Maybe ADBDatumPtr) -> IO Bool
insertMaybeFeatures adb datumPtr = do
  maybe (return False)
    (\p -> do { insertFeatures adb p })
    datumPtr
