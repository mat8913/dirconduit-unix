-- Copyright (C) 2017  Matthew Harm Bekkema
--
-- This file is part of dirconduit-unix.
--
-- dirconduit-unix is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- dirconduit-unix is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Internal.RawFilePath
    ( RawFilePath
    , special
    , (</>)
    ) where

import Data.Maybe
import Data.Monoid
import Data.Word (Word8)

import qualified Data.ByteString as BS
import System.Posix.ByteString.FilePath


special :: [RawFilePath]
special = [ "."
          , ".."
          ]

pathSep :: Word8
pathSep = fromIntegral $ fromEnum '/'

(</>) :: RawFilePath -> RawFilePath -> RawFilePath
px </> py = fromMaybe (px <> py) $ do
    (_, x) <- BS.unsnoc px
    (y, _) <- BS.uncons py
    if | x == pathSep -> pure $ px <> py
       | y == pathSep -> pure $ py
       | otherwise    -> pure $ px <> BS.singleton pathSep <> py
