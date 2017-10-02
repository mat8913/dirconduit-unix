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

module Internal.Exception
    ( tryJust
    , tryIOError
    , ioeGetLocation
    , eNOTDIR
    , require
    , getErrno
    ) where

import Control.Exception (tryJust)
import System.IO.Error   (tryIOError, ioeGetLocation)
import Foreign.C.Error   (Errno(Errno), eNOTDIR)
import GHC.IO.Exception  (ioe_errno)


require :: (a -> Bool) -> a -> Maybe a
require f x = if f x
    then Just x
    else Nothing

getErrno :: IOError -> Maybe Errno
getErrno = fmap Errno . ioe_errno
