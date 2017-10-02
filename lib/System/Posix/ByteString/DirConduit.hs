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

{-# LANGUAGE LambdaCase #-}

module System.Posix.ByteString.DirConduit
    ( FileType (..)
    , sourceDirDeep
    , sourceDirStream
    ) where

import Data.Function
import Data.Foldable

import Data.Conduit
import qualified Data.Conduit.List as C

import qualified Data.ByteString as BS

import Control.Monad
import Control.Monad.IO.Class            (MonadIO, liftIO)
import Control.Monad.Trans.Resource      (MonadResource)

import System.Posix.Files.ByteString     (readSymbolicLink)
import System.Posix.Directory.ByteString (DirStream, openDirStream,
                                          closeDirStream, readDirStream)

import Internal.Exception
import Internal.RawFilePath


data FileType s = File
                | Directory s
                | LinkTo RawFilePath
  deriving Show

sourceDirStream :: MonadIO m => DirStream -> ConduitM i RawFilePath m ()
sourceDirStream s = fix $ \r -> do
    x <- liftIO $ readDirStream s
    unless (BS.null x) (yield x >> r)

sourceDirDeep :: MonadResource m
              => RawFilePath
              -> ConduitM i (RawFilePath, Either IOError (FileType ())) m ()
sourceDirDeep fp = bracketP
    (tryIOError $ categorizeFilePath fp)
    (traverse_ uncategorizeFilePath) $
        \case
            Left er             -> yield $ (fp, Left er)
            Right File          -> yield $ (fp, Right File)
            Right (LinkTo l)    -> yield $ (fp, Right $ LinkTo l)
            Right (Directory d) -> do
                yield $ (fp, Right $ Directory ())
                sourceDirStream d .| C.filter (`notElem` special)
                                  .| C.map (fp </>)
                                  .| awaitForever sourceDirDeep

categorizeFilePath :: RawFilePath -> IO (FileType DirStream)
categorizeFilePath fp = trySymbolicLink fp >>= \case
    Just l  -> pure $ LinkTo l
    Nothing -> tryDirectory fp <&> \case
        Just d  -> Directory d
        Nothing -> File

uncategorizeFilePath :: FileType DirStream -> IO ()
uncategorizeFilePath File = pure ()
uncategorizeFilePath (LinkTo _) = pure ()
uncategorizeFilePath (Directory d) = closeDirStream d

trySymbolicLink :: RawFilePath -> IO (Maybe RawFilePath)
trySymbolicLink fp = either (const Nothing) Just <$>
    tryJust (require $ \ex -> ioeGetLocation ex == "readSymbolicLink")
            (readSymbolicLink fp)

tryDirectory :: RawFilePath -> IO (Maybe DirStream)
tryDirectory fp = either (const Nothing) Just <$>
    tryJust (require $ \ex -> ioeGetLocation ex == "openDirStream"
                           && getErrno ex == Just eNOTDIR)
            (openDirStream fp)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
