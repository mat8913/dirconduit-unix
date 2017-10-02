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
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Conduit
import Data.ByteString                    (ByteString)
import qualified Data.ByteString as BS

import Control.Monad.IO.Class             (liftIO)

import System.IO                          (hPutStrLn, hPrint, stderr)
import System.Environment                 (getProgName)

import System.Posix.Env.ByteString        (getArgs)
import System.Posix.ByteString.FilePath   (RawFilePath)
import System.Posix.ByteString.DirConduit


main :: IO ()
main = getArgs >>= program

program :: [ByteString] -> IO ()
program []  = go "."
program [x] = go x
program _   = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName ++ " <path>"

go :: RawFilePath -> IO ()
go = \fp -> runConduitRes $ sourceDirDeep fp
                         .| awaitForever helper
  where
    helper (_ , Left ex)             = liftIO $ hPrint stderr ex
    helper (fp, Right File)          = liftIO $ BS.putStr fp >> putStrLn ""
    helper (fp, Right (Directory _)) = liftIO $ BS.putStr fp >> putStrLn "/"
    helper (fp, Right (LinkTo l))    = liftIO $ do
        BS.putStr fp
        putStr " -> "
        BS.putStr l
        putStrLn ""
