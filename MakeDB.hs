{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding(String)
import Control.Concurrent.ParallelIO
import System.Environment(getArgs, getProgName)
import System.IO(stderr, hPutStrLn, hPutStr)
import System.Exit
import Control.Monad(when)
import Data.STAR.Type(String(..))

import Util(withParallel, repaFromList1, repaFromLists2, repaConcat2d, repaConcat1d)
import DatabaseCreation(dbFromFile, mergeResults, showDbErrors)
import Database(Database(..), encodeCompressedFile)

-- | Parse .str files and generate arrays in parallel,
--   then merge results into a single database.
--   NOTE: mergeResults should probably be parallel too?
--         Then we should use some kind of parallel queue for reduction?
--   NOTE: this seems like typical "map-reduce" application, except that reduce is mostly trivial.
makeDB :: [FilePath] -> IO Database
makeDB fnames = do dbs <- parallel (Prelude.map dbFromFile fnames)
                   db <- return . mergeResults $ dbs
                   showDbErrors "MERGED" db
                   return db

-- | Print usage on the command line
usage = do prog <- getProgName
           hPutStrLn stderr $ "Usage: " ++ prog ++ " <input1.str> ... <output.db>"

-- | Get arguments, and run makeDB on them, and write
--   resulting database into a single file.
main = do args <- getArgs
          when (length args < 2) $ do usage
                                      exitFailure
          let dbfname     = last    args
          let inputfnames = butlast args
          db <- withParallel $ makeDB inputfnames
          encodeCompressedFile dbfname db
  where
    butlast [b]    = []
    butlast []     = []
    butlast (b:bs) = b:butlast bs

