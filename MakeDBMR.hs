{-# LANGUAGE FlexibleInstances, BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, ImpredicativeTypes #-}
module Main where

import Prelude hiding(String)
import System.IO(stderr, hPutStrLn, hPutStr)
import System.Environment(getArgs, getProgName)
import Control.Monad(when, forM_)

import Data.STAR.Type(String(..))

import Control.Monad.IO.Class(liftIO)

import Remote -- CloudHaskell
import Remote.Task(liftTaskIO)

import Database
import DatabaseCreation
import Util(withParallel, repaFromList1, repaFromLists2, repaConcat2d, repaConcat1d)


-- | Print usage on the command line
usage = do prog <- getProgName
           hPutStrLn stderr $ "Usage: " ++ prog ++ " <input1.str> ... <output.db>"

butlast :: [a] -> [a]
butlast [b]    = []
butlast []     = []
butlast (b:bs) = b:butlast bs

-- Here comes CloudHaskell stuff...
dbFromFileTask :: [[Char]] ->TaskM [Database]
dbFromFileTask fnames  = liftTaskIO $ mapM dbFromFile fnames

mergeResultsTask :: [Database] ->TaskM [Database]
mergeResultsTask inputs = return [mergeResults inputs]

$( remotable [ 'dbFromFileTask, 'mergeResultsTask ] )

dbMapReduce = MapReduce 
               {
                 mtMapper = dbFromFileTask__closure,
                 mtReducer = mergeResultsTask__closure,
                 mtChunkify = chunkify 1,
                 mtShuffle = \d -> [d]
               }

-- | Get arguments, and run makeDB on them, and write
--   resulting database into a single file.

initialProcess "MASTER" = 
                   do args <- getCfgArgs
                      let inputfiles = butlast args
                      let outputfile = last    args
                      db <-runTask $ mapReduce dbMapReduce inputfiles
                      liftIO $ encodeCompressedFile outputfile db

initialProcess "WORKER" = receiveWait [] 
initialProcess _ = say "You need to start this program as either a MASTER or a WORKER. Set the appropiate value of cfgRole on the command line or in the config file."

main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess

