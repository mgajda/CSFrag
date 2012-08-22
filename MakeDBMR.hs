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
import Remote.Task(liftTaskIO, newPromise, readPromise)

import Database
import DatabaseCreation

import Control.DeepSeq
import Data.DeriveTH
import qualified Data.Array.Repa as Repa

-- | Print usage on the command line
usage = do prog <- getProgName
           hPutStrLn stderr $ "Usage: " ++ prog ++ " <input1.str> ... <output.db>"

butlast :: [a] -> [a]
butlast [b]    = []
butlast []     = []
butlast (b:bs) = b:butlast bs

-- Here comes CloudHaskell stuff...
dbFromFileTask :: [Char] ->TaskM Database
dbFromFileTask fname = liftTaskIO $ dbFromFile fname

mergeResultsTask :: [Database] ->TaskM Database
mergeResultsTask inputs = let r = mergeResults inputs
                          in r `deepseq` return r

$( remotable [ 'dbFromFileTask, 'mergeResults ] )

-- Here is a hand-made modification of mapReduce to do map in parallel, but fold on MASTER
mapFold mapper reducer inputs = -- TODO: check if chunkify packages anything?
        do pmapResult <- mapM (newPromise . mapper) inputs
           mapResult  <- mapM readPromise pmapResult
           return $ reducer mapResult

-- | Get arguments, and run makeDB on them, and write
--   resulting database into a single file.

initialProcess "MASTER" = 
                   do args <- getCfgArgs
                      let inputfiles = butlast args
                      let outputfile = last    args
                      db <-runTask $! mapFold dbFromFileTask__closure mergeResults inputfiles
                      liftIO $ writeDB outputfile db

initialProcess "WORKER" = receiveWait [] 
initialProcess _        = say ("You need to start this program as either a MASTER or a WORKER." ++
                               "Set the appropiate value of cfgRole on the command line or in " ++
                               "the config file.")

main = remoteInit (Just "config") [Main.__remoteCallMetaData] initialProcess

