{-# LANGUAGE FlexibleInstances, CPP #-}
module Main(main) where

import System.IO(stderr, hPutStrLn)
import System.Exit
import System.FilePath
import System.Environment(getArgs, getProgName)
import Control.Monad(when)
import Data.Binary
import Data.List(intercalate)
import Control.Concurrent.ParallelIO
#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif

import Data.STAR
import Data.STAR.Coords
import qualified Data.Array.Repa as Repa
import Database

--   Here is code for parallellism
-- | Sets up as many capabilities as we have processors.
#ifdef __GLASGOW_HASKELL__
setupParallel = GHC.Conc.getNumProcessors >>= GHC.Conc.setNumCapabilities
#else
setupParallel = return ()
#endif

-- | Finalization of parallel pool.
stopParallel = stopGlobalPool

-- | Wraps parallel-io computation with setupParallel and stopParallel.
--   NOTE: Not yet exception-proof.
withParallel act = do setupParallel
                      r <- act
                      stopParallel
                      return r

-- | Parse .str files and generate arrays in parallel,
--   then merge results into a single database.
makeDB :: [FilePath] -> IO Database
makeDB fnames = parallel (Prelude.map processFile fnames) >>= mergeResults

-- | Reads a single database
processFile fname = do putStrLn fname -- TODO: implement reading
                       parsed <- parseSTARFile fname
                       case parsed of
                         Left errmsg ->do hPutStrLn stderr errmsg
                                          return nullDb
                         Right star  ->do let chemShifts = extractChemShifts star
                                          let coords     = extractCoords     star
                                          --chemShifts `par` coords `par` ...
                                          printMsg [show (length chemShifts)
                                                   ,"chemical shifts from"
                                                   ,fname ++ "."]
                                          printMsg [show (length chemShifts)
                                                   ,"atomic coordinates from"
                                                   ,fname ++ "."]
                                          return nullDb
  where
    printMsg aList = putStrLn $ intercalate " " aList

-- | Merge multiple databases into one.
mergeResults (r:rs) = return r -- TODO: proper merging!
mergeResuls  _      = return nullDb

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
          encodeFile dbfname db
  where
    butlast [b]    = []
    butlast []     = []
    butlast (b:bs) = b:butlast bs

