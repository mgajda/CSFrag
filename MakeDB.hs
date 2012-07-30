{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, CPP, NoMonomorphismRestriction #-}
module Main(main) where

import System.FilePath
import System.Environment(getArgs)
import Data.Binary
import Data.Typeable
import Control.Concurrent.ParallelIO
#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif

import Data.DeriveTH

import Data.STAR.Coords
import Data.Array.Repa
import RepaBin

-- | Database works as an array of residues in the sequence order, splitted by '*' structure separators,
--   along with coordinates, and chemical shifts
data Database = Database { resArray   :: Array U DIM1 Char   -- (nRes + nStruct)
                         , csArray    :: Array U DIM2 Double -- (nRes + nStruct) x nShifts
                         , shiftNames :: Array U DIM2 Char   -- nShifts x 2 (max length of chemical shift code)
                         , crdArray   :: [[Coord]]           -- (nRes + nStruct) -> pointers to all coordinates
                                                             -- in each residue (first model only.)
                         }
  deriving (Typeable, Show, Eq)

$(derive makeBinary ''Database)


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
makeDB fnames = (parallel $ Prelude.map processFile fnames) >>= mergeResults

-- | Reads a single database
processFile fname = do putStrLn fname -- TODO: implement reading
                       return nullDb

-- | Empty array of rank 1
emptyArrayDim1 = fromListUnboxed (ix1 0  ) []

-- | Empty array of rank 2
emptyArrayDim2 = fromListUnboxed (ix2 0 0) []

-- | Empty database.
nullDb :: Database
nullDb = Database emptyArrayDim1 emptyArrayDim2 emptyArrayDim2 []

-- | Merge multiple databases into one.
mergeResults (r:rs) = return r -- TODO: proper merging!
mergeResuls  _      = return nullDb

-- | Get arguments, and run makeDB on them, and write
--   resulting database into a single file.
main = do args <- getArgs
          let dbfname     = last    args
          let inputfnames = butlast args
          db <- withParallel $ makeDB inputfnames
          encodeFile dbfname db
  where
    butlast [b]    = []
    butlast []     = []
    butlast (b:bs) = b:butlast bs

