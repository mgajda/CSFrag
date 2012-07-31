{-# LANGUAGE FlexibleInstances, CPP #-}
module Main(main) where

import Prelude hiding(String)
import System.IO(stderr, hPutStrLn)
import System.Exit
import System.FilePath
import System.Environment(getArgs, getProgName)
import Control.Monad(when)
import Data.Binary
import Data.List(intercalate, foldl')
import Data.Map as Map
import Control.Concurrent.ParallelIO
#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.RepaBinary()

import Data.STAR
import Data.STAR.Coords
import Data.STAR.ChemShifts
import Data.STAR.Type(String(..))
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

-- | Key for sorting dictionary
data ResId = ResId { resnum  :: Int
                   , rescode :: String
                   }
  deriving (Eq, Ord, Show, Read)

-- | Finds ChemShift's key for sorting
csKey (ChemShift { seq_id  = num
                 , comp_id = code
                 }) = ResId { resnum  = num
                            , rescode = code
                            }

-- | Finds Coord's key for sorting
coordKey (Coord { res_id  = num
                , resname = code
                }) = ResId { resnum  = num
                           , rescode = code
                           }

-- | Transient sorting structure is a @Data.Map.Map@ of SortingEntry items.
data SortingEntry = SE { key        :: ResId,
                         chemShifts :: [ChemShift],
                         coords     :: [Coord]
                       }
  deriving (Eq, Show)

-- | Empty @SortingEntry@
emptySE k = SE k [] []

emptySMap :: Map.Map ResId SortingEntry
emptySMap = Map.empty

-- | Adds ChemShift to @SortingEntry@
csAdd    se cs    = se { chemShifts = cs   :chemShifts se }

-- | Adds Coord to @SortingEntry@
coordAdd se coord = se { coords     = coord:coords     se }

-- | Given projection to key, and adding function adds an object to a sorting structure.
addToSMap finder adder smap entry = smap'
  where
    k     = finder entry
    se    = Map.findWithDefault (emptySE k) k smap
    se'   = adder se entry
    smap' = Map.insert k se' smap

addCSToSMap    = addToSMap csKey    csAdd

addCoordToSMap = addToSMap coordKey coordAdd

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
                                          printMsg [show (length coords)
                                                   ,"atomic coordinates from"
                                                   ,fname ++ "."]
                                          print $ head chemShifts
                                          print $ head coords
                                          let smap = makeSMap chemShifts coords
                                          print $ head $ toList smap
                                          return nullDb
  where
    printMsg aList = putStrLn $ intercalate " " aList
    makeSMap chemShifts coords = let smapCoords = Data.List.foldl' addCoordToSMap emptySMap  coords
                                 in               Data.List.foldl' addCSToSMap    smapCoords chemShifts

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

