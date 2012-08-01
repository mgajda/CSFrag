{-# LANGUAGE FlexibleInstances, CPP, BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module Main(main, dbFromFile, mergeResults) where

import Prelude hiding(String)
import System.IO(stderr, hPutStrLn)
import System.Exit
import System.FilePath
import System.Environment(getArgs, getProgName)
import Control.Monad(when)
import Data.Binary
import Data.List(intercalate, foldl', map)
import Data.Map as Map hiding (map)
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
import ResidueCodes

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
makeDB fnames = parallel (Prelude.map dbFromFile fnames) >>= mergeResults

-- | Key for sorting dictionary
data ResId = ResId { resnum  :: !Int
                   , rescode :: !String
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

-- | Filters chemical shift records - take all.
csFilter :: ChemShift -> Bool
csFilter    _                           = True

-- | Filters coordinate records - only those from first model.
coordFilter :: Coord -> Bool
coordFilter (Coord { model_id = mid })  = mid == 1

-- | Transient sorting structure is a @Data.Map.Map@ of SortingEntry items.
data SortingEntry = SE { se_key     :: !ResId,
                         chemShifts :: ![ChemShift],
                         coords     :: ![Coord]
                       }
  deriving (Eq, Show)

-- | Empty @SortingEntry@
emptySE k = SE k [] []

-- | Empty transient sorting structure.
emptySMap :: Map.Map ResId SortingEntry
emptySMap = Map.empty

-- | Adds ChemShift to @SortingEntry@
csAdd    se cs    = se { chemShifts = cs   :chemShifts se }

-- | Adds Coord to @SortingEntry@
coordAdd se coord = se { coords     = coord:coords     se }

-- | Given a filter, projection to key, and adding function
--   adds an object to a sorting structure, when filter is true.
addToSMap aFilter finder adder smap entry = if aFilter entry
                                              then k `seq` smap'
                                              else smap
  where
    k     = finder entry
    se    = Map.findWithDefault (emptySE k) k smap
    se'   = adder se entry
    smap' = se' `seq` Map.insert k se' smap

-- | Adds chemical shift record to a sorting map.
addCSToSMap    = addToSMap csFilter    csKey    csAdd

-- | Adds coordinate record to a sorting map, if it passes a filter.
addCoordToSMap = addToSMap coordFilter coordKey coordAdd

-- | Reads a single database
dbFromFile fname = do putStrLn fname -- TODO: implement reading
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
                                         let ssmap = sortSMap smap
                                         print $ head $ toList smap
                                         print $ fastaSequence ssmap 
                                         return nullDb
  where
    printMsg aList = putStrLn $ intercalate " " aList
    makeSMap chemShifts coords = let smapCoords = Data.List.foldl' addCoordToSMap emptySMap  coords
                                 in               Data.List.foldl' addCSToSMap    smapCoords chemShifts

-- | Converts a map of sorting entries, to an ordered list of per-residue SortingEntries (with no gaps.)
sortSMap = fillGaps . map snd . toAscList . mapKeys resnum

-- | Fill gaps in an ordered list of SortingEntry records.
fillGaps :: [SortingEntry] -> [SortingEntry]
fillGaps []           = []
fillGaps (first:rest) = first:fillGaps' (se_key first) rest
  where
    fillGaps' :: ResId -> [SortingEntry] -> [SortingEntry]
    fillGaps' !_           []          = []
    fillGaps' !(ResId n _) (next:rest) = if n+1 == k
                                           then      next:cont
                                           else gap :next:cont
      where
        nextKey@(ResId k _) = se_key next
        cont      = fillGaps' nextKey rest
        gap       = SE { se_key     = ResId { resnum = n+1,
                                              rescode = "-"
                                            },
                         chemShifts = [],
                         coords     = []
                       }

-- | Takes an ordered, sorted per-residue groups of SortingEntry, and returns FASTA sequence.
--   NOTE: does not yet handle gaps!
fastaSequence = Data.List.map (toSingleLetterCode' . rescode . se_key)

toSingleLetterCode' "-" = '-'
toSingleLetterCode' aa  = toSingleLetterCode aa

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

