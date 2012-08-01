{-# LANGUAGE FlexibleInstances, BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
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
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.RepaBinary()

import Data.STAR
import Data.STAR.Coords
import Data.STAR.ChemShifts
import Data.STAR.Type(String(..))

import Database
import ResidueCodes
import Util(withParallel)

-- | Parse .str files and generate arrays in parallel,
--   then merge results into a single database.
--   NOTE: mergeResults should probably be parallel too?
--         Then we should use some kind of parallel queue for reduction?
--   NOTE: this seems like typical "map-reduce" application, except that reduce is mostly trivial.
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
-- NOTE: better to put coords into separate file, indexed from the first. This way we may avoid reading most of the coordinates!
-- There is a difference between saved ChemShift and Coord files: ~9MB vs ~4GB.

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
sortSMap = addChainTerminator . fillGaps . map snd . toAscList . mapKeys resnum

-- | Fill gaps in an ordered list of SortingEntry records.
--   The goal is to assure that selected fragments will have no breaks.
fillGaps :: [SortingEntry] -> [SortingEntry]
fillGaps []           = []
fillGaps (first:rest) = first:fillGaps' (se_key first) rest
  where
    fillGaps' :: ResId -> [SortingEntry] -> [SortingEntry]
    fillGaps' !_           []          = []
    fillGaps' !(ResId n _) (next:rest) = if n+1 == k
                                           then             next:cont
                                           else gapSE (n+1):next:cont
      where
        nextKey@(ResId k _) = se_key next
        cont      = fillGaps' nextKey rest

-- | Makes a @SortingEntry@ for a gap in chain with a given residue number.
gapSE n = SE { se_key     = ResId { resnum  = n
                                  , rescode = "-"
                                  }
             , chemShifts = []
             , coords     = []
             }

-- | Makes a @SortingEntry@ for a gap in chain with a given residue number.
chainTerminusSE n = SE { se_key     = ResId { resnum  = n
                                            , rescode = "*"
                                            }
                       , chemShifts = []
                       , coords     = []
                       }

-- | This function adds chain terminator.
--   Perhaps it would be better to use @intercalate@ during reduction.
--   TODO: Recognize chain breaks within the same file.
addChainTerminator [] = []
addChainTerminator (s:ss) = s:addChainTerminator' (keyfun s) ss
  where
    keyfun = resnum . se_key
    addChainTerminator' k []     = [chainTerminusSE (k+1)]
    addChainTerminator' _ (s:ss) = s:addChainTerminator' (keyfun s) ss

-- | Takes an ordered, sorted per-residue groups of SortingEntry, and returns FASTA sequence.
--   NOTE: does not yet handle gaps!
fastaSequence = Data.List.map (toSingleLetterCode' . rescode . se_key)

toSingleLetterCode' "-"   = '-'
toSingleLetterCode' "*"   = '*'
toSingleLetterCode' "TER" = '*'
toSingleLetterCode' aa    = toSingleLetterCode aa

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

