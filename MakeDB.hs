{-# LANGUAGE FlexibleInstances, BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module Main(main
           ,dbFromFile
           ,mergeResults
           ) where

import Prelude hiding(String)
import System.IO(stderr, hPutStrLn, hPutStr)
import System.Exit
import System.FilePath
import System.Environment(getArgs, getProgName)
import Control.Monad(when, forM_)
import Data.Binary
import Data.List(intercalate, foldl', map)
import Data.Map as Map hiding (map)
import Control.Concurrent.ParallelIO
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Index(ix2)
import Data.Array.Repa.RepaBinary()
import qualified Data.ByteString.Char8 as BS

import Data.STAR
import Data.STAR.Coords     as Coord
import Data.STAR.ChemShifts as CS
import Data.STAR.Type(String(..))

import Database
import ResidueCodes
import Util(withParallel, repaFromList1, repaFromLists2, repaConcat2d, repaConcat1d)

import qualified Data.List as L
import qualified Data.Vector.Unboxed  as V

-- | Converts a list of @ChemicalShift@s within residue, to a row of CS array.
shiftsSigmasRow cs = (mkRow shifts, mkRow sigmas)
  where
    mkRow al = emptyRow V.// al -- TODO: make a row out of assoclist
    emptyRow = V.replicate usedShiftsCount 0.0
    entries  = L.concatMap shiftEntry cs
    shifts   = L.map fst entries
    sigmas   = L.map snd entries
    shiftEntry (ChemShift { CS.atom_id   = atid
                          , CS.chemshift = value
                          , CS.sigma     = sigma }) =
      case atid `L.elemIndex` usedShiftNames of
        Nothing -> []
        Just n  -> [((n, value), (n, sigma))]

-- | Make shifts and sigmas arrays.
makeShiftsSigmas ses = (shifts, sigmas)
  where
    cs     = L.map chemShifts ses
    ssRows = L.map shiftsSigmasRow cs
    shifts = make . L.map fst $ ssRows
    sigmas = make . L.map snd $ ssRows
    !count = L.length cs
    make   = Repa.fromUnboxed (ix2 count usedShiftsCount) . V.concat

-- | Parse .str files and generate arrays in parallel,
--   then merge results into a single database.
--   NOTE: mergeResults should probably be parallel too?
--         Then we should use some kind of parallel queue for reduction?
--   NOTE: this seems like typical "map-reduce" application, except that reduce is mostly trivial.
makeDB :: [FilePath] -> IO Database
makeDB fnames = do dbs <- parallel (Prelude.map dbFromFile fnames)
                   db <- return $ mergeResults dbs
                   showDbErrors "MERGED" db
                   return db

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

showDbErrors :: String -> Database -> IO ()
showDbErrors fname db = do printDims fname db
                           BS.hPutStr stderr . BS.concat . map (\m -> BS.concat [fname, ":", m, "\n"]) $ checkDb db

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
                                         let smap = makeSMap chemShifts coords
                                         let ssmap = sortSMap smap
                                         print $ head $ toList smap
                                         print "AAA"
                                         let result = selistToDb ssmap
                                         print "BBB"
                                         showDbErrors (BS.pack fname) result
                                         print "CCC"
                                         print result
                                         return result
  where
    printMsg aList = putStrLn $ intercalate " " aList
    makeSMap chemShifts coords = let smapCoords = Data.List.foldl' addCoordToSMap emptySMap  coords
                                 in               Data.List.foldl' addCSToSMap    smapCoords chemShifts

-- | Converts a map of sorting entries, to an ordered list of per-residue SortingEntries (with no gaps.)
sortSMap = addChainTerminator . fillGaps . map snd . toAscList . mapKeys resnum

-- | Converts an ordered list of per-residue @SortingEntry@ records to @Database@
selistToDb selist = nullDb { resArray     = repaFromList1 $ fastaSequence selist
                           , csArray      = shifts
                           , csSigmaArray = sigmas
                           }
  where
    (shifts, sigmas) = makeShiftsSigmas selist


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

-- | Extends @toSingleLetterCode@ with treatment of gap, and terminator symbols.
toSingleLetterCode' "-"   = '-'
toSingleLetterCode' "*"   = '*'
toSingleLetterCode' "TER" = '*'
toSingleLetterCode' aa    = toSingleLetterCode aa

-- | Merge multiple databases into one.
mergeResults dbs = Database { resArray     = repaConcat1d $ map resArray     dbs
                            , csArray      = repaConcat2d $ map csArray      dbs
                            , csSigmaArray = repaConcat2d $ map csSigmaArray dbs
                            , shiftNames   = shiftNames . head             $ dbs -- TODO: add assertion
                            , crdArray     = L.concatMap        crdArray     dbs
                            }

printDims fname db = flip forM_ (\(name, d) -> BS.hPutStrLn stderr . BS.concat $ [fname, ": ", name, bshow d]) [
                       ("resArray",     f resArray),
                       ("csArray",      f csArray),
                       ("csSigmaArray", f csSigmaArray),
                       ("shiftNames",   f shiftNames)
                     --  ,("crdArray",     [length .        crdArray $ db,
                     --                    length . head . crdArray $ db])
                     ]
  where
    f g = Repa.listOfShape . Repa.extent . g $ db
    bshow = BS.pack . show

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

