{-# LANGUAGE FlexibleInstances, BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
module DatabaseCreation(dbFromFile
                       ,mergeResults
                       ,showDbErrors
                       ) where

import Prelude hiding(String)
import System.FilePath
import Control.Monad(when, forM_)
import System.IO(stderr, hPutStrLn)
import Data.Binary
import Data.List(intercalate, foldl', map)
import Data.Map as Map hiding (map)
import Control.Exception(assert)
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

-- TODO: Re-reference chemical shifts?
-- TODO: Check that all information is from solution NMR?

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
                          , CS.sigma     = sigma
                          }) =
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

-- | Key for sorting dictionary
data ResId = ResId { resnum  :: !Int
                   , rescode :: !String -- just informative, nothing else
                   , entity  :: !Int
                   }
  deriving (Eq, Ord, Show, Read)

-- | Finds ChemShift's key for sorting
csKey (ChemShift { seq_id       = num
                 , comp_id      = code
                 , CS.entity_id = ent
                 }) = ResId { entity  = ent
                            , resnum  = num
                            , rescode = code
                            }

-- | Finds Coord's key for sorting
coordKey (Coord { res_id          = num
                , resname         = code
                , Coord.entity_id = ent
                }) = ResId { entity  = ent
                           , resnum  = num
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
addToSMap aFilter finder adder (!ignoreCount, smap) entry =
    if aFilter entry
      then k `seq` (ignoreCount, smap')
      else (ignoreCount+1, smap)
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
                           BS.hPutStr stderr . BS.concat . map (\m -> BS.concat [fname, ":", m, "\n"])
                             $ checkDb db
                           BS.hPutStr stderr (fname `BS.append` ": ")
                           hPutStrLn stderr . Repa.toList . resArray $ db

-- | Reads a single database
dbFromFile fname = do putStrLn fname -- TODO: implement reading
                      parsed <- parseSTARFile fname
                      case parsed of
                        Left errmsg -> do hPutStrLn stderr $ Prelude.concat ["In file ", fname, ": ", errmsg]
                                          return nullDb
                        Right star  -> do let chemShifts = extractChemShifts star
                                          let coords     = extractCoords     star
                                          printMsg [show (length chemShifts)
                                                   ,"chemical shifts from"
                                                   ,fname ++ "."]
                                          printMsg [show (length coords)
                                                   ,"atomic coordinates from"
                                                   ,fname ++ "."]
                                          let (ignCrd, ignCS, smap) = makeSMap chemShifts coords
                                          let keys     = map fst . toAscList . mapKeys sortingKey $ smap
                                          --putStrLn $ "KEYS: " ++ show keys
                                          let testsmap = map snd . toAscList . mapKeys sortingKey $ smap
                                          let ssmap = sortSMap smap
                                          let result = selistToDb ssmap fname
                                          printMsg ["Ignored ", show ignCrd, " coordinates and ", show ignCS, " chemical shift entries."]
                                          showDbErrors (BS.pack fname) result
                                          return result
  where
    printMsg aList = putStrLn $ unwords aList
    makeSMap chemShifts coords = let (ignoredCoords, smapCoords) = Data.List.foldl' addCoordToSMap (0, emptySMap ) coords
                                     (ignoredShifts, smapResult) = Data.List.foldl' addCSToSMap    (0, smapCoords) chemShifts
                                 in (ignoredCoords, ignoredShifts, smapResult)

-- | Gives a sorting key for distinguishing residues.
sortingKey resid = (entity resid, resnum resid)

-- | Converts a map of sorting entries, to an ordered list of per-residue SortingEntries (with no gaps.)
sortSMap = addChainTerminator . fillGaps . map snd . toAscList . mapKeys sortingKey

-- | Converts an ordered list of per-residue @SortingEntry@ records to @Database@
selistToDb selist fname = nullDb { resArray     = repaFromList1 $ fastaSequence selist
                                 , csArray      = shifts
                                 , csSigmaArray = sigmas
                                 , crdArray     = map coords selist
                                 , posNames     = map (residToPos fname . se_key) selist
                                 }
  where
    (shifts, sigmas) = makeShiftsSigmas selist

residToPos fname (ResId { entity  = ent
                        , resnum  = resi
                        , rescode = _
                        }) = DBPos { posFilename = fname
                                   , posEntity   = ent
                                   , posResidue  = resi
                                   }

-- | Fill gaps in an ordered list of SortingEntry records.
--   The goal is to assure that selected fragments will have no breaks.
fillGaps :: [SortingEntry] -> [SortingEntry]
fillGaps []           = []
fillGaps (first:rest) = first:fillGaps' (se_key first) rest
  where
    fillGaps' :: ResId -> [SortingEntry] -> [SortingEntry]
    fillGaps' !_           []          = []
    fillGaps' !(ResId n _ ei) (next:rest) = if ei /= fi
                                              then chainTerminusSE n ei:next:cont
                                              else
                                                if n+1 == k
                                              then                next:cont
                                              else gapSE (n+1) ei:next:cont
      where
        nextKey@(ResId k _ fi) = se_key next
        cont      = fillGaps' nextKey rest

-- | Makes a @SortingEntry@ for a gap in chain with a given residue number.
gapSE n e = SE { se_key     = ResId { resnum  = n
                                    , rescode = "-"
                                    , entity  = e
                                    }
               , chemShifts = []
               , coords     = []
               }

-- | Makes a @SortingEntry@ for a gap in chain with a given residue number.
chainTerminusSE n e = SE { se_key     = ResId { resnum  = n
                                              , rescode = "*"
                                              , entity  = e  
                                              }
                         , chemShifts = []
                         , coords     = []
                         }

-- | This function adds chain terminator.
--   Perhaps it would be better to use @intercalate@ during reduction.
--   TODO: Recognize chain breaks within the same file.
addChainTerminator [] = []
addChainTerminator (s:ss) = s:addChainTerminator' (keyfun s, entfun s) ss
  where
    keyfun = resnum . se_key
    entfun = entity . se_key
    addChainTerminator' (k, e) []     = [chainTerminusSE (k+1) e]
    addChainTerminator' _      (s:ss) = s:addChainTerminator' (keyfun s, entfun s) ss

-- | Takes an ordered, sorted per-residue groups of SortingEntry, and returns FASTA sequence.
--   NOTE: does not yet handle gaps!
fastaSequence = Data.List.map (toSingleLetterCode' . rescode . se_key)

-- | Extends @toSingleLetterCode@ with treatment of gap, and terminator symbols.
toSingleLetterCode' "-"   = '-'
toSingleLetterCode' "*"   = '*'
toSingleLetterCode' "TER" = '*'
toSingleLetterCode' aa    = toSingleLetterCode aa

-- | Merge multiple databases into one.
mergeResults ::  [Database] -> Database
mergeResults dbs = assert allShiftNamesEqual
                   Database { resArray     = repaConcat1d $ map resArray     dbs
                            , csArray      = repaConcat2d $ map csArray      dbs
                            , csSigmaArray = repaConcat2d $ map csSigmaArray dbs
                            , shiftNames   = shiftNames . head             $ dbs -- TODO: add assertion
                            , crdArray     = L.concatMap        crdArray     dbs
                            , posNames     = L.concatMap        posNames     dbs
                            }
  where
    firstShiftNames = shiftNames . head $ dbs
    allShiftNamesEqual = all ((==firstShiftNames) . shiftNames) dbs

printDims fname db = mapM_ (\(name, d) -> BS.hPutStrLn stderr . BS.concat $ [fname, ": ", name, bshow d]) [
                       ("resArray",     f resArray),
                       ("csArray",      f csArray),
                       ("csSigmaArray", f csSigmaArray),
                       ("shiftNames",   [length . shiftNames $ db])
                     --  ,("crdArray",     [length .        crdArray $ db,
                     --                    length . head . crdArray $ db])
                     ]
  where
    f g = Repa.listOfShape . Repa.extent . g $ db
    bshow = BS.pack . show

