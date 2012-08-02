{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields #-}
module Database( Database(..)
               , nullDb
               , checkDb
               , encodeCompressedFile
               , decodeCompressedFile
               , usedShiftsCount
               , usedShiftNames
               ) where

import Data.Binary
import Data.Typeable

import Data.DeriveTH

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.RepaBinary

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8      as BS
import Codec.Compression.GZip(compress, decompress)

import Data.STAR.Coords     as Coord
import Data.STAR.ChemShifts as CS

-- | Database works as an array of residues in the sequence order, splitted by '*' structure separators,
--   along with coordinates, and chemical shifts
data Database = Database { resArray     :: Array U DIM1 Char  -- (nRes + nStruct)
                         , csArray      :: Array U DIM2 Float -- (nRes + nStruct) x nShifts
                         , csSigmaArray :: Array U DIM2 Float -- (nRes + nStruct) x nShifts
                         , shiftNames   :: Array U DIM2 Char  -- nShifts x 2 (max length of chemical shift code)
                         , crdArray     :: [[Coord]] -- (nRes + nStruct) -> pointers to all coordinates
                                                            -- in each residue (first model only.)
                         }
  deriving (Typeable, Show, Eq)

$(derive makeBinary ''Database)

-- | Empty array of rank 1
emptyArrayDim1 = fromListUnboxed (ix1 0  ) []

-- | Empty array of rank 2
emptyArrayDim2 = fromListUnboxed (ix2 0 0) []

usedShiftNames = ["CA", "CB", "CO", "HA", "HB", "N", "H"]
-- NOTE: "H" are also sometimes called "HN"
usedShiftsCount = length usedShiftNames

{-
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
makeShiftsSigmas cs = (shifts, sigmas)
  where
    ssRows = L.map shiftsSigmasRow cs
    shifts = make . L.map fst $ ssRows
    sigmas = make . L.map snd $ ssRows
    !count = L.length cs
    make   = fromUnboxed (ix2 count usedShiftsCount) . V.concat
-}

-- | Empty database.
nullDb :: Database
nullDb = Database emptyArrayDim1 emptyArrayDim2 emptyArrayDim2 emptyArrayDim2 []

-- | Decode and decompress database file.
decodeCompressedFile f = return . decode . decompress =<< BSL.readFile f

-- | Encode and compress database file.
encodeCompressedFile f = BSL.writeFile f . compress . encode

bshow = BS.pack . show

-- | Checks database, and produces a list of messages about possible problems.
--   TODO: implement!
checkDb :: Database -> [BS.ByteString]
checkDb db = countRes ++ countShifts ++ countCoords ++ countNamesCS ++ shiftNameSize ++ countNamesSigmas ++ countSigmas
  where
    outerLen a  = head .        listOfShape . extent $ a
    innerLen a  = head . tail . listOfShape . extent $ a
    lenRes      = outerLen . resArray     $ db
    lenCS       = outerLen . csArray      $ db
    lenSigmas   = outerLen . csSigmaArray $ db
    widthCS     = innerLen . csArray      $ db
    widthSigmas = innerLen . csSigmaArray $ db
    widthNames  = innerLen . shiftNames   $ db
    lenCrd      = length   . crdArray     $ db
    lenNames    = outerLen . shiftNames   $ db
    countRes         = (lenRes     >=10)           `check` ["Found only ", bshow lenRes, " residues."] -- minimum expected number of residues
    countShifts      = (lenRes     == lenCS)       `check` ["Different number of residues ", bshow lenRes, " than shifts ", bshow lenCS, "."]
    countSigmas      = (lenSigmas  == lenCS)       `check` ["Different number of shift records ", bshow lenCS,
                                                            " than sigma records ", bshow lenSigmas, "."]
    countNamesCS     = (lenNames   == widthCS)     `check` ["Different number of chemical shift codes ", bshow widthCS,
                                                            " than width of chemical shift array ", bshow lenCS, "."]
    countNamesSigmas = (lenNames   == widthSigmas) `check` ["Different number of chemical shift codes ", bshow widthCS,
                                                            " than width of chemical shift array ", bshow lenCS, "."]
    shiftNameSize    = (widthNames == 2)           `check` ["Shift names size is ", bshow widthNames, " not 2."]
    countCoords      = (lenRes     == lenCrd)      `check` ["Different number of residues ", bshow lenRes,
                                                            " than coordinate table lists ", bshow lenCrd, "."]

-- | Helper method for making a lot of tests that may generate singleton lists of error messages.
check :: Bool -> [BS.ByteString] -> [BS.ByteString]
infix 9 `check`
check test msg = if test
                   then []
                   else [BS.concat msg]
