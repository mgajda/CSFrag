{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, NoMonomorphismRestriction, OverloadedStrings #-}
module Database( Database(..)
               , nullDb
               , checkDb
               , encodeCompressedFile
               , decodeCompressedFile
               ) where

import Data.Binary
import Data.Typeable

import Data.DeriveTH

import Data.STAR.Coords
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Repr.Vector
import Data.Array.Repa.RepaBinary

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8      as BS
import Codec.Compression.GZip(compress, decompress)

-- | Database works as an array of residues in the sequence order, splitted by '*' structure separators,
--   along with coordinates, and chemical shifts
data Database = Database { resArray   :: Array U DIM1 Char  -- (nRes + nStruct)
                         , csArray    :: Array U DIM2 Float -- (nRes + nStruct) x nShifts
                         , shiftNames :: Array U DIM2 Char  -- nShifts x 2 (max length of chemical shift code)
                         , crdArray   :: [[Coord]] -- (nRes + nStruct) -> pointers to all coordinates
                                                            -- in each residue (first model only.)
                         }
  deriving (Typeable, Show, Eq)

$(derive makeBinary ''Database)

-- | Empty array of rank 1
emptyArrayDim1 = fromListUnboxed (ix1 0  ) []

-- | Empty array of rank 2
emptyArrayDim2 = fromListUnboxed (ix2 0 0) []

-- | Empty database.
nullDb :: Database
nullDb = Database emptyArrayDim1 emptyArrayDim2 emptyArrayDim2 []

-- | Decode and decompress database file.
decodeCompressedFile f = return . decode . decompress =<< BSL.readFile f

-- | Encode and compress database file.
encodeCompressedFile f = BSL.writeFile f . compress . encode

bshow = BS.pack . show

-- | Checks database, and produces a list of messages about possible problems.
--   TODO: implement!
checkDb :: Database -> [BS.ByteString]
checkDb db = countRes ++ countShifts ++ countCoords ++ countNames ++ shiftNameSize
  where
    outerLen a = head .        listOfShape . extent $ a
    innerLen a = head . tail . listOfShape . extent $ a
    lenRes     = outerLen . resArray    $ db
    lenCS      = outerLen . csArray     $ db
    widthCS    = innerLen . csArray     $ db
    widthNames = innerLen . shiftNames  $ db
    lenCrd     = length   . crdArray    $ db
    lenNames   = outerLen . shiftNames  $ db
    countRes      = (lenRes >=10) `check` ["Found only ", bshow lenRes, " residues."] -- minimum expected number of residues
    countShifts   = (lenRes == lenCS) `check` ["Different number of residues ", bshow lenRes, " than shifts ", bshow lenCS, "."]
    countNames    = (lenNames == widthCS) `check` ["Different number of chemical shift codes ", bshow widthCS, " than width of chemical shift array ", bshow lenCS, "."]
    shiftNameSize = (widthNames == 2) `check` ["Shift names size is ", bshow widthNames, " not 2."]
    countCoords   = (lenRes == lenCrd) `check` ["Different number of residues ", bshow lenRes, " than coordinate table lists ", bshow lenCrd, "."]

-- | Helper method for making a lot of tests that may generate singleton lists of error messages.
check :: Bool -> [BS.ByteString] -> [BS.ByteString]
infix 9 `check`
check test msg = if test
                   then []
                   else [BS.concat msg]
