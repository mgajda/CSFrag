{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, NoMonomorphismRestriction #-}
module Database(Database(..), nullDb
               ,encodeCompressedFile
               ,decodeCompressedFile
               ) where

import Data.Binary
import Data.Typeable

import Data.DeriveTH

import Data.STAR.Coords
import Data.Array.Repa
import Data.Array.Repa.RepaBinary

import Data.ByteString.Lazy as BS
import Codec.Compression.GZip(compress, decompress)

-- | Database works as an array of residues in the sequence order, splitted by '*' structure separators,
--   along with coordinates, and chemical shifts
data Database = Database { resArray   :: Array U DIM1 Char  -- (nRes + nStruct)
                         , csArray    :: Array U DIM2 Float -- (nRes + nStruct) x nShifts
                         , shiftNames :: Array U DIM2 Char  -- nShifts x 2 (max length of chemical shift code)
                         , crdArray   :: [[Coord]]          -- (nRes + nStruct) -> pointers to all coordinates
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
decodeCompressedFile f = return . decode . decompress =<< BS.readFile f

-- | Encode and compress database file.
encodeCompressedFile f = BS.writeFile f . compress . encode
