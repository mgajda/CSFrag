{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell #-}
module MakeDB where

import Data.STAR.Coords
import System.FilePath
import Data.Array.Repa
import Data.Binary
import Data.Typeable
import Data.DeriveTH
import RepaBin

makeDB :: [FilePath] -> Database
makeDB = undefined

data Database = Database { resArray   :: Array U DIM1 Char   -- (nRes + nStruct)
                         , csArray    :: Array U DIM2 Double -- (nRes + nStruct) x nShifts
                         , crdArray   :: Array U DIM2 Double -- (nRes + nStruct) x 3
                         , shiftNames :: Array U DIM2 Char   -- nShifts x 2 (max length of chemical shift code)
                         }
  deriving (Typeable, Show, Read, Eq)

$(derive makeBinary ''Database)
