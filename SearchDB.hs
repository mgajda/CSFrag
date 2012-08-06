{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main(main) where

import System.Environment(getArgs)
import Data.Binary
import Data.List as L
import Control.Monad(when)
--import qualified Data.ByteString.Char8 as BS

import qualified Data.Array.Repa as Repa
import Database
import ShiftsCSVInput

-- ^ Finding fragments in the database.

-- | Matches chemical shift names in query and database.
shiftIndices queryShiftNames dbShiftNames = (xlate, errs)
  where
    maybeIndices  = map (\n -> L.findIndex (shiftEq n) dbShiftNames) queryShiftNames
    xlate = concat $ zipWith shiftXlate maybeIndices [1..]
    shiftXlate (Just i) j = [(i, j)]
    shiftXlate Nothing  _ = []
    errs = L.concat $ L.zipWith shiftErr maybeIndices queryShiftNames
    shiftErr (Just i) name = []
    shiftErr Nothing  name = ["No shifts named " ++ name ++ " in database."]

-- | Comparing chemical shifts in case names are different, but mean something the same.
shiftEq "HN" "H" = True
shiftEq "H" "HN" = True
shiftEq a   b    = a == b

-- | Fill in index array from assoclist si, and then use it to transfer indices.
useShiftIndices si db = undefined
--  where
--    shDb = extent . shifts $ db
--    findI (Z :. x :. y) = lookup x si 

-- | Reads database, and query, then it shows query results (as fragment indices.)
main = do [dbfname, shiftsfname] <- getArgs
          (db :: Database) <- decodeCompressedFile dbfname
          let csNum = head . tail . Repa.listOfShape . Repa.extent . csArray $ db
          putStrLn $ L.concat ["Read ", show csNum, " chemical shifts."]
          Just input <- processInputFile shiftsfname
          putStrLn . ("Header: " ++) . L.intercalate " " . headers $ input
          let inputNum = head . tail . Repa.listOfShape . Repa.extent . shifts $ input
          putStrLn $ L.concat ["Read ", show csNum, " rows of input."]
          let (si, shiftNameErrs) = shiftIndices (shiftLabels input) (shiftNames db)
          when (shiftNameErrs /= []) . putStrLn . L.intercalate "\n" $ shiftNameErrs
          putStr "Query indices:"
          print si 
       

