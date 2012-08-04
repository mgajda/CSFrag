{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import qualified Data.Array.Repa as Repa
import Database
import System.Environment(getArgs)
import Data.Binary
import Data.List as L
import ShiftsCSVInput

main = do [dbfname, shiftsfname] <- getArgs
          (db :: Database) <- decodeFile dbfname
          let csNum = head . tail . Repa.listOfShape . Repa.extent . csArray $ db
          putStrLn $ L.concat ["Read ", show csNum, " chemical shifts."]
          Just input <- processInputFile shiftsfname
          putStrLn . (++ "Header: ") . L.concat . headers $ input
          let inputNum = head . tail . Repa.listOfShape . Repa.extent . shifts $ input
          putStrLn $ L.concat ["Read ", show csNum, " rows of input."]
       

