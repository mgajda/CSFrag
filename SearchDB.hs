{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TypeOperators #-}
module Main(main) where

import System.Environment(getArgs, getProgName)
import System.Exit
import Data.Binary
import Data.List as L
import Control.Monad(when)
--import qualified Data.ByteString.Char8 as BS

import qualified Data.Array.Repa as Repa
import Data.Array.Repa((:.)(..), Z)
import Database
import ShiftsCSVInput
import qualified SeqSim
import Outer

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
computeScores si query db weights = undefined
  where
    shiftsInd i = Repa.slice (csArray db   ) (Repa.Z :. i :. Repa.All)
    queryInd  i = Repa.slice (shifts  query) (Repa.Z :. i :. Repa.All)

shiftWeights = [] 

-- | Prints command-line help for the program.
printUsage = do prog <- getProgName
                putStrLn $ " " `L.intercalate` usage prog
 where
   usage prog = [ "Usage: "
                , prog
                , "<database from MakeDB>"
                , "<query.csv>"
                , "\nProgram for searching with Preditor algorithm."
                ]

-- | Reads database, and query, then it shows query results (as fragment indices.)
main = do args <- getArgs
          when (L.length args /=2) $ do printUsage
                                        exitFailure
          let [dbfname, shiftsfname] = args
          (db :: Database) <- decodeCompressedFile dbfname
          let csNum = head . tail . Repa.listOfShape . Repa.extent . csArray $ db
          putStrLn $ L.concat ["Read ", show csNum, " chemical shifts."]
          Just input <- processInputFile shiftsfname
          putStrLn . ("Header: " ++) . L.intercalate " " . headers $ input
          let inputNum = head . tail . Repa.listOfShape . Repa.extent . shifts $ input
          putStrLn $ L.concat ["Read ", show csNum, " rows of input."]
          let (si, shiftNameErrs) = shiftIndices (shiftLabels input) (shiftNames db)
          when (shiftNameErrs /= []) . putStrLn . L.intercalate "\n" $ shiftNameErrs
          seqSim <-(maybe (error "Cannot find file with sequence similarity weights!") SeqSim.seqSim
                      `fmap` SeqSim.readWeights)
          putStr "Query indices:"
          print si 
       

