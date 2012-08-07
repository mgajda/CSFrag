{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TypeOperators #-}
module Main where

import System.Environment(getArgs, getProgName)
import System.Exit
import Data.Binary
import Data.List as L
import Control.Monad(when)
import Control.Exception(assert)

import qualified Data.Array.Repa as Repa
import Data.Array.Repa((:.)(..), Z)
import Data.Array.Repa.Eval()

import Database
import ShiftsCSVInput
import qualified SeqSim
import Outer

-- ^ Finding fragments in the database.

-- | Matches chemical shift names in query and database.
shiftIndices queryShiftNames dbShiftNames = (xlate, errs)
  where
    maybeIndices  = map (\n -> L.findIndex (shiftEq n) dbShiftNames) queryShiftNames
    xlate = concat $ zipWith3 shiftXlate maybeIndices [1..] dbShiftNames
    shiftXlate (Just i) j name = [(name, i, j)]
    shiftXlate Nothing  _ name = []
    errs = L.concat $ L.zipWith shiftErr maybeIndices queryShiftNames
    shiftErr (Just i) name = []
    shiftErr Nothing  name = ["No shifts named " ++ name ++ " in database."]


-- | Comparing chemical shifts in case names are different, but mean something the same.
shiftEq "HN" "H" = True
shiftEq "H" "HN" = True
shiftEq a   b    = a == b

-- | Fill in index array from assoclist si, and then use it to transfer indices.
computeScores :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Int) -> Repa.Array Repa.D Repa.DIM1 Float
computeScores si query db seqsim = undefined
  where
    shiftsInd i = Repa.slice (csArray db   ) (Repa.Z :. i :. Repa.All)
    queryInd  i = Repa.slice (shifts  query) (Repa.Z :. i :. Repa.All)

-- | Returns weight of a given chemical shift at a given position within a fragment.
shiftWeights :: (RealFloat b) => Int -> String -> Maybe b
shiftWeights relativeIndex name = assert (relativeIndex <= 1 && relativeIndex >= (-1)) $
                                  L.lookup name (weights !! (relativeIndex + 1))
  where
    weights = [[("seqsim", 0.5), ("HA", 37), ("CA", 11), ("CB",  9), ("CO", 5), ("N",1  ), ("H", 1  )] -- -1 on seq
              ,[("seqsim", 2.5), ("HA", 31), ("CA", 14), ("CB", 14), ("CO", 6), ("N",1.5), ("H", 0.3)] --  0 on seq
              ,[("seqsim", 1.5), ("HA", 37), ("CA",  7), ("CB",  7), ("CO", 4), ("N",2  ), ("H", 1.5)] -- +1 on seq
              ]

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
          s <- Repa.computeUnboxedP $ computeScores si input db seqSim
          print $ (Repa.toList s :: [Float])


