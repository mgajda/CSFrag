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
    shiftXlate (Just i) j name | any (shiftEq name) weightNames = [(name, i, j)]
    shiftXlate _        _ _                                     = []
    errs = L.concat $ L.zipWith shiftErr maybeIndices queryShiftNames
    shiftErr (Just i) name = []
    shiftErr Nothing  name = ["No shifts named " ++ name ++ " in database."]


-- | Comparing chemical shifts in case names are different, but mean something the same.
shiftEq :: String -> String -> Bool
shiftEq "HN" "H" = True
shiftEq "H" "HN" = True
shiftEq a   b    = a == b

-- | Fill in index array from assoclist si, and then use it to transfer indices.
computeScores :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Int) -> Repa.Array Repa.D Repa.DIM2 Float
computeScores si query db seqsim = cutindex (reindex residueScores (-1) Repa.+^
                                             reindex residueScores   0  Repa.+^
                                             reindex residueScores   1)
  where
    shiftComparison (name, i, j) = (name, outer1 (-) (shiftsInd db i) (queryInd query j ))
    comparisons                  = [seqComparison seqsim db query] ++ map shiftComparison si
    weightComparison relIndex (name, arr) = case shiftWeights relIndex name of
                                              Just weight -> Repa.map (*weight) arr
                                              Nothing     -> error $ "Cannot find index: " ++ show name
    residueScores relIndex       = foldr1 (Repa.+^) . map (weightComparison relIndex) $ comparisons
    reindex array relIndex       = array relIndex -- TODO: add zeros on the left and/or right.
    cutindex array               = array          -- TODO: cut leftmost and rightmost column

seqComparison :: (Num b) => (Char -> Char -> Int)-> Database-> ShiftsInput-> (String, Repa.Array Repa.D ((Z :. Int) :. Int) b)
seqComparison seqsim db query = ("seqsim", Repa.map fromIntegral $ outer1 seqsim (resArray db) (resseq query))

shiftsInd :: Database-> Int -> Repa.Array Repa.D (Repa.SliceShape (Z :. Int ) :. Int) Float
shiftsInd db    i = Repa.slice (csArray db   ) (Repa.Z :. i :. Repa.All)

queryInd :: ShiftsInput-> Int -> Repa.Array Repa.D (Repa.SliceShape (Z :. Int) :. Int) Float
queryInd  query i = Repa.slice (shifts  query) (Repa.Z :. i :. Repa.All)

-- | Returns weight of a given chemical shift at a given position within a fragment.
shiftWeights :: (RealFloat b) => Int -> String -> Maybe b
shiftWeights relativeIndex name = assert (relativeIndex <= 1 && relativeIndex >= (-1)) $
                                  L.lookup name (weightsList !! (relativeIndex + 1))

weightsList :: (RealFloat b) => [[(String, b)]]
weightsList = [[("seqsim", 0.5), ("HA", 37), ("CA", 11), ("CB",  9), ("CO", 5), ("N",1  ), ("H", 1  )] -- -1 on seq
              ,[("seqsim", 2.5), ("HA", 31), ("CA", 14), ("CB", 14), ("CO", 6), ("N",1.5), ("H", 0.3)] --  0 on seq
              ,[("seqsim", 1.5), ("HA", 37), ("CA",  7), ("CB",  7), ("CO", 4), ("N",2  ), ("H", 1.5)] -- +1 on seq
              ]

weightNames :: [String]
weightNames = map fst . head $ weightsList

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
          putStrLn $ "Labels: "   ++ show (shiftLabels input)
          putStrLn $ "DB names: " ++ show (shiftNames  db)
          let (si, shiftNameErrs) = shiftIndices (shiftLabels input) (shiftNames db)
          when (shiftNameErrs /= []) . putStrLn . L.intercalate "\n" $ shiftNameErrs
          seqSim <-(maybe (error "Cannot find file with sequence similarity weights!") SeqSim.seqSim
                      `fmap` SeqSim.readWeights)
          putStr "Query indices:"
          print si
          --s <- Repa.computeUnboxedP $ computeScores si input db seqSim
          let s = Repa.computeUnboxedS $ computeScores si input db seqSim
          print . Repa.listOfShape . Repa.extent $ s
          print $ (Repa.toList s :: [Float])


