{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TypeOperators, NoMonomorphismRestriction, FlexibleContexts #-}
module Main where

import System.Environment(getArgs, getProgName)
import System.Exit
import System.IO(hFlush, stdout)
import Data.Binary
import Data.List as L
import Control.Monad(when)
import Control.Exception(assert)
import Debug.Trace(trace)

import qualified Data.Array.Repa as Repa
import Data.Array.Repa((:.)(..), Z(..))
import Data.Array.Repa.Eval()

--import Data.Array.Repa.IO.Matrix(writeMatrixToTextFile)

import Database
import DatabaseSearch
import ShiftsCSVInput
import qualified SeqSim
import Outer
import Util

-- ^ Finding fragments in the database.

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
          db <- readDB dbfname
          let csNum   = head . tail . Repa.listOfShape . Repa.extent . csArray $ db
          let csShape =               Repa.listOfShape . Repa.extent . csArray $ db
          putStrLn $ L.concat ["Read ", show csNum, " chemical shifts."]
          putStrLn $ "Read shifts array of shape " ++ show csShape
          Just input <- processInputFile shiftsfname
          putStrLn . ("Header: " ++) . L.unwords . headers $ input
          let inputNum   = head . tail . Repa.listOfShape . Repa.extent . shifts $ input
          let inputShape =               Repa.listOfShape . Repa.extent . shifts $ input
          putStrLn $ L.concat ["Read ", show inputNum, " rows of input."]
          putStrLn $ "Read input of shape " ++ show inputShape
          putStrLn $ "Labels: "   ++ show (shiftLabels input)
          putStrLn $ "DB names: " ++ show (shiftNames  db)
          let (si, shiftNameErrs) = shiftIndices (shiftLabels input) (shiftNames db)
          when (shiftNameErrs /= []) . putStrLn . L.intercalate "\n" $ shiftNameErrs
          seqSim <-maybe (error "Cannot find file with sequence similarity weights!") SeqSim.seqSim
                      `fmap` SeqSim.readWeights
          putStr "Query indices:"
          print si
          hFlush stdout 
          let compsco = computeScores si input db seqSim
          --print . ("Final shape:" ++) . show . Repa.listOfShape . Repa.extent $ compsco
          hFlush stdout 
          --print compsco
          s <- Repa.computeUnboxedP compsco
          --logS <- Repa.computeUnboxedP $ Repa.map (logBase 2) compsco
          --print $ L.sort (Repa.toList s :: [Float])
          --print $ (Repa.toList s :: [Float])
          -- TODO: 1. Print as 2 dim array (so that boundaries are clear)
          -- TODO: 2. Show as 2D heat map in GNUPlot?
          -- TODO: 3. Show indices of best scores.
          writeFile "matrix.out" $ showsMatrix' (repaToLists2 compsco) ""
          --writeFile "matrix.out" $ showsMatrix s ""


