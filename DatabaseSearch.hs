{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TypeOperators, NoMonomorphismRestriction, FlexibleContexts #-}
module DatabaseSearch where

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
import ShiftsCSVInput
import qualified SeqSim
import Outer
import Util

-- ^ Finding fragments in the database.

-- | Matches chemical shift names in query and database.
shiftIndices queryShiftNames dbShiftNames = (xlate, errs)
  where
    maybeIndices  = map (\n -> L.findIndex (shiftEq n) dbShiftNames) queryShiftNames
    xlate = concat $ zipWith3 shiftXlate maybeIndices [0..] dbShiftNames
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

traceShape :: (Repa.Shape b, Repa.Source r e) => String -> Repa.Array r b e -> Repa.Array r b e
traceShape name ary = trace (showShape name ary) ary

showShape  :: (Repa.Shape b, Repa.Source r e) => String -> Repa.Array r b e -> [Char]
showShape name = (\s -> name ++ ": " ++ s) . show . Repa.listOfShape . Repa.extent

traceShapeOfSnd :: (Repa.Shape b, Repa.Source r e) => String -> (t, Repa.Array r b e) -> (t, Repa.Array r b e)
traceShapeOfSnd name (a, b) = trace (showShape name b) (a, b)

-- | Compute a score without using Repa compound operations
{-
computeScores :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Double) -> [[Double]]
computeScores si query db seqsim = [[answer (Z:. dbi :. qi)
                                     |  qi  <- [0..len1d (resseq   query) - 1]]
                                      | dbi <- [0..len1d (resArray db   ) - 1]]
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((resArray db    Repa.! (Repa.ix1 dbi)) `seqsim`
                        (resseq   query Repa.! (Repa.ix1 qi ))         )
    len1d arr = let Z :. i = Repa.extent arr
                in i

computeScores'' :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Double) -> Repa.Array Repa.D Repa.DIM2 Double
computeScores'' si query db seqsim = Repa.fromFunction (Z :. len1d (resArray db) :. len1d (resseq query))
                                   answer
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((resArray db    Repa.! (Repa.ix1 dbi)) `seqsim`
                        (resseq   query Repa.! (Repa.ix1 qi ))         )
    len1d arr = let Z :. i = Repa.extent arr
                in i
-}
-- | Fill in index array from assoclist si, and then use it to transfer indices.
computeScores :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Double) -> Repa.Array Repa.D Repa.DIM2 Double
computeScores si query db seqsim = residueScores 0
                                   {-shiftIndex (-1) 0.0 (residueScores (-1)) Repa.+^
                                   residueScores                        0   Repa.+^
                                   shiftIndex   1  0.0 (residueScores   1 )-}
  where
    shiftComparison (name, i, j) = traceShapeOfSnd "shiftComparison" (name, outer1 absDiff (shiftsInd db i) (queryInd query j ))
    --comparisons                 = [seqComparison seqsim db query] -- : map shiftComparison si
    --comparisons                 = seqComparison seqsim db query : map shiftComparison si
    comparisons                  = map (snd . shiftComparison) si
    weightComparison relIndex (name, arr) = case shiftWeights relIndex name of
                                              Just weight -> Repa.map (*weight) arr
                                              Nothing     -> error $ "Cannot find index: " ++ show name
    --residueScores relIndex = foldr1 (Repa.+^) . map (weightComparison relIndex) $ comparisons
    residueScores relIndex = foldr1 (testAdd) $ comparisons
    --residueScores relIndex = foldr1 (Repa.+^) $ comparisons
    reindex array relIndex = array relIndex -- TODO: add zeros on the left and/or right.
    cutindex array         = array          -- TODO: cut leftmost and rightmost column
    a `absDiff` b          = abs (a - b)

testAdd = elementWise (+)

elementWise f a b = assert (Repa.extent a == Repa.extent b) $
                      Repa.fromFunction (Repa.extent a)
                                        (\i -> (a Repa.! i) `f` (b Repa.! i))

shiftIndex :: Repa.Source r1 e => Int -> e -> Repa.Array r1 Repa.DIM2 e -> Repa.Array Repa.D Repa.DIM2 e
shiftIndex shift defaultValue arr = Repa.backpermuteDft defaultsArray indexMapping arr
  where
    shape         = Repa.extent arr
    defaultsArray = Repa.fromFunction shape (const defaultValue)
    indexMapping  (Repa.Z :. x :. y) = let sh :: Repa.DIM2 = Repa.Z :. x :. (y + shift)
                                  in if sh `Repa.inShape` shape
                                       then Just sh
                                       else Nothing

seqComparison :: (Char -> Char -> Double)-> Database-> ShiftsInput-> (String, Repa.Array Repa.D Repa.DIM2 Double)
seqComparison seqsim db query = traceShapeOfSnd "seqComparison" ("seqsim", outer1 seqsim (resArray db) (resseq query))

shiftsInd :: Database   -> Int -> Repa.Array Repa.D (Repa.SliceShape Repa.DIM1  :. Int) Double
shiftsInd db    i = undefined --traceShape "shiftsInd" $ Repa.slice (csArray db   ) (Repa.Z :. Repa.All :. i)

queryInd :: ShiftsInput -> Int -> Repa.Array Repa.D (Repa.SliceShape (Z :. Int) :. Int) Double
queryInd  query i = undefined --traceShape "queryInd"  $ Repa.slice (shifts  query) (Repa.Z :. i :. Repa.All)

-- | Returns weight of a given chemical shift at a given position within a fragment.
shiftWeights :: (RealFloat b) => Int -> String -> Maybe b
shiftWeights relativeIndex name = assert (relativeIndex <= 1 && relativeIndex >= (-1)) $
                                  L.lookup name (weightsList !! (relativeIndex + 1))

weightsList :: (RealFloat b) => [[(String, b)]]
weightsList = [[("seqsim", -0.5), ("HA", 37), ("CA", 11), ("CB",  9), ("CO", 5), ("N",1  ), ("H", 1  )] -- -1 on seq
              ,[("seqsim", -2.5), ("HA", 31), ("CA", 14), ("CB", 14), ("CO", 6), ("N",1.5), ("H", 0.3)] --  0 on seq
              ,[("seqsim", -1.5), ("HA", 37), ("CA",  7), ("CB",  7), ("CO", 4), ("N",2  ), ("H", 1.5)] -- +1 on seq
              ]

weightNames :: [String]
weightNames = map fst . head $ weightsList

showsMatrix :: Repa.Source r Double => Repa.Array r Repa.DIM2 Double -> ShowS
showsMatrix m = joinWith '\n' . map makeLine . repaToLists2 $ m
  where
    joinWith :: Char -> [ShowS] -> ShowS
    joinWith c = foldr1 (\a b -> a . (c:) . b)
    makeLine :: [Double] -> ShowS = joinWith ' ' . map (showsPrec 1)

showsMatrix' :: [[Double]] -> ShowS
showsMatrix' m = joinWith '\n' . map makeLine $ m
  where
    joinWith :: Char -> [ShowS] -> ShowS
    joinWith c = foldr1 (\a b -> a . (c:) . b)
    makeLine :: [Double] -> ShowS = joinWith ' ' . map (showsPrec 1)

