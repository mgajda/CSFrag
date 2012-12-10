{-# LANGUAGE TemplateHaskell #-}
module Bug where

import Data.Array.Repa           as Repa
import Data.Array.Repa.Arbitrary as Repa
import qualified Data.Array.Repa.Repr.Vector as RepaV
import Test.QuickCheck
import Test.QuickCheck.All
import Util(repaToLists2)

checkFromFunction sh phantom = Repa.forAll2VShaped sh test
  where
    test (arr1, arr2) = nonEmpty sh ==> (computeScores arr1 arr2 ==
                                         repaToLists2 (computeScores'' arr1 arr2))
      where
        nothing = phantom + arr1 ! Repa.ix1 undefined
    nonEmpty (Z :. i) = i >= 2

prop_checkFromFunctionFloat sh = checkFromFunction sh (0.0::Float)

prop_checkFromFunctionDouble sh = checkFromFunction sh (0.0::Double)

-- | Compute a score without using Repa compound operations
--computeScores :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Float) -> [[Float]]
computeScores arr1 arr2 = [[answer (Z:. dbi :. qi)
                                      | dbi <- [0..(len1d arr2) - 1]]
                                     |  qi  <- [0..(len1d arr1) - 1]]
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((arr1 Repa.! (Repa.ix1 qi)) *
                        (arr2 Repa.! (Repa.ix1 dbi )) )
    len1d arr = let Z :. i = Repa.extent arr
                in i

--computeScores'' :: [(String, Int, Int)] -> ShiftsInput -> Database -> (Char -> Char -> Float) -> Repa.Array Repa.D Repa.DIM2 Float
computeScores'' arr1 arr2 = Repa.fromFunction (Z :. len1d arr1 :. len1d arr2)
                                   answer
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((arr1    Repa.! (Repa.ix1 dbi)) *
                        (arr2 Repa.! (Repa.ix1 qi ))         )
    len1d arr = let Z :. i = Repa.extent arr
                in i

{-
repaToLists2
  :: Repa.Source r t => RepaV.Array r Repa.DIM2 t -> [[t]]
repaToLists2 m  = [[m Repa.! (Repa.ix2 i j)
                      | i <- [0..x-1]]
                     | j <- [0..y-1]]
  where
    [x, y] = Repa.listOfShape $ Repa.extent m
    l      = Repa.toList m
-}

main = $quickCheckAll
