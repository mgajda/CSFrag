{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeOperators #-}
module Main where

import Data.Array.Repa           as Repa
import Data.Array.Repa.Arbitrary as Repa
import qualified Data.Array.Repa.Repr.Vector as RepaV
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Util(repaToLists2)
import Outer

forAllArb :: (Show a, Arbitrary a, Testable prop) => (a -> prop) -> Property
forAllArb = forAll arbitrary

checkFromFunction2 :: (Eq a, Num a, Show a, Data.Vector.Unboxed.Base.Unbox a,Arbitrary a) =>a -> Property
checkFromFunction2 phantom = (forAllArb
                                 (\(arr1 :: Array U DIM1 a) -> forAllArb
                                     (\(arr2 :: Array U DIM1 a) -> test arr1 arr2))){- &&
                              (forAllArb
                                 (\(arr1 :: Array RepaV.V DIM1 a) -> forAllArb
                                     (\(arr2 :: Array RepaV.V DIM1 a) -> test arr1 arr2))))-}

  where
    test arr1 arr2 = (nonEmpty arr1 && nonEmpty arr2)
                        ==> (computeDot arr1 arr2 ==
                               repaToLists2 (computeDotFromFunction arr1 arr2))
      where
        nothing = phantom + arr1 ! Repa.ix1 undefined

prop_checkFromFunction2Float  = checkFromFunction2 (0.0::Float)

prop_checkFromFunction2Double = checkFromFunction2 (0.0::Double)

nonEmpty ::  Source r e => Array r (Z :. Int) e -> Bool
nonEmpty = let len1d (Z :. i) = i >= 2 in len1d . Repa.extent

-- | Checks outer product of two symmetric shapes, given a phantom type of a product
checkFromFunction :: (Eq a, Num a, Show a, Arbitrary a) => (Z :. Int) -> a -> Property
checkFromFunction sh phantom = Repa.forAll2VShaped sh test
  where
    test (arr1, arr2) = nonEmpty sh ==> (computeDot arr1 arr2 ==
                                         repaToLists2 (computeDotFromFunction arr1 arr2))
      where
        nothing = phantom + arr1 ! Repa.ix1 undefined
    nonEmpty (Z :. i) = i >= 2

-- | Checks outer product of two symmetric shapes, given a phantom type of a product
checkOuter1 :: (Eq a, Num a, Show a, Arbitrary a) => (Z :. Int) -> a -> Property
checkOuter1 sh phantom = Repa.forAll2VShaped sh test
  where
    test (arr1, arr2) = nonEmpty sh ==> (repaToLists2 (outer1 (*) arr1 arr2) ==
                                         computeDot arr1 arr2)
      where
        nothing = phantom + arr1 ! Repa.ix1 undefined
    nonEmpty (Z :. i) = i >= 2

computeDot :: (Num t, Source r1 t, Source r t) =>Array r DIM1 t -> Array r1 DIM1 t -> [[t]]
computeDot arr1 arr2 = [[answer (Z:. dbi :. qi)
                           | dbi <- [0..(len1d arr2) - 1]]
                          |  qi  <- [0..(len1d arr1) - 1]]
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((arr1 Repa.! (Repa.ix1 qi)) *
                        (arr2 Repa.! (Repa.ix1 dbi )) )
    len1d arr = let (Z :. i) = Repa.extent arr
                in i :: Int

computeDotFromFunction :: (Num a, Source r1 a, Source r a) =>Array r DIM1 a -> Array r1 DIM1 a -> Array D ((Z :. Int) :. Int) a
computeDotFromFunction arr1 arr2 = Repa.fromFunction (Z :. len1d arr1 :. len1d arr2)
                                   answer
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((arr1 Repa.! (Repa.ix1 dbi)) *
                        (arr2 Repa.! (Repa.ix1 qi ))         )
    len1d arr = let Z :. i = Repa.extent arr
                in i

prop_checkFromFunctionFloat sh = checkFromFunction sh (0.0::Float)

prop_checkFromFunctionDouble sh = checkFromFunction sh (0.0::Double)

prop_checkOuter1Float sh = checkOuter1 sh (0.0::Float)

prop_checkOuter1Double sh = checkOuter1 sh (0.0::Double)

main = $quickCheckAll
