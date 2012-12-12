{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleInstances, TypeOperators, FlexibleContexts #-}
module Main where

import Data.Vector.Unboxed(Unbox(..))
import Data.Array.Repa           as R
import Data.Array.Repa.Arbitrary as R
import Data.List                 as L
import Test.QuickCheck           as Q
import Test.QuickCheck.Property
import Test.QuickCheck.All(quickCheckAll)
import Debug.Trace(trace)
import Outer
import qualified Data.Array.Repa.Repr.Vector as RepaV
import Util(repaToLists2)
import Outer

forAllArb :: (Show a, Arbitrary a, Testable prop) => (a -> prop) -> Property
forAllArb = forAll arbitrary

checkFromFunction2 :: (Eq a, Num a, Show a, Unbox a, Arbitrary a) => a -> Property
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
        nothing = phantom + arr1 ! R.ix1 undefined

prop_checkFromFunction2Float  = checkFromFunction2 (0.0 :: Float )

prop_checkFromFunction2Double = checkFromFunction2 (0.0 :: Double)

nonEmpty ::  Source r e => Array r DIM1 e -> Bool
nonEmpty = let len1d (Z :. i) = i >= 2 in len1d . R.extent

-- | Checks outer product of two symmetric shapes, given a phantom type of a product
checkFromFunction :: (Eq a, Num a, Show a, Arbitrary a) => DIM1 -> a -> Property
checkFromFunction sh phantom = R.forAll2VShaped sh test
  where
    test (arr1, arr2) = nonEmpty sh ==> (computeDot arr1 arr2 ==
                                         repaToLists2 (computeDotFromFunction arr1 arr2))
      where
        nothing = phantom + arr1 ! R.ix1 undefined
    nonEmpty (Z :. i) = i >= 2

-- | Checks outer product of two symmetric shapes, given a phantom type of a product
checkOuter1 :: (Eq a, Num a, Show a, Arbitrary a) => DIM1 -> a -> Property
checkOuter1 sh phantom = R.forAll2VShaped sh test
  where
    test (arr1, arr2) = nonEmpty sh ==> (repaToLists2 (outer1 (*) arr1 arr2) ==
                                         computeDot arr1 arr2)
      where
        nothing = phantom + arr1 ! R.ix1 undefined
    nonEmpty (Z :. i) = i >= 2

computeDot :: (Num t, Source r1 t, Source r t) =>Array r DIM1 t -> Array r1 DIM1 t -> [[t]]
computeDot arr1 arr2 = [[answer (Z:. dbi :. qi)
                           | dbi <- [0..(len1d arr2) - 1]]
                          |  qi  <- [0..(len1d arr1) - 1]]
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((arr1 R.! (R.ix1 qi)) *
                        (arr2 R.! (R.ix1 dbi )) )
    len1d arr = let (Z :. i) = R.extent arr
                in i :: Int

computeDotFromFunction :: (Num a, Source r1 a, Source r a) =>Array r DIM1 a -> Array r1 DIM1 a -> Array D DIM2 a
computeDotFromFunction arr1 arr2 = R.fromFunction (Z :. len1d arr1 :. len1d arr2)
                                   answer
  where
    answer (Z :. dbi :. qi ) = answerSeq dbi qi
    answerSeq dbi qi = ((arr1 R.! (R.ix1 dbi)) *
                        (arr2 R.! (R.ix1 qi ))         )
    len1d arr = let Z :. i = R.extent arr
                in i

prop_checkFromFunctionFloat sh = checkFromFunction sh (0.0::Float)

prop_checkFromFunctionDouble sh = checkFromFunction sh (0.0::Double)

prop_checkOuter1Float sh = checkOuter1 sh (0.0::Float)

prop_checkOuter1Double sh = checkOuter1 sh (0.0::Double)
prop_outer1_a :: Bool
prop_outer1_a = arr `deepSeqArray` True
  where
    arr = outer1 (*) v v
    v :: R.Array R.U DIM1 Double = R.fromListUnboxed (R.ix1 3) [1..3.0]

{-
instance (Arbitrary a, U.Unbox a) => Arbitrary (Array U DIM2 a) where
  arbitrary = sized (\nSize ->
              sized (\mSize ->
    do n <- choose (0, nSize)
       m <- choose (0, mSize)
       l <- sequence [ arbitrary | _ <- [1..n], _ <- [1..m] ]
       return $ fromListUnboxed (Z :. n :. m) l))
  shrink xs = []

instance Arbitrary (R.Array R.U DIM1 Double) where
  arbitrary = do l <- arbitrary
                 return . R.fromListUnboxed (R.ix1 . L.length $ l) $ l 
-}

prop_outer1_b :: R.Array R.U DIM1 Double -> R.Array R.U DIM1 Double -> Bool
prop_outer1_b v w = let a = outer1 (*) v w
                        indexValue i@(Z :. x :. y) = (((v ! ix1 x) * (w ! ix1 y)) == a ! i) :: Bool
                    in Prelude.all indexValue . allIndices . R.extent $ a

prop_slice_x :: Array U DIM2 Float -> Int -> Property
prop_slice_x v i = nonZeroExtentX v ==> checkElts $ sliceX v i

prop_slice_y :: Array U DIM2 Float -> Int -> Property
prop_slice_y v i = nonZeroExtentY v ==> checkElts $ sliceY v i

nonZeroExtentX arr = x > 0
  where
    Z :. x :. y = extent arr

nonZeroExtentY arr = y > 0
  where
    Z :. x :. y = extent arr

checkElts arr = all (\i -> (arr ! i) == (arr ! i)) . allIndices . extent $ arr

sliceX arr i = slice arr (Z :. (i `mod` x) :. All)
  where Z:.x:.y = extent arr

sliceY arr i = slice arr (Z :. All :. i `mod` y)
  where Z:.x:.y = extent arr

allIndices sh = Prelude.map (fromIndex sh) [0..size sh-1]

--main = print "QUQU!"

main = $quickCheckAll

