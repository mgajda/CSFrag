{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleInstances, TypeOperators, FlexibleContexts #-}
module Main where

import qualified Data.Vector.Unboxed as U
import Data.Array.Repa     as R
import Data.List           as L
import Test.QuickCheck     as Q
import Debug.Trace(trace)
import Test.QuickCheck.All(quickCheckAll)
import Outer

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

prop_slice_x :: Array U (Z :. Int :. Int) Float -> Int -> Property
prop_slice_x v i = nonZeroExtentX v ==> checkElts $ sliceX v i

prop_slice_y :: Array U (Z :. Int :. Int) Float -> Int -> Property
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

