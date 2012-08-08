{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, FlexibleInstances #-}
module Main where

import Data.Array.Repa as R
import Data.List       as L
import Debug.Trace(trace)
import Test.QuickCheck as Q
import Test.QuickCheck.All(quickCheckAll)
import Outer

prop_outer1_a :: Bool
prop_outer1_a = arr `deepSeqArray` True
  where
    arr = outer1 (*) v v
    v :: R.Array R.U DIM1 Double = R.fromListUnboxed (R.ix1 3) [1..3.0]

instance Arbitrary (R.Array R.U DIM1 Double) where
  arbitrary = do l <- arbitrary
                 return . R.fromListUnboxed (R.ix1 . L.length $ l) $ l 

prop_outer1_b :: R.Array R.U DIM1 Double -> R.Array R.U DIM1 Double -> Bool
prop_outer1_b v w = let a = outer1 (*) v w
                        indexValue i@(Z :. x :. y) = (((v ! ix1 x) * (w ! ix1 y)) == a ! i) :: Bool
                    in Prelude.all indexValue . allIndices . R.extent $ a

allIndices sh = Prelude.map (fromIndex sh) [0..size sh-1]

main = $quickCheckAll

