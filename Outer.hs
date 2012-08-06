{-# LANGUAGE TypeOperators #-}
module Outer(outer2,
             outer1)
where

import Data.Array.Repa               as R
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as U

-- | Takes 2-dimensional A, transposed(B) arguments, and computes outer product A*B.
--   A[m, n] and transposed(B)[m,l], shape of result is A*B[n,l].
outer2
  :: (Num a, Source r1 a, Source r a,
      U.Unbox a,
      Elt a) =>
     Array r1 ((Z :. Int) :. Int) a
     -> Array r ((Z :. Int) :. Int) a -> Array D ((Z :. Int) :. Int) a
outer2 a b = fromFunction resultShape generator
  where
    x1, y1, x2, y2 :: Int
    (Z :. x1 :.y1) = extent a
    (Z :. x2 :.y2) = extent b
    resultShape    = Z :. y1 :. y2
    row i a        = slice a (Z :. All :. (i :: Int))
    generator (Z :. y1 :. y2) = sumAllS $ row y1 a -^ row y2 b

-- | Takes 1-dimensional arrays A[n] and B[m],
--   zips elements each-against-each with function f :: a -> b -> c
--   and produces an outer product array C[n,m] :: Array ... c.
outer1
  :: (Source r1 e1, Source r e) =>
     (e1 -> e -> a)
     -> Array r1 DIM1 e1
     -> Array r DIM1 e
     -> Array D ((Z :. Int) :. Int) a
outer1 f a b = fromFunction resultShape generator
  where
    x, y :: Int
    (Z :. x)  = extent a
    (Z :. y)  = extent b
    resultShape             = Z :. x :. y
    generator (Z :. x :. y) = (a ! ix1 x) `f` (b ! ix1 y)

