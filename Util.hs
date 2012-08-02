{-# LANGUAGE CPP #-}
module Util(withParallel
           ,setupParallel
           ,stopParallel
           ,repaFromList1
           ,repaFromLists2
           ,repaConcat1d
           ,repaConcat2d)
where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif
import Control.Concurrent.ParallelIO(stopGlobalPool)
import qualified Data.Array.Repa     as Repa
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as V
import Control.Exception(assert)

--   Here is code for parallellism
-- | Sets up as many capabilities as we have processors.
#ifdef __GLASGOW_HASKELL__
setupParallel = GHC.Conc.getNumProcessors >>= GHC.Conc.setNumCapabilities
#else
setupParallel = return ()
#endif

-- | Finalization of parallel pool.
stopParallel = stopGlobalPool

-- | Wraps parallel-io computation with setupParallel and stopParallel.
--   NOTE: Not yet exception-proof.
withParallel act = do setupParallel
                      r <- act
                      stopParallel
                      return r

repaFromList1  l = Repa.fromListUnboxed (Repa.ix1 . length $ l) l

repaFromLists2 l = assert allLike $ Repa.fromListUnboxed (Repa.ix2 len2 len1) $ concat l
  where
    len2 = length . head $ l -- inner dimension
    len1 = length l          -- outer dimension
    allLike   = all ((==len2) . length) l

repaConcat1d :: (V.Unbox e) => [Repa.Array Repa.U Repa.DIM1 e] -> Repa.Array Repa.U Repa.DIM1 e
repaConcat1d arrays = Repa.fromUnboxed (Repa.ix1 size) . V.concat . L.map Repa.toUnboxed $ arrays
  where
    shapes = L.map (head . Repa.listOfShape . Repa.extent) arrays
    size = L.foldr (+) 0 shapes

repaConcat2d :: (V.Unbox e) => [Repa.Array Repa.U Repa.DIM2 e] -> Repa.Array Repa.U Repa.DIM2 e
repaConcat2d arrays = assert sndDimOk $ Repa.fromUnboxed (Repa.ix2 fstDim sndDim) vector
  where
    shapes   = L.map (Repa.listOfShape . Repa.extent) arrays
    vector   = V.concat (L.map Repa.toUnboxed arrays)
    fstDim   = L.foldr (+) 0 $ map head shapes
    sndDim   = head sndDims
    sndDims  = map (head . tail) shapes
    sndDimOk = all (==sndDim) sndDims
