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
import Control.Monad(when)
import GHC.Environment(getFullArgs) -- to check RTS params

--   Here is code for parallellism
-- | Sets up as many capabilities as we have processors.
#ifdef __GLASGOW_HASKELL__
setupParallel = do rtsConcArgs <- filter (L.isPrefixOf "-N") `fmap` getFullArgs
                   when (rtsConcArgs == []) $ do
                     nProc <- GHC.Conc.getNumProcessors
                     let nCap = min 12 nProc
                     putStrLn $ concat ["Found ", show nProc, " processors ",
                                        " and no -N argument - initializing ",
                                        show nCap, "capabilities."]
                     GHC.Conc.setNumCapabilities nCap
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
repaConcat2d arrays = assert fstDimOk $ Repa.fromUnboxed (Repa.ix2 sndDim fstDim) vector
  where
    shapes   = L.map (Repa.listOfShape . Repa.extent) arrays
    vector   = V.concat (L.map Repa.toUnboxed arrays)
    sndDim   = L.foldr (+) 0 $ map (head . tail) shapes
    fstDim   = head fstDims
    fstDims  = map head shapes
    fstDimOk = True || (all (==fstDim) fstDims)
