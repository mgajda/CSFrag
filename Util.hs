{-# LANGUAGE CPP #-}
module Util(withParallel
           ,setupParallel
           ,stopParallel
           ,repaFromList1U
           ,repaFromList1B
           ,repaFromLists2U
           ,repaToLists2
           ,repaConcat1dU
           ,repaConcat1dB
           ,repaConcat2d)
where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif
import Control.Concurrent.ParallelIO(stopGlobalPool)
import qualified Data.Array.Repa     as Repa
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector         as Vec
import qualified Data.Array.Repa.Repr.Vector as RepaV
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
                                        show nCap, " capabilities."]
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

repaFromList1U  l = Repa.fromListUnboxed (Repa.ix1 . length $ l)                l
repaFromList1B  l = RepaV.fromVector     (Repa.ix1 . length $ l) . Vec.fromList $ l

repaFromLists2U l = assert allLike $ Repa.fromListUnboxed (Repa.ix2 len2 len1) $ concat l
  where
    len2 = length . head $ l -- inner dimension
    len1 = length l          -- outer dimension
    allLike   = all ((==len2) . length) l

repaToLists2
  :: Repa.Source r t => RepaV.Array r Repa.DIM2 t -> [[t]]
repaToLists2 m  = [[m Repa.! (Repa.ix2 i j)
                      | i <- [1..x]]
                     | j <- [1..y]]
  where
    [x, y] = Repa.listOfShape $ Repa.extent m
    l      = Repa.toList m

repaConcat1dU :: (V.Unbox e) => [Repa.Array Repa.U Repa.DIM1 e] -> Repa.Array Repa.U Repa.DIM1 e
repaConcat1dU arrays = Repa.fromUnboxed (Repa.ix1 size) . V.concat . L.map Repa.toUnboxed $ arrays
  where
    size= sum $ L.map (head . Repa.listOfShape . Repa.extent) arrays

repaConcat1dB :: [Repa.Array RepaV.V Repa.DIM1 e] -> Repa.Array RepaV.V Repa.DIM1 e
repaConcat1dB arrays = RepaV.fromVector (Repa.ix1 size) . Vec.concat . L.map RepaV.toVector $ arrays
  where
    size = sum $ L.map (head . Repa.listOfShape . Repa.extent) arrays

repaConcat2d :: (V.Unbox e) => [Repa.Array Repa.U Repa.DIM2 e] -> Repa.Array Repa.U Repa.DIM2 e
repaConcat2d arrays = assert fstDimOk $ Repa.fromUnboxed (Repa.ix2 sndDim fstDim) vector
  where
    shapes   = L.map (Repa.listOfShape . Repa.extent) arrays
    vector   = V.concat (L.map Repa.toUnboxed arrays)
    sndDim   = sum $ map (head . tail) shapes
    fstDim   = head fstDims
    fstDims  = map head shapes
    fstDimOk = all (==fstDim) fstDims
