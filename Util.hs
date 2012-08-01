{-# LANGUAGE CPP #-}
module Util(withParallel
           ,setupParallel
           ,stopParallel
           ,repaFromList1
           ,repaFromLists2)
where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif
import Control.Concurrent.ParallelIO(stopGlobalPool)
import qualified Data.Array.Repa as Repa
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