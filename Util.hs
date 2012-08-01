{-# LANGUAGE CPP #-}
module Util(withParallel,
            setupParallel,
            stopParallel)
where

#ifdef __GLASGOW_HASKELL__
import GHC.Conc
#endif
import Control.Concurrent.ParallelIO(stopGlobalPool)

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
