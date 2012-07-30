{-# LANGUAGE FlexibleInstances #-}
module Utils() where

import qualified Data.Array.Repa as Repa
import qualified Data.Vector.Unboxed.Base
import Data.Binary

-- | This is an instance of Binary for subclass of Repa arrays.
instance (Repa.Shape sh, Data.Vector.Unboxed.Base.Unbox a, Binary sh,
      Binary a) => Binary (Repa.Array Repa.U sh a) where
  put arr = do put $  Repa.extent arr
               put $  Repa.toList arr
  get     = do sh  <- get
               l   <- get
               return $ Repa.fromListUnboxed sh l

