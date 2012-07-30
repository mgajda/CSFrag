{-# LANGUAGE FlexibleInstances #-}
module RepaBin() where

import qualified Data.Array.Repa      as Repa
import Data.Array.Repa((:.)(..), Z(..), DIM2)
import Data.Vector.Unboxed.Base(Unbox(..))
import Data.Binary

instance (Repa.Shape sh, Data.Vector.Unboxed.Base.Unbox a, Binary sh,
      Binary a) => Binary (Repa.Array Repa.U sh a) where
  put arr = do put $  Repa.extent arr
               put $  Repa.toList arr
  get     = do sh  <- get
               l   <- get
               return $ Repa.fromListUnboxed sh l

