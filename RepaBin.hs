{-# LANGUAGE FlexibleInstances, RankNTypes, UndecidableInstances, OverlappingInstances #-}
module RepaBin() where

import qualified Data.Array.Repa      as Repa
import Data.Array.Repa((:.)(..), Z(..), DIM2)
import Data.Vector.Unboxed.Base(Unbox(..))
import Data.Binary
import Data.Array.Repa.Shape(listOfShape, shapeOfList)

type StorableArray sh a = (Repa.Shape sh
                          ,Data.Vector.Unboxed.Base.Unbox a
                          ,Binary     sh
                          ,Binary     a                    ) =>Repa.Array Repa.U sh a

instance (Repa.Shape sh, Data.Vector.Unboxed.Base.Unbox a, Binary sh,
      Binary a) => Binary (Repa.Array Repa.U sh a) where
  put arr = do put $  Repa.extent arr
               put $  Repa.toList arr
  get     = do sh  <- get
               l   <- get
               return $ Repa.fromListUnboxed sh l


instance (Repa.Shape sh) => Binary sh where
  get    = shapeOfList `fmap` get
  put sh = put $ listOfShape sh

