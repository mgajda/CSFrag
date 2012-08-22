{-# LANGUAGE FlexibleInstances #-}
module Data.Array.Repa.RepaNFData() where

import qualified Data.Array.Repa             as Repa
import qualified Data.Array.Repa.Repr.Vector as RepaV
import qualified Data.Vector                 as V
import Control.DeepSeq

instance (NFData a) => NFData (Repa.Array RepaV.V sh a) where
  rnf = rnf . RepaV.toVector

instance (NFData a) => NFData (V.Vector a) where
  rnf v = V.foldr (\a b -> rnf a `seq` b) () v