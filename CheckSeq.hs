{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Database

import System.Environment(getArgs)
import Control.Monad(forM)

main = do fnames <- getArgs
          forM fnames $ \fname ->
            do putStr $ fname ++ ": "
               db <- decodeCompressedFile fname
               print $ resArray (db :: Database)