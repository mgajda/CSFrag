{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Database

import System.Environment(getArgs)
import Control.Monad(forM)
import qualified Data.Array.Repa as Repa

main = do fnames <- getArgs
          forM fnames $ \fname ->
            do putStr $ fname ++ ": "
               db <- readDB fname
               putStrLn . Repa.toList . resArray $ db