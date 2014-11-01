{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           System.IO(putStrLn)
import           System.Environment(getArgs)
import           Control.Monad     (forM, when)
import qualified Data.Array.Repa as Repa

import Database

main = do fnames <- getArgs
          when (null fnames) $ do
            putStrLn "Usage: CheckSeq <input.str> ..."
            putStrLn "Shows FASTA sequence read from CSFrag database file."
            --putStrLn "Shows FASTA sequence read from NMR-STAR file."
          forM fnames $ \fname ->
            do putStr $ fname ++ ": "
               db <- readDB fname
               putStrLn . Repa.toList . resArray $ db

