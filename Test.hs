module Main(main) where

import Data.ByteString.Char8 as BS
import Text.CSV.ByteString(parseCSV)
import System.IO(openFile, IOMode(ReadMode))

main = do Just csv <- parseCSV `fmap` (openFile "K18_alone_shifty.csv" ReadMode >>= BS.hGetContents)
          print $ csv !! 0
