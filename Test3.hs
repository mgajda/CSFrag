{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import System.Environment(getArgs)
import Control.Monad(forM_)
import Text.ParseCSV(parseCSV)
import Data.Text(Text, strip)
import qualified Data.Text.IO as TextIO
import qualified Data.Attoparsec.Text as Atto

convEntry :: [Text] -> Either String (Text, [Int])
convEntry (rescode:others) =
    do let c = strip rescode
       case redParse $ map parseInt others of
               Left  msg     -> Left  msg
               Right pothers -> Right (c, pothers)
  where
    parseInt ft = case strip ft of
                    "" -> Right (-1)
                    fs -> Atto.parseOnly Atto.decimal fs

redParse []              = Right []
redParse (Left msg:_   ) = Left msg
redParse (Right f :rest) = case redParse rest of
    Right prest -> Right $ f:prest
    Left  msg   -> Left msg

processFile fname = do txt <- TextIO.readFile fname 
                       let Right result = parseCSV txt
                       putStr "Header:"
                       let headers = map strip $ head result
                       print $ map (map strip) $ tail result
                       print $ map convEntry $ tail result
                       let arrP = redParse $ map convEntry $ tail result
                       print headers
                       print arrP
                       case arrP of
                         Left  msg -> return ()
                         Right arr -> print $ map (\(_a, vals) -> length vals) arr

main = getArgs >>=
       (flip forM_) processFile

