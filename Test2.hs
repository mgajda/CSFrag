module Main(main) where

import System.Environment(getArgs)
import Control.Monad(forM_)
import Text.ParseCSV(parseCSV)
import Data.Text(Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Attoparsec.Text as Atto

convEntry :: [Text] -> Either String (Int, Text, [Double])
convEntry (resi:resname:others) =
  case Atto.parseOnly Atto.decimal resi of
    Left  msg       -> Left msg
    Right presi     -> 
      case redParse $ map parseFloat others of
          Left  msg     -> Left msg
          Right pothers -> Right (presi, resname, pothers)
  where
    parseFloat ft = Atto.parseOnly Atto.double ft

redParse []             = Right []
redParse (Left msg:_)    = Left msg
redParse (Right f :rest) = case redParse rest of
    Right prest -> Right $ f:prest
    Left  msg   -> Left msg

processFile fname = do txt <- TextIO.readFile fname -- "K18_alone_shifty.csv"
                       let Right result = parseCSV txt
                       putStr "Header:"
                       let headers = head result
                       let arrP = redParse $ map convEntry $ tail result
                       print headers
                       print arrP
                       case arrP of
                         Left  msg -> return ()
                         Right arr -> print $ map (\(_a, _b, vals) -> length vals) arr

main = getArgs >>=
       (flip forM_) processFile

