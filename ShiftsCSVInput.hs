{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ShiftsCSVInput(processInputFile, ShiftsInput(..)) where --(main) where

import System.Environment(getArgs)
import Control.Monad(forM_)
import Text.ParseCSV(parseCSV)
import Data.Text(Text, strip, unpack)
import qualified Data.Text.IO         as TextIO
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Array.Repa      as Repa
import Data.Array.Repa((:.)(..), Z(..), DIM2)
import Control.Exception(assert)
import Data.Binary
--import Data.Vector.Unboxed.Base(Unbox(..))

data ShiftsInput = ShiftsInput { headers :: [String]
                               , shifts  :: Repa.Array Repa.U Repa.DIM2 Double
                               }

convEntry :: [Text] -> Either String (Text, [Double])
convEntry (rescode:others) =
    do let c = strip rescode
       case redParse $ map parseDouble others of
               Left  msg     -> Left  msg
               Right pothers -> Right (c, pothers)
  where
    parseDouble ft = case strip ft of
                       "" -> Right (-1)
                       fs -> Atto.parseOnly Atto.double fs

redParse []              = Right []
redParse (Left msg:_   ) = Left msg
redParse (Right f :rest) = case redParse rest of
    Right prest -> Right $ f:prest
    Left  msg   -> Left msg

invIndex :: DIM2 -> DIM2
invIndex (Z :. x :. y) = (Z :. y :. x)

symmetrize arr = Repa.traverse arr id xform
  where
    xform lookup i = case lookup i of
                       (-1) -> lookup $ invIndex i
                       v    -> v

--mkMatrix :: [Text] -> [(Text, [Int])] -> Array r DIM2 Float
mkMatrix headers list = assert (map fst list == tail headers)  $
                        assert (squareMatrix . map snd $ list) $
                        symmetrize arr
  where
    squareMatrix m = all ((==size) . length) m
    size           = length list
    arr            = Repa.fromListUnboxed (Repa.ix2 size size) . concat . map snd $ list

processInputFile fname = do txt <- TextIO.readFile fname 
                            let Right result = parseCSV txt
                            let headers = map strip $ head result
                            let arrP = redParse $ map convEntry $ tail result
                            case arrP of
                              Left  msg -> return Nothing
                              Right arr -> do --print $ map (\(_a, vals) -> length vals) arr
                                              m <- Repa.computeUnboxedP $ mkMatrix headers arr
                                              return $ Just ShiftsInput { headers = map unpack headers
                                                                        , shifts  = m
                                                                        }


-- TODO: reorder input array by headers

-- TODO: extract input conversion into separate (non-IO) function returning Either Text ShiftsInput