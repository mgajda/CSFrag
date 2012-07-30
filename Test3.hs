{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main where --(main) where

import System.Environment(getArgs)
import Control.Monad(forM_)
import Text.ParseCSV(parseCSV)
import Data.Text(Text, strip)
import qualified Data.Text.IO         as TextIO
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Array.Repa      as Repa
import Data.Array.Repa((:.)(..), Z(..), DIM2)
import Control.Exception(assert)
import Data.Binary
import RepaBin
--import Data.Vector.Unboxed.Base(Unbox(..))

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
                         Right arr -> do print $ map (\(_a, vals) -> length vals) arr
                                         m <- Repa.computeUnboxedP $ mkMatrix headers arr
                                         print m

{- 
instance (Repa.Shape sh, Data.Vector.Unboxed.Base.Unbox a, Binary sh,
      Binary a) => Binary (Repa.Array Repa.U sh a) where
  put arr = do put $  Repa.extent arr
               put $  Repa.toList arr
  get     = do sh  <- get
               l   <- get
               return $ Repa.fromListUnboxed sh l
 -}

main = getArgs >>=
       (flip forM_) processFile

