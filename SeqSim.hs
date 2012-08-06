{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module SeqSim( SeqSimWeights(..)
             , readWeights
             , readWeightsFromFile
             , getDefaultWeightsFilename
             , prepareSeqSim
             , seqSim )
where

import System.FilePath
import Control.Monad(forM_)
import Text.ParseCSV(parseCSV)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as L
import qualified Data.Text            as T
import qualified Data.Text.IO         as TextIO
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Array.Repa      as Repa
import Data.Array.Repa((:.)(..), Z(..), DIM2)
import qualified Data.Vector as V
import Control.Exception(assert)
import Data.Binary
import qualified Data.Char(ord)

data SeqSimWeights = SeqSimWeights { aaCodes   :: BS.ByteString -- used as array of char
                                   , aaIndices :: V.Vector Int
                                   , matrix    :: Repa.Array Repa.U Repa.DIM2 Int 
                                   } deriving (Show)

-- TODO: merge convEntry, redParse, invIndex with other source files?
convEntry :: [T.Text] -> Either String (T.Text, [Int])
convEntry (rescode:others) =
    do let c = T.strip rescode
       case redParse $ map parseInt others of
               Left  msg     -> Left  msg
               Right pothers -> Right (c, pothers)
  where
    parseInt ft = case T.strip ft of
                    "" -> Right (-1)
                    fs -> Atto.parseOnly Atto.decimal fs

redParse []              = Right []
redParse (Left msg:_   ) = Left msg
redParse (Right f :rest) = case redParse rest of
    Right prest -> Right $ f:prest
    Left  msg   -> Left msg

invIndex :: DIM2 -> DIM2
invIndex (Z Repa.:. x Repa.:. y) = (Z Repa.:. y Repa.:. x)

symmetrize arr = Repa.traverse arr id xform
  where
    xform lookup i = case lookup i of
                       (-1) -> lookup $ invIndex i
                       v    -> v

mkMatrix headers list = assert (map fst list == tail headers ) $
                        assert (squareMatrix . map snd $ list) $
                        symmetrize arr
  where
    squareMatrix m = all ((==size) . length) m
    size           = length list
    arr            = Repa.fromListUnboxed (Repa.ix2 size size) . concat . map snd $ list

computeIndices headers = V.replicate 256 (-1) V.// L.zip ords [0..]
  where
    ords = L.map Data.Char.ord (BS.unpack headers)

-- | TODO: getDefaultWeightsFilename should find data dir!
getDefaultWeightsFilename = return $ "." </> "share" </> "seqsim.txt"

readWeights = getDefaultWeightsFilename >>= readWeightsFromFile

prepareSeqSim = (maybe errMsg SeqSim.seqSim
                   `fmap` SeqSim.readWeights)
  where
    errMsg = error "Cannot find file with sequence similarity weights!"

readWeightsFromFile fname = do txt <- TextIO.readFile fname 
                               let Right result = parseCSV txt
                               let headers = map T.strip $ head result
                               let arrP = redParse . map convEntry . tail $ result
                               case arrP of
                                 Left  msg -> return Nothing
                                 Right arr -> do m <- Repa.computeUnboxedP $ mkMatrix headers arr
                                                 let codes = BS.pack . L.concat . map T.unpack $ headers
                                                 return . Just $ SeqSimWeights codes
                                                                               (computeIndices codes)
                                                                               m

-- | Compute sequence match score for a given pair of characters
seqSim (SeqSimWeights codes indices matrix) a b = matrix Repa.! (Z Repa.:. findInd a Repa.:. findInd b)
  where
    findInd x = if i >= 0
                  then i
                  else error $ "Cannot find index for " ++ show x
      where i = indices V.! Data.Char.ord x
    ai = findInd a
    bi = findInd b

