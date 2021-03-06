{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ShiftsCSVInput(processInputFile,
                      ShiftsInput(..) )
where

import Prelude hiding(sequence)
import System.Environment(getArgs)
import qualified System.IO(stderr, hPutStrLn)
import Control.Monad(forM_)
import GHC.Float(double2Float)
import Data.CSV.Conduit.Types(CSVSettings(..))
import Data.CSV.Conduit.Parser.Text(parseCSV)
import Data.Text(Text, strip, unpack)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TextIO
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Array.Repa      as Repa
import Data.Array.Repa((:.)(..), Z(..), DIM2)
import Data.Array.Repa.Eval()
import Control.Exception(assert)
import Data.Binary
import qualified Data.List as L
--import Data.Vector.Unboxed.Base(Unbox(..))

data ShiftsInput = ShiftsInput { headers     :: [String]
                               , shiftLabels :: [String]
                               , resseq      :: Repa.Array Repa.U Repa.DIM1 Char  -- (nRes + nStruct)
                               , resnums     :: [Int]
                               , shifts      :: Repa.Array Repa.U Repa.DIM2 Float
                               }

printErr = System.IO.hPutStrLn System.IO.stderr

convEntry :: [Text] -> Either String (Int, Text, [Float])
convEntry (resnum:rescode:others) =
    case parseInt resnum of
      Left  msg -> Left msg
      Right num -> do let c = strip rescode
                      case redParse $ map parseFloat others of
                        Left  msg     -> Left  msg
                        Right pothers -> Right (num, c, pothers)
  where
    parseFloat ft = case strip ft of
                      "" -> Right (-1)
                      fs -> Atto.parseOnly (double2Float `fmap` Atto.double) fs
    parseInt   ft = case strip ft of
                      "" -> Right (-1)
                      fs -> Atto.parseOnly Atto.decimal fs

redParse []              = Right []
redParse (Left msg:_   ) = Left msg
redParse (Right f :rest) = case redParse rest of
    Right prest -> Right $ f:prest
    Left  msg   -> Left msg

invIndex :: DIM2 -> DIM2
invIndex (Z :. x :. y) = Z :. y :. x

symmetrize arr = Repa.traverse arr id xform
  where
    xform lookup i = case lookup i of
                       (-1) -> lookup $ invIndex i
                       v    -> v

-- TODO: strip spaces from sequence
-- TODO: assure that each aa code is a single character
mkMatrix :: [Text] -> [(Int, Text, [Float])] -> ([Int], Text, Repa.Array Repa.U DIM2 Float)
mkMatrix headers list = assert dimensions (nums, T.concat aSeq, arr)
  where
    headerLen      = L.length . tail . tail $ headers
    dimensions     = all ((==headerLen) . length) ary
    (nums, aSeq, ary) = unzip3 list
    arr            = Repa.fromListUnboxed (Repa.ix2 headerLen $ length ary) . concat $ ary

processInputFile fname = do txt <- TextIO.readFile fname 
                            case parseCSV (CSVSettings ',' $ Just '\"') txt of
                              Left msg -> do printErr msg
                                             return Nothing
                              Right result -> do let header = map strip $ head result
                                                 let arrP = redParse $ map convEntry $ tail result
                                                 case arrP of
                                                   Left  msg -> do printErr msg
                                                                   return Nothing
                                                   Right arr -> do let (nums, seq, m) = mkMatrix header arr
                                                                   --mat <- Repa.computeUnboxedP m
                                                                   return $ Just ShiftsInput { shiftLabels = tail . tail $ map unpack header
                                                                                             , headers  = map unpack header
                                                                                             , resnums = nums
                                                                                             , resseq  = Repa.fromListUnboxed (Repa.ix1 . T.length $ seq) (T.unpack seq)
                                                                                             , shifts   = m
                                                                                             }


-- TODO: reorder input array by headers

-- TODO: extract input conversion into separate (non-IO) function returning Either Text ShiftsInput
