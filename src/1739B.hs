{-# LANGUAGE ScopedTypeVariables #-}

--import Data.ByteString.Builder.Extra (flush) -- for interactive problems

import Control.Monad
import Control.Monad.Trans.State
import Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Lazy.Char8 as P
import Data.Maybe
import Debug.Trace
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

mainFun :: SP Builder
mainFun = do
  casnum <- getInt
  ress <- forM [1 .. casnum] $ \_ -> do
    n <- getInt
    d <- replicateM n getInt
    let a = scanl1 (+) d
    let only = all (\(x, y) -> x < y || y == 0) $ zip a (tail d)
    let res = if only then a else [-1]
    pure res
  pure $ foldl1 (<>) $ map putInts ress

type SP = State P.ByteString

type S = StateT P.ByteString

dropSpace :: P.ByteString -> P.ByteString
dropSpace = P.dropWhile (<= ' ') -- not exactly right, but close enough

--getNext :: Monad m => S m P.ByteString
--getNext = state $ P.span (> ' ') . dropSpace

getInt :: Monad m => S m Int
getInt = state $ fromJust . P.readInt . dropSpace

putInts :: [Int] -> Builder
putInts vs =
  let sepPrim =
        (,) ' '
          Prim.>$< Prim.liftFixedToBounded Prim.char7 Prim.>*< Prim.intDec
   in case vs of
        [] -> char7 '\n'
        x : xs -> intDec x <> Prim.primMapListBounded sepPrim xs <> char7 '\n'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  inp <- P.getContents
  let outp = evalState mainFun inp
  P.putStr $ toLazyByteString outp