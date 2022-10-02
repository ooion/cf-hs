{-# LANGUAGE ScopedTypeVariables #-}

--import Data.ByteString.Builder.Extra (flush) -- for interactive problems
import Control.Monad
import Control.Monad.Trans.State
import Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Lazy.Char8 as P
import Data.Maybe
import System.IO

mainFun :: SP Builder
mainFun = do
  casnum <- getInt
  ress <- forM [1 .. casnum] $ \_ -> do
    n <- getInt
    m <- getInt
    let res
          | n == 1 || m == 1 = [1, 1] -- isolate on [1,1]
          | n >= 4 || m >= 4 = [1, 1] -- no isolate
          | otherwise = [2, 2]
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