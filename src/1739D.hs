{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

--import Data.ByteString.Builder.Extra (flush) -- for interactive problems
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.State
import Data.Array
import Data.Array.ST
import Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Lazy.Char8 as P
import Data.List
import Data.Maybe
import qualified Data.Ord
import Debug.Trace
import System.IO

mainFun :: SP Builder
mainFun = do
  casnum <- getInt
  res <- forM [1 .. casnum] $ \_ -> do
    n <- getInt
    m <- getInt
    p <- replicateM (n -1) getInt
    let g = genGraph n (zip p [2 ..])
    let d = calcDepth g
    let d' = sortOn (Data.Ord.Down . snd) d
    let p' = listArray (2, n) p
    let min_h = binSearch (validHeight g m p' d') 1 (n -1)
    pure [min_h]
  pure $ mconcat $ map putInts res

validHeight :: Array Int [Int] -> Int -> Array Int Int -> [(Int, Int)] -> Int -> Bool
validHeight g m p d h = runST $ do
  let n = rangeSize $ bounds g
  visited <- newArray (1, n) False -- :: ST s (STArray s Int Bool)
  let goUp x 0 = x
      --   goUp 1 h = if h > 0 then 0 else 1
      goUp x h = goUp (p ! x) (h -1)

  let visDown :: STArray s Int Bool -> Int -> ST s Int
      visDown vis x = do
        vx <- readArray vis x
        if vx
          then pure 0
          else do
            writeArray vis x True
            mapM_ (visDown vis) (g ! x)
            pure 1

  cuts <- forM d $ \(x, d) -> do
    vx <- readArray visited x
    if not vx && d > h
      then
        let anc = goUp x (h -1)
         in visDown visited anc
      else pure 0
  pure $ sum cuts <= m

binSearch :: (Int -> Bool) -> Int -> Int -> Int
binSearch v l r =
  if l == r
    then l
    else
      let m = div (l + r) 2
       in if v m
            then binSearch v l m
            else binSearch v (m + 1) r

calcDepth :: Array Int [Int] -> [(Int, Int)]
calcDepth g = go 0 1
  where
    go d x =
      let cs = (g ! x)
       in (x, d) : foldl' (++) [] ( map (go (d + 1)) cs)

genGraph :: Int -> [(Int, Int)] -> Array Int [Int]
genGraph n es = runSTArray $ do
  arr <- newArray (1, n) [] :: ST s (STArray s Int [Int])
  forM_ es $ \(a, b) -> do
    lst <- readArray arr a
    writeArray arr a (b : lst)
  return arr

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