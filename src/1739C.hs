{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

--import Data.ByteString.Builder.Extra (flush) -- for interactive problems
import Control.Monad
import Control.Monad.Trans.State
import Data.Array.Base (UArray (UArray))
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Builder
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Lazy.Char8 as P
import Data.List
import Data.Maybe
import System.IO

mainFun :: SP Builder
mainFun = do
  casnum <- getInt
  ans <- forM [1 .. casnum] $ \_ -> do
    n <- getInt
    let m = div n 2
    let (w, l) = win !! m
    pure [w, l, 1]
  pure $ mconcat $ map putInts ans
  where
    win =
      map (\(a, b, c) -> (b, c)) $
        take (maxM + 1) $
          iterate
            ( \(i, w, l) ->
                (i + 1, l +! combine (2 * i + 1) i, w +! combine (2 * i) (i + 1))
            )
            (0, 0, 0)

maxN = 60

maxM = div maxN 2

facts :: UArray Int Int
facts = listArray (0, maxN) $ scanl' (*!) 1 [1 .. maxN]

ifacts :: UArray Int Int
ifacts = listArray (0, maxN) [inv (facts ! i) | i <- [0 .. maxN]]

combine :: Int -> Int -> Int
combine n m
  | n < m = 0
  | otherwise = (facts ! n) *! (ifacts ! m) *! (ifacts ! (n - m))

modulo :: Int
modulo = 998244353

infixl 6 +!

(+!) :: Int -> Int -> Int
x +! y = x + y -! modulo

infixl 6 -!

(-!) :: Int -> Int -> Int
x -! y =
  let raw = x - y
   in raw + (modulo .&. shiftR raw 63)

infixl 8 *!

(*!) :: Int -> Int -> Int
x *! y = mod (x * y) modulo
--   let raw = mod (x * y) modulo
--    in raw + (modulo .&. shiftR raw 63)

pow :: Int -> Int -> Int
pow =
  let -- I used to use stimes for this, but it seems that's 15x slower
      -- than just the hand-rolled loop, so no more is it used.
      go acc !x 0 = acc
      go !acc !x k =
        if even k
          then go acc (x *! x) (div k 2)
          else go (acc *! x) (x *! x) (div k 2)
   in go 1

inv :: Int -> Int
inv n = pow n (modulo - 2)

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