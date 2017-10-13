#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p "haskell.packages.ghc802.ghcWithPackages (pkgs: with pkgs; [smallcheck QuickCheck criterion])"

{-# LANGUAGE FlexibleContexts #-}

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import Data.Array.ST
import Control.Monad.ST (ST)
import Data.List (partition, nub)
import qualified Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as UV
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SCS

import System.Random
import Criterion.Main

(\\) :: Eq a => [a] -> [a] -> [a]
as \\ bs = filter (not . flip elem bs) as

minfree :: [Int] -> Int
minfree xs = head ([0.. ] \\ xs)

minfreeAccumArray :: [Int] -> Int
minfreeAccumArray = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n)
  [(i, True) | i <- xs, i < n]
  where n = length xs

minfreeST :: [Int] -> Int
minfreeST = search . checklistST

minfreeSTU :: [Int] -> Int
minfreeSTU = search . checklistSTU

minfreeMono :: [Int] -> Int
minfreeMono = search . checklistMono

minfreeV :: [Int] -> Int
minfreeV = searchV . checklistVector

checklistComp :: MArray a Bool m => [Int] -> m (a Int Bool)
checklistComp xs = do
    a <- newArray (0,n) False
    sequence_ [writeArray a x True | x <- xs, x <= n]
    return a
  where n = length xs
-- Without specializing performance degrades a lot compared to minfreeMono
{-# SPECIALIZE checklistComp :: [Int] -> ST s (STArray s Int Bool) #-}
{-# SPECIALIZE checklistComp :: [Int] -> ST s (STUArray s Int Bool) #-}

-- Very interesting: try to write checklistST point-free style, related to
-- impredicative types and the ($) hack
checklistST :: [Int] -> Array Int Bool
checklistST xs = runSTArray $ checklistComp xs

checklistSTU :: [Int] -> UArray Int Bool
checklistSTU xs = runSTUArray $ checklistComp xs

checklistMono :: [Int] -> Array Int Bool
checklistMono xs = runSTArray $ do
    a <- newArray (0,n) False
    sequence_ [writeArray a x True | x <- xs, x <= n]
    return a
  where n = length xs

checklistVector :: [Int] -> V.Vector Bool
checklistVector xs = V.create $ do
    v <- UV.replicate (n+1) False
    sequence_ [UV.write v x True | x <- xs, x <= n]
    return v
  where n = length xs

search :: (IArray a Bool, Ix i) => a i Bool -> Int
search = length . takeWhile id . elems

searchV :: V.Vector Bool -> Int
searchV = V.length . V.takeWhile id

countlist :: Int -> [Int] -> Array Int Int
countlist n = accumArray (+) 0 (0,n) . flip zip (repeat 1)

sort :: Int -> [Int] -> [Int]
sort n xs =
  concat [replicate count val | (val, count) <- assocs (countlist n xs)]

minfreeCountList :: [Int] -> Int
minfreeCountList xs = search' $ countlist n (filter (<n) xs)
  where n = length xs

search' :: Array Int Int -> Int
search' ar = head [n | (n, count) <- assocs ar, count == 0]


minfreeDivConq :: [Int] -> Int
minfreeDivConq xs = minfrom 0 (length xs,xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n,xs)
  | n == 0 = a
  | m == b - a = minfrom b (n - m,vs)
  | otherwise = minfrom a (m,us)
    where (us,vs)=partition (< b) xs
          b = a + 1 + n `div` 2
          m = length us


works :: [Int] -> Bool
works xs =
   minfreeAccumArray input == res &&
   minfreeST input == res &&
   minfreeMono input == res &&
   minfreeSTU input == res &&
   minfreeV input == res &&
   minfreeCountList input == res &&
   minfreeDivConq input == res
  where
    res = minfree input
    input = nub $ filter (>= 0) xs

randomWorks :: [QC.NonNegative Int] -> Bool
randomWorks = works . map QC.getNonNegative

smallWorks :: [SCS.NonNegative Int] -> Bool
smallWorks =
  works . map SCS.getNonNegative

testRandom :: IO ()
testRandom = QC.quickCheckWith opts randomWorks
  where opts = QC.stdArgs {QC.maxSuccess = 10000}

testSort :: IO ()
testSort = QC.quickCheckWith opts sorting
  where
    opts = QC.stdArgs {QC.maxSuccess = 10000}
    sorting :: [QC.NonNegative (QC.Small Int)] -> Bool
    sorting xs = sort 1000 input == Data.List.sort input
      where input = nub [x | QC.NonNegative (QC.Small x) <- xs, x < 1000]

testSmall :: IO ()
testSmall = SC.smallCheck 7 smallWorks

testAll :: IO ()
testAll = testSort >> testSmall >> testRandom

main :: IO ()
main = do
  testAll

  defaultMain [
    bgroup "minfree" [
        bench "specification" $ whnf minfree list
      , bench "accumArray" $ whnf minfreeAccumArray list
      , bench "mutable ST Mono" $ whnf minfreeMono list
      , bench "mutable ST" $ whnf minfreeST list
      , bench "mutable ST Unboxed" $ whnf minfreeSTU list
      , bench "vector" $ whnf minfreeV list
      , bench "countList" $ whnf minfreeCountList list
      , bench "divide and conq" $ whnf minfreeDivConq list
                    ]
    ]
  where
    list = nub $ filter (/= (n - 444)) $ take (n*10) $ randomRs (0, n) stdGen
    n = 20000
    stdGen = mkStdGen 42

{-

+++ OK, passed 10000 tests.
Completed 8660 tests without failure.
+++ OK, passed 10000 tests.
benchmarking minfree/specification
time                 1.270 s    (1.241 s .. 1.293 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.299 s    (1.291 s .. 1.316 s)
std dev              14.19 ms   (0.0 s .. 14.30 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking minfree/accumArray
time                 1.056 ms   (1.054 ms .. 1.058 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.056 ms   (1.054 ms .. 1.059 ms)
std dev              9.142 μs   (6.198 μs .. 14.15 μs)

benchmarking minfree/mutable ST Mono
time                 530.6 μs   (529.8 μs .. 531.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 530.8 μs   (530.0 μs .. 532.1 μs)
std dev              3.516 μs   (2.533 μs .. 5.428 μs)

benchmarking minfree/mutable ST
time                 534.0 μs   (530.8 μs .. 537.7 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 533.0 μs   (531.4 μs .. 535.3 μs)
std dev              6.598 μs   (4.712 μs .. 9.226 μs)

benchmarking minfree/mutable ST Unboxed
time                 144.1 μs   (143.6 μs .. 144.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 144.2 μs   (143.8 μs .. 144.8 μs)
std dev              1.666 μs   (1.091 μs .. 2.442 μs)

benchmarking minfree/vector
time                 136.0 μs   (134.1 μs .. 138.3 μs)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 134.4 μs   (133.4 μs .. 136.3 μs)
std dev              4.084 μs   (2.486 μs .. 6.224 μs)
variance introduced by outliers: 28% (moderately inflated)

benchmarking minfree/countList
time                 1.737 ms   (1.723 ms .. 1.750 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.722 ms   (1.717 ms .. 1.728 ms)
std dev              17.96 μs   (14.11 μs .. 22.20 μs)

benchmarking minfree/divide and conq
time                 2.123 ms   (2.116 ms .. 2.132 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.125 ms   (2.119 ms .. 2.134 ms)
std dev              24.30 μs   (19.12 μs .. 31.01 μs)

-}
