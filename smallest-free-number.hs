{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Data.Array.IArray
import Data.Array.Unboxed (UArray)
import qualified Data.Array.MArray as MA
import Data.Array.ST
import Control.Monad.ST (ST)
import Data.List (partition, nub)
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

search :: (IArray a Bool, Ix i) => a i Bool -> Int
search = length . takeWhile id . elems

countlist :: Int -> [Int] -> Array Int Int
countlist n = accumArray (+) 0 (0,n) . flip zip (repeat 1)

sort :: Int -> [Int] -> [Int]
sort n xs =
  concat [replicate count n | (n, count) <- assocs (countlist n xs)]

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

testSmall :: IO ()
testSmall = SC.smallCheck 7 smallWorks

testAll = testSmall >> testRandom

main = do
  testAll

  defaultMain [
    bgroup "minfree" [
        bench "specification" $ whnf minfree list
      , bench "accumArray" $ whnf minfreeAccumArray list
      , bench "mutable ST Mono" $ whnf minfreeMono list
      , bench "mutable ST" $ whnf minfreeST list
      , bench "mutable ST Unboxed" $ whnf minfreeSTU list
      , bench "countList" $ whnf minfreeCountList list
      , bench "divide and conq" $ whnf minfreeDivConq list
                    ]
    ]
  where
    list = nub $ filter (/= (n - 444)) $ take (n*10) $ randomRs (0, n) stdGen
    n = 20000
    stdGen = mkStdGen 42
    specResult = minfree list

{-

Completed 8660 tests without failure.
+++ OK, passed 10000 tests.
benchmarking minfree/specification
time                 1.299 s    (1.299 s .. 1.299 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.299 s    (1.299 s .. 1.299 s)
std dev              71.03 μs   (0.0 s .. 81.89 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking minfree/accumArray
time                 1.100 ms   (1.098 ms .. 1.101 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.099 ms   (1.097 ms .. 1.100 ms)
std dev              3.767 μs   (3.005 μs .. 4.754 μs)

benchmarking minfree/mutable ST Mono
time                 557.0 μs   (556.5 μs .. 557.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 557.2 μs   (556.4 μs .. 558.3 μs)
std dev              2.951 μs   (1.942 μs .. 4.991 μs)

benchmarking minfree/mutable ST
time                 556.2 μs   (555.7 μs .. 556.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 556.4 μs   (555.7 μs .. 557.0 μs)
std dev              2.109 μs   (1.622 μs .. 2.878 μs)

benchmarking minfree/mutable ST Unboxed
time                 145.2 μs   (145.2 μs .. 145.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 145.2 μs   (145.2 μs .. 145.3 μs)
std dev              95.13 ns   (29.13 ns .. 181.8 ns)

benchmarking minfree/countList
time                 1.007 ms   (1.006 ms .. 1.009 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.005 ms   (1.003 ms .. 1.007 ms)
std dev              7.035 μs   (5.811 μs .. 8.976 μs)

benchmarking minfree/divide and conq
time                 2.178 ms   (2.171 ms .. 2.186 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.189 ms   (2.183 ms .. 2.197 ms)
std dev              22.63 μs   (17.78 μs .. 31.10 μs)

-}
