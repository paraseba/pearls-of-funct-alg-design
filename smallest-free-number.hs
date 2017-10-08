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

