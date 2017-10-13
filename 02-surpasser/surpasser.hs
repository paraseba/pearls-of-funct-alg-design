#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p "haskell.packages.ghc802.ghcWithPackages (pkgs: with pkgs; [QuickCheck criterion])"

import Data.List (splitAt)
import Criterion.Main
import Test.QuickCheck
import System.Random

tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = (x:xs):tails xs

mscSpec :: Ord a => [a] -> Int
mscSpec as =  maximum [scount z zs | (z:zs) <- tails as]

scount :: Ord a => a -> [a] -> Int
scount z = length . filter (>z)

table :: Ord a => [a] -> [(a, Int)]
table [x] = [(x,0)]
table xs = join (m-n) (table ys) (table zs)
  where
    m = length xs
    n = m `div` 2
    (ys,zs) = splitAt n xs

join :: Ord a => Int -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join _ txs [] = txs
join _ [] tys = tys
join n txs@((x,c) : txs') tys@((y,d) : tys')
  | x < y = (x,c+n) : join n txs' tys
  | otherwise = (y,d): join (n-1) txs tys'

msc :: Ord a => [a] -> Int
msc = maximum . map snd . table

testMsc :: NonEmptyList Int -> Bool
testMsc (NonEmpty as) = msc as == mscSpec as

main :: IO ()
main = do
  quickCheckWith opts testMsc
  defaultMain [
    bgroup "surpasser" [
        bench "specification" $ whnf mscSpec list
      , bench "implementation" $ whnf msc list
                    ]
    ]

  where
    opts = stdArgs {maxSuccess = 10000}
    list =  take 10000 $ randomRs (0, 5000 :: Int) (mkStdGen 42)

{-

+++ OK, passed 10000 tests.
benchmarking surpasser/specification
time                 223.8 ms   (220.2 ms .. 227.2 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 227.8 ms   (227.0 ms .. 229.2 ms)
std dev              1.538 ms   (130.7 μs .. 2.025 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking surpasser/implementation
time                 14.03 ms   (13.99 ms .. 14.06 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 14.06 ms   (14.03 ms .. 14.09 ms)
std dev              60.72 μs   (40.07 μs .. 83.40 μs)

-}
