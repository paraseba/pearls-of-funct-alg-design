import Criterion.Main
import Test.QuickCheck
import Data.List ((\\), nub)
import Data.Array
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V

spec :: Ord a => Int -> ([a],[a]) -> a
spec k (xs, ys) = union (xs, ys) !! k

union :: Ord a => ([a],[a]) -> [a]
union (xs, []) = xs
union ([], ys) = ys
union (x:xs, y:ys)
  | x < y = x : union (xs, y:ys)
  | x > y = y : union (x:xs, ys)

smallest :: Ord a => Int -> ([a],[a]) -> a
smallest k ([ ],ws) = ws !! k
smallest k (zs,[ ]) = zs !! k
smallest k (zs,ws) =
  case (a < b, k <= p+q) of
    (True,True) -> smallest k (zs,us)
    (True,False) -> smallest (k - p - 1) (ys,ws)
    (False,True) -> smallest k (xs,ws)
    (False,False) -> smallest (k - q - 1) (zs,vs)
  where p = length zs `div` 2
        q = length ws `div` 2
        (xs,a : ys) = splitAt p zs
        (us,b : vs) = splitAt q ws

-- there are some bugs in the book for this code,
-- at least for the edition and GHC version I have
smallestArray :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallestArray k (xa,ya) = search k (0, m + 1) (0, n + 1)
  where
    (0,m) = bounds xa
    (0,n) = bounds ya
    search k (lx, rx) (ly, ry)
      | lx == rx = ya ! (ly + k)  -- ly and lx are the base offset for the ya and xa arrays

      | ly == ry = xa ! (lx + k)  -- because now we are not splitting them

        -- lengths must be computed against the base offset
      | otherwise = case (xa ! mx < ya ! my, k <= mx + my - lx - ly) of
          (True,True) -> search k (lx, rx) (ly, my)
          (True,False) -> search (k - mx - 1 + lx) (mx + 1, rx) (ly, ry)
          (False,True) -> search k (lx, mx) (ly, ry)
          (False,False) -> search (k - my - 1 + ly) (lx, rx) (my + 1, ry)
          where mx = (lx+rx) `div` 2
                my = (ly+ry) `div` 2

smallestVector :: (Ord a, VG.Vector v a) => Int -> (v a, v a) -> a
smallestVector k (zs,ws)
  | VG.null zs = ws VG.! k
  | VG.null ws = zs VG.! k
  | otherwise = case (a < b, k <= p+q) of
    (True,True) -> smallestVector k (zs,us)
    (True,False) -> smallestVector (k - p - 1) (ys,ws)
    (False,True) -> smallestVector k (xs,ws)
    (False,False) -> smallestVector (k - q - 1) (zs,vs)
  where p = VG.length zs `div` 2
        q = VG.length ws `div` 2
        (xs,ys') = VG.splitAt p zs; a = VG.head ys'; ys = VG.tail ys'
        (us,vs') = VG.splitAt q ws; b = VG.head vs'; vs = VG.tail vs'

testSmallest :: NonNegative Int -> OrderedList Int -> OrderedList Int -> Property
testSmallest (NonNegative k') (Ordered xs'') (Ordered ys'') =
  m > 0 ==>
    res == smallest k (xs, ys) .&&.
      res == (smallestArray k (xa,ya)) .&&.
      res == (smallestVector k (vx,vy))
  where
    (xs', ys') = (nub xs'', nub ys'')
    (xs, ys) = (xs' \\ ys', ys' \\ xs')
    xa = listArray (0, length xs - 1) xs
    ya = listArray (0, length ys - 1) ys
    vx = V.fromList xs
    vy = V.fromList ys
    m = length xs + length ys
    k = k' `mod` m
    res = spec k (xs, ys)

test :: IO ()
test = quickCheckWith opts testSmallest
  where
    opts = stdArgs {maxSuccess = 5000}

main :: IO ()
main = do
  test
  defaultMain [
    bgroup "selection" [
        bench "array" $ whnf (smallestArray n) $ (xsa,ysa)
      , bench "vector" $ whnf (smallestVector n) $ (xsv,ysv)
    ]
    ]

  where n = 100000000
        xs = [1,3..n]
        ys = [0,2..2*n]
        xsa = listArray (0, length xs - 1) xs
        ysa = listArray (0, length ys - 1) ys
        xsv = V.fromList xs
        ysv = V.fromList ys


{-
+++ OK, passed 5000 tests.
benchmarking selection/array
time                 638.6 ns   (635.0 ns .. 644.0 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 636.7 ns   (635.0 ns .. 641.3 ns)
std dev              8.061 ns   (786.1 ps .. 14.24 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarking selection/vector
time                 700.9 ns   (700.2 ns .. 702.0 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 700.7 ns   (700.4 ns .. 701.2 ns)
std dev              1.374 ns   (1.012 ns .. 1.883 ns)

-}
