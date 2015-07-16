module Problems.PartThree where

import System.Random
import Data.List

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs t
  | t <= 0 = undefined
  | t > (length xs + 1) = undefined
  | otherwise = take i xs ++ [x] ++ drop i xs
    where i = t - 1

range :: Int -> Int -> [Int]
range m n
  | m > n = []
  | m == n = [m]
  | otherwise = m:range (m+1) n

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs num = do
  g <- newStdGen
  return $ [ xs !! x | x <- take num $ randomRs (0, length xs - 1) g ]

pickRandom :: Eq a => [a] -> Int -> StdGen -> [a]
pickRandom [] _ _ = []
pickRandom [x] _ _ = [x]
pickRandom _ 0 _ = []
pickRandom list num gen = element:pickRandom newList (num - 1) newGen
  where (r, newGen) = randomR (0, length list - 1) gen
        element = list !! r
        newList = delete element list

diff_select :: Int -> Int -> IO [Int]
diff_select num m = do
  g <- newStdGen
  return $ pickRandom (range 1 m) num g

rnd_permu :: Eq a => [a] -> IO [a]
rnd_permu xs = do
  g <- newStdGen
  return $ pickRandom xs (length xs) g

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                    , ys <- combinations (n-1) xs']

group3 :: Eq a => [Int] -> [a] -> [[[a]]]
group3 [a,b,c] xs = [ [x,y,z] | x <- combinations a xs,
                                  y <- combinations b (xs \\ x),
                                  z <- combinations c (xs \\ (x ++ y))]

group' :: Eq a => [Int] -> [a] -> [[[a]]]
group' [] _ = [[]]
group' _ [] = [[]]
group' (size:sizes) xs = do
                           x <- combinations size xs
                           xs' <- group' sizes (xs \\ x)
                           return (x:xs')
