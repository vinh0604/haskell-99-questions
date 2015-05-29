module Problems.PartThree where

import System.Random

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
  g <- getStdGen
  return $ [ xs !! x | x <- take num $ randomRs (0, length xs - 1) g ]
