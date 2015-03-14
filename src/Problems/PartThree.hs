module Problems.PartThree where

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs t
  | t <= 0 = undefined
  | t > (length xs + 1) = undefined
  | otherwise = take i xs ++ [x] ++ drop i xs
    where i = t - 1
