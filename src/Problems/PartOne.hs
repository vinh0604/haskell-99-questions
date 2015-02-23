module Problems.PartOne where

myLast :: [a] -> a
myLast [] = undefined
myLast [x] = x
myLast (x:xs) = myLast xs
