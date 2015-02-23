module Problems.PartOne where

myLast :: [a] -> a
myLast [] = undefined
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = undefined
myButLast [x] = undefined
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs
