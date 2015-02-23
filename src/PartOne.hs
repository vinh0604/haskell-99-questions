module HaskellNinetyNineQuestions.PartOne where

myLast :: [a] -> a
myLast [] = []
myLast [x] = x
myLast (x:xs) = myLast xs
