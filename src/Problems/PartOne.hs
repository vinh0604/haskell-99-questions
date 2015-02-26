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

elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt xs i = if length xs >= i then last $ take i xs
                                   else undefined

myLength :: [a] -> Int
myLength = foldr (\_ n -> 1 + n) 0

myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x:xs) []
