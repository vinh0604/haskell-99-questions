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

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome [x] = True
isPalindrome xs = xs == myReverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (++) [] $ map flatten xs

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = foldl (\ys y -> if y == last ys then ys else ys ++ [y]) [x] xs

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack xs = foldr (\z (y:ys) -> if z == head y then (z:y):ys else [z]:y:ys) [[last xs]] (init xs)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1,x)]
encode xs = foldr (\z (y:ys) -> if z == snd y then (fst y + 1, snd y):ys else (1, z):y:ys) [(1, last xs)] (init xs)
