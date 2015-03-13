module Problems.PartTwo where

data EncodedElement a = Single a | Multiple Int a

instance (Show a) => Show (EncodedElement a) where
    show (Single a) = show a
    show (Multiple t a) = show t ++ " " ++ show a

instance (Eq a) => Eq (EncodedElement a) where
    (Single a1) == (Single a2) = a1 == a2
    (Multiple t1 a1) == (Multiple t2 a2) = t1 == t2 && a1 == a2
    (Single a1) == (Multiple t a2) = t == 1 && a1 == a2
    (Multiple t a1) == (Single a2) = t == 1 && a1 == a2

processEncodedElement :: Eq a => a -> [EncodedElement a] -> [EncodedElement a]
processEncodedElement _ [] = []
processEncodedElement a (Multiple t x:xs) = if a == x then Multiple (1 + t) x:xs else Single a:Multiple t x:xs
processEncodedElement a (Single x:xs) = if a == x then Multiple 2 x:xs else Single a:Single x:xs

encodeModified :: Eq a => [a] -> [EncodedElement a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified xs = foldr processEncodedElement [Single $ last xs] (init xs)

decodeElement :: Eq a => EncodedElement a -> [a]
decodeElement (Single x) = [x]
decodeElement (Multiple t x) = replicate t x

decodeModified :: Eq a => [EncodedElement a] -> [a]
decodeModified = foldr (\x xs -> decodeElement x ++ xs) []

encodeDirect :: Eq a => [a] -> [EncodedElement a]
encodeDirect = encodeModified

dupli :: [a] -> [a]
dupli = foldr (\x xs -> [x,x] ++ xs) []

repli :: [a] -> Int -> [a]
repli xs n
  | n <= 0 = []
  | otherwise = foldr (\x ys -> replicate n x ++ ys) [] xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs 0 = xs
dropEvery xs count = helper xs count
    where helper [] _ = []
          helper (_:ys) 1 = helper ys count
          helper (y:ys) n = y:helper ys (n-1)

split :: [a] -> Int -> ([a],[a])
split xs n
  | n < 0 = ([], xs)
  | otherwise = (take n xs,drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs m n
  | n < m = []
  | m < 1 = helper xs 0 n
  | otherwise = helper xs (m-1) (n-m+1)
    where helper ys i k = take k $ drop i ys
