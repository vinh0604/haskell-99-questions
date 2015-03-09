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
