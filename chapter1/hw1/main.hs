-- exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <=0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <=0 = []
    | otherwise =  [n `mod` 10] ++ toDigitsRev (n `div` 10)

-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = zipWith (*) (cycle [1,2]) (reverse n)


-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x `div` 10 >= 1 = sumDigits (toDigits x) + sumDigits xs
    | otherwise = x + sumDigits xs

-- exercise 4
validate :: Integer -> Bool
validate n = if (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0 then True else False

-- exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a,b)] ++ (hanoi (n - 1) c b a)