sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise       = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
    | "Haskell" > "C++" = 3
    | otherwise = 4
foo n
    | n < 0 =0
    | otherwise = n + 3

nums, range, ranges :: [Integer]
nums = [1,2,3,4]
range = [1..100]
ranges = [1,2..100]

l :: [Integer]
l = [x*2 | x <- [1..10]] 

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)