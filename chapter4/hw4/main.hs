fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs
fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> y * (x - 2) ) 1 . filter (\x -> x `mod` 2 == 0)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)
generate :: Int -> [Int]
generate 1 = []
generate n
    | even n = n : (generate $ n `div` 2)
    | otherwise = (generate $ 3 * n + 1)
fun2' :: Int -> Int
fun2' = foldr (+) 0 . generate 


-- main = 
--     print $ fun1' [3,3,4,5,6]