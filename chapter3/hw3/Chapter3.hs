module Chapter3 
( every_nth
, skips
, localMaxima 
, histogram
) where
import Data.List
every_nth :: Int -> Int -> [b] -> [b]
every_nth n u l = [l !! i | i <- [n-1, 2*n-1..u-1]]

skips :: [a] -> [[a]]
skips x = [every_nth idx (length x) x | idx <- [1..length x]] 


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest) = if y > x && y > z 
    then y : localMaxima (y:z:rest)
    else localMaxima (y:z:rest) 

countElem :: [Int] -> [Int]
countElem l = map (\x -> length $ elemIndices x l) [0..9]

row :: Int ->[Int]-> [Char]
row n l = [if i >= n then '*' else ' '| i <- l]

-- histogram :: [Int] -> String
-- histogram l = foldr (++) " " [ row n ns | n<-[9,8..1]]
--     where ns = countElem l

histogram :: [Int] -> String
histogram l = intercalate "\n" [ row n ns | n<-[cel,cel-1..1]]
    where ns = (countElem l)
          cel = maximum ns