module Test
(
    Point(..)
    ,Shape(..)
    ,nudge
    ,doubleMe
) where
doubleMe x = x * x

-- doubleSmall x = if x > 100
--     then x
--     else x*2

-- con = "hello iam con"

-- [x*2|x<-[1,2..10]]
-- -- list compresion

-- [x*2|x<-[1,2..10],x `mod` 7 == 3]

-- length' xs = sum [1|_<-xs]

-- removeCap :: [Char] -> [Char]
-- removeCap xs = [x|x<-xs,x `element` [A,B..Z]]

-- removeCap :: String -> String
-- removeCap xs = [x|x<-xs,x `element` [A,B..Z]]

-- sayMe ::(Integral a)=>a->String
-- sayMe 1 = "one"
-- sayMe 2 = "two"
-- sayMe x = "others"

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  

-- case expression of pattern -> result  
-- pattern -> result  
-- pattern -> result  

mutliTHree :: (Num a)=>a -> a ->a ->a
mutliTHree x y z = x*y*z


filter' ::(a->Bool)->[a]->[a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $(y2 - y1))


data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) 

nudge :: Shape->Float->Float->Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (x+b)) r
