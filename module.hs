-- import Test
import qualified Test
-- data Person = Person String String Int deriving (Show)

data Person = Person{
    firstName :: String
    ,lastName :: String
    ,age :: Int
}deriving (Show)

let guy = Person {firstname = "kk",lastName = "bb",age = 19}
-- firstName :: Person -> String
-- firstName (Person firstname _ _) = firstname 

double x = x + x
minus x = x - 1
(double . minus) 4