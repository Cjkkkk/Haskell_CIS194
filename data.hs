data Vector a = Vector a a a deriving(Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)


data Maybe a = Nothing | Just a

data Person = Person {
    firstName::String
    ,age::Int
}deriving (Eq, Show, Read)

read  "Person {firstName = \"kk\", age = 19}" :: Person

data Day = Monday | Tuesday | Wednesday | Thusday | Friaday | Saturday | Sunday 
    deriving(Show, Read, Ord , Bounded)

minBounded :: Day

data Either a b = Left a|Right b deriving (Eq, Ord, Read, Show)