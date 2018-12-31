import qualified Data.Map as Map
data LockerState = Taken|Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ "does not exist!"
        Just (state, code) -> if state == Taken
            then Left $ "Locker number " ++ show lockerNumber ++ " is taken!"
            else Right code

lockers :: LockerMap
lockers = Map.fromList [(100,(Taken,"Zd11")),(101,(Free,"dede"))]

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a)=> a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a)=>a->Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right) 
    | x == a = True
    | x > a = treeElem x right
    | x < a = treeElem x left
-- let nums = [ 8, 6, 4, 1, 7, 3, 5]
-- let numTree = foldr treeInsert EmptyTree nums

-- class Eq a where
--     (==) :: a-> a -> Bool
--     (/=) :: a-> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

data TrafficLight = Green|Red|Yellow
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False 

--无法保证 m是Eq的
-- class (Eq a) => Num a where

class YesNo a where
    yesno :: a-> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe m) where
    yesno (Just x) = True
    yesno Nothing = False

class Functors f where
    fmaps :: (a->b) -> f a -> f b

instance Functors [] where
    fmaps = map