data IntList = Empty | Cons Int IntList
    deriving Show

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons a b) = Cons (abs a) (absAll b)

-- 泛化IntList
-- 引入Polymorphism data type
data List t = E | C t (List t)
    deriving Show

-- polymorphic functions
mapList :: (t -> t) -> List t -> List t
mapList _ E = E
mapList p (C a b) = C (p a) (mapList p b)

list1 :: List Int
list1 = C 3 (C 5 E)

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C a b)
    | p a = C a (filterList p b)
    | otherwise = filterList p b

-- data Maybe a = Nothing | Just a 

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x