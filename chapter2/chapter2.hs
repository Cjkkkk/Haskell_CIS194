data Thing = Shoe
    | Ship
    | SealingWax
    | Cabbage
    | King
    deriving Show

shoe :: Thing
shoe = Shoe

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

-- OK Failure都是data constructor
data FailableDouble = Failure | OK Double
    deriving Show


safeDiv:: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d


data Person = Person String Int String
    deriving Show
baz :: Person -> String
baz p@(Person n _ _) = "the name field of (" ++ show p ++ ") is " ++ n


ex :: Int
ex = case "HELLo" of 
    [] -> 3
    ('H':s) -> length s
    _ -> 7