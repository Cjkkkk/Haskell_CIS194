class Functor f where
    fmap :: (a->b) -> f a -> f b
class (Functor f) => Applicative f where
    pure :: a-> f a
    (<*>) :: f (a->b) -> f a -> f b

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

pure (+) <*> Just 3 <*> Just 5
-- pure f <*> x <*> y <*> .. allow us to take a function that expects parameters that aren't necessarily wrapped in functors and use that function to operate on several values that are in functor contexts.

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x| f<- fs, x<- xs]

[(*0), (+100), (^2)] <*> [1,2,3] -- 产生9个项

(*) <$> [2,5,10] <*> [8,10,11]

instance Applica