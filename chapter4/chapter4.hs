greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs

foo :: (b->c) -> (a->b) -> (a->c)
foo f g = \x -> f $ g x
-- foo (+4) (+5) 3 = 12

a :: Integer -> Integer
a = (+3)

b :: Integer -> Integer
b = (+2)

c :: Integer
c = 3