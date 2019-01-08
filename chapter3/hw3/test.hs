import Chapter3

test :: (Eq a, Ord a) => a -> a -> Bool
test l r = l == r 

main = 
    print (test (skips "ABCD") ["ABCD", "BD", "C", "D"]) >>
    print (test (skips "hello!") ["hello!", "el!", "l!", "l", "o", "!"]) >>
    print (test (skips [1]) [[1]]) >>
    print (test (skips [True,False]) [[True,False], [False]])