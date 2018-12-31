-- doubleMe :: [Char] -> [Char]
-- doubleMe x = x ++ x
-- main = do
--     putStrLn "hello, what is your name"
--     name <- getLine
--     putStrLn $ "Hey "++ name ++ ", you rock!" ++ doubleMe "k"
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reversewords line
            main
reversewords :: String -> String
reversewords = unwords . map reverse . words