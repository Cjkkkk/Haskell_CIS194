-- main = do 
--     line <- fmap reverse getLine
--     putStrLn $ "you said "++ line ++" backwards!"

import Data.List
import Data.Char

main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line