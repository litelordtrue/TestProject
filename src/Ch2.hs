{-# LANGUAGE TemplateHaskellQuotes #-}
module Ch2 where

-- Ch2,#3: Script contains syntax errors, fix them. The errors were a capital first letter of a function, not the correct backwards quote, and incorrect indenting
n = a `div` length xs 
    where
        a = 10
        xs = [1,2,3,4,5]

--Ch2,#4: Rewrite last using other library functions


slowlast :: [a] -> a
slowlast (a) = head (reverse (a))