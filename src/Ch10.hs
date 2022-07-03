module Ch10 where

hangman :: IO ()

hangman = do 
    putStr "enter your name: "
    word <- getLine
    putStrLn "lets guess!"
    play word

play :: String -> IO ()
play w = putStrLn (concat (replicate (length w) "*"))

-- Ch10,#1. Redefine putStr using a list comprehension and sequence_
putStrMy :: String -> IO ()
putStrMy [] = putChar '.'
putStrMy cs = sequence_ ([putChar c  | c<- cs] ++ [putChar '\n'])