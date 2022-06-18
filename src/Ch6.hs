module Ch6 where

-- RECURSION
-- Ch6,#2. write a recursive function to sum down to 0 from a given integer
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown (x-1)

-- Ch6,#3. Write a recursive exponentiation function using *

power :: Int -> Int -> Int

power _ 0 = 1
power x y = x * power x (y-1)

-- Ch6,#4. Write a recursive version of Euclid's algorithm. I have never seen this version to be fair, but it does seem to work

euclid :: Int -> Int -> Int

euclid x y = if x == y then x else euclid (min x y) (abs (x-y))

-- Ch6,#6. Construct some basic functions using recursion.

andmy :: [Bool] -> Bool
andmy [] = True
andmy (b:bs) = b && andmy bs

concatmy :: [[a]] -> [a]
concatmy [] = []
concatmy (l:ls) = l ++ concatmy ls

replicatemy :: Int -> a -> [a]
replicatemy 0 _ = []
replicatemy n x = [x] ++ replicatemy (n-1) x

indexmy :: Int -> [a] -> a
indexmy 0 xs = head xs
indexmy n xs = indexmy (n-1) (drop 1 xs)

elemmy :: Eq a => a -> [a] -> Bool
elemmy _ [] = False
elemmy x xs = (x == head xs) || elemmy x (drop 1 xs)