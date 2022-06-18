module Ch5 where
import Data.Char ()

-- Ch5,#1. Sum of first 100 squares
sumsquares :: Int ->  Int
sumsquares n = sum [x^2 | x <-  [1..n]]

-- Ch5,#2. Make an m x n grid

grid :: Int -> Int -> [(Int, Int)]

grid m n = [(x,y) | x <- [0..m], y <- [0..n], x /= y]

-- Ch5,#3. Make an n x n grid that excludes the main diagonal

square :: Int -> [(Int, Int)]

square n = grid n n

--Ch5,#4. make your own replicate. replicate :: Int -> a -> [a]. takes an integer n, then an  object, and returns an array of length n of that object 

myReplicate :: Int -> a -> [a]

myReplicate n a = [a | x <- [1..n]]

-- Ch5,#5. Use a list comprehension with 3 variables to generate all pythagorean triples up to a limit

pyths :: Int -> [(Int,Int,Int)]

pyths n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x^2 + y^2 == z^2]

-- Ch5,#6. Make a function using list comprehension that returns all perfect numbers up to an upper limit. A perfect number is equal to the sum of its factors
-- factors copied from textbook, given in problem
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- could be a more efficient way to get rid of the last element but i am MOST lazy
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum ( drop 1 (reverse (factors x))) == x]

-- Ch5,#7. Redefine [(x,y) | x<-[1,2], y<-[3,4]] using two comprehensions with one generator each
oldP :: IO ()
oldP = print ([(x,y) | x<-[1,2], y<-[3,4]])


myP :: IO ()
myP = print (concat [ [(x,y) | x <- [1,2]] | y<-[3,4] ])

--Ch5,#8. Rewrite positions, which takes in an element and a list and returns the positions that that element is in, using the find function, which searches all key,value pairs and returns all values with the matching key

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

find :: Eq a => a -> [(a,b)] -> [b] 
find k t = [v | (k',v) <- t, k == k']

myPositions :: Eq a => a -> [a] -> [Int]
myPositions x xs = find x (zip xs [0..])

--Ch5,#9. Write a function to take the dot product of two lists of integers

dotproduct :: [Int] -> [Int] -> Int
dotproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

