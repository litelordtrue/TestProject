module Ch7 where

-- Ch7,#1. Rewrite [f x | x <- xs, p x] using map and filter

numone :: (a -> b) -> [a] -> (a -> Bool) -> [b]
numone f xs p = map f (filter p xs)

numtwo :: (t -> a) -> [t] -> (t -> Bool) -> [a]
numtwo f xs p = [f x | x <- xs, p x]

-- Ch7,#3. Redefine map f and filter p using foldr
mapmy :: (a -> b) -> [a] -> [b]
mapmy f = foldr ((:) . f) []

filtermy :: (a -> Bool) -> [a] -> [a]
filtermy _ [] = []
filtermy p (x:xs) = if p x then x : filtermy p xs else filtermy p xs

-- Ch7,#4. Using foldl, write a function that converts a list of decimals into an integer.
-- non foldl solution using a dotproduct with increasing powers of 10. 
dotproduct :: [Int] -> [Int] -> Int
dotproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- extremely elegant foldl solution.
dec2int :: [Int] -> Int
dec2int =  foldl (\x y -> 10*x + y) 0