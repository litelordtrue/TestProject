-- Ch4,#1.
halve :: [a] -> ([a], [a])

halve xs = splitAt w xs
    where
        w = length xs `div` 2

-- Ch4,#2. return the third element of a list  with at least 3 elements using:
-- Ch4,#2a. head and tail

thirda :: [a] -> a

thirda xs = head(tail (tail xs))

-- Ch4,#2b. list indexing. The !! operator is used as so. [a] !! Int

thirdb :: [a] -> a

thirdb xs  = xs !! 2

-- Ch4,#2c. Using pattern matching.

thirdc :: [a] -> a

thirdc (a:b:c:cs) = c

-- Ch4,#3. Define a function safetail such that it maps the empty set to itself instead of returning an error. 

-- #3a. Using a conditional expression

safetaila :: [a] -> [a]

safetaila a = if null a then [] else tail a

-- #3b. using guarded expressions with | 

safetailb :: [a] -> [a]

safetailb a | null a = [] | otherwise = tail a

-- #3c. Using pattern matching

safetailc :: [a] -> [a]

safetailc [] = []
safetailc xs = tail xs

-- #4. Define || which is or using pattern matching

myor :: Bool -> Bool -> Bool

True `myor` _ = True
_ `myor` True = True
_ `myor` _ = False 

-- #5. Write && using two nested conditionals and no other library functions. 

myanda :: Bool -> Bool -> Bool

myanda x y = if x then if y then True else False else False

-- #6. Do the same with only one conditional

myandb :: Bool -> Bool -> Bool

myandb x y = if x then y else False

-- #7 write a function to multiply three integers using lambda expressions

mult :: Int -> Int -> Int -> Int

mult = \x -> \y -> \z -> x*y*z

-- #8. Write something to check a Luhn Algorithm

luhnDouble :: Int -> Int

luhnDouble x | 2*x <9 = 2*x | otherwise = 2*x - 9

luhn :: Int -> Int -> Int -> Int -> Bool

luhn w x y z = mod (sum [luhnDouble w, x, luhnDouble y, z]) 10 == 0