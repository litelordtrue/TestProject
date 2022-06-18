module Ch1 where

-- Ch1,#3: writing a product function via recursion. product takes an input of a list of variables with type Num, and outputs a Num
-- prod :: Num a => [a] -> a

prod [] = 1 -- the product of the empty set must be 1, because 1 is the identity
prod (n:ns) = n * product ns -- product of a nonempty set: split the  set into the first element n and the other elements ns.  the product of that entire set is equal to n times the product of ns

-- Ch1,#4: writing a reverse qsort such that we end up with a list of numbers in reverse order
rsort :: Ord a => [a] -> [a]
rsort [] = [] -- sorting an empty set results in an empty set
rsort (x:xs) = rsort larger ++ [x] ++ rsort smaller -- ++ is the append operator
    where -- where defines some local things 
        larger = [a | a <- xs, a >= x] -- larger is the  set of all such that a is in xs and a is greater than or equal to x
        smaller = [b | b <- xs, b < x]

--Ch1,#5: What if we removed the equality from qsort? What would happen
-- my hypothesis is that any duplicates will be removed.  this hypothesis was correct

sqsort :: Ord a => [a] -> [a]
sqsort [] = []
sqsort (x:xs) = sqsort smaller ++ [x] ++ sqsort larger
    where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]