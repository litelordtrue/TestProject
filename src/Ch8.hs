module Ch8 where

-- Ch8, #1. Build a mult function on the naturals built in Ch8  
data Nat = Zero | Succ Nat
        deriving (Eq,Show)

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)

multNat :: Nat -> Nat -> Nat
multNat Zero n = Zero
multNat (Succ Zero) n = n
multNat (Succ m) n = multNat m n `addNat` n

{-
--Ch8, #3. a tree s.t. only leaves have values. 
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Eq, Show, Ord)

countleaves :: Tree a -> Int
countleaves (Leaf a) = 1
countleaves (Node l r) = countleaves l + countleaves r

balanced :: Tree a -> Bool
balanced (Leaf a) = True
balanced (Node l r) = abs(countleaves l - countleaves r) <= 1 && balanced l && balanced r

t :: Tree Int
t = Node (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) (Leaf 5)

t' :: Tree Int
t' = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

--Ch8, #4. Define a function balance that converts a non-empty list into a balanced tree. 
splitlist :: [a] -> ([a], [a])
splitlist xs = splitAt l xs where l = length xs `div` 2

balance :: [a] -> Tree a
balance as | length as == 1 = Leaf (head as) | otherwise = Node (balance l) (balance r) where 
                                                l = fst (splitlist as)
                                                r = snd (splitlist as)

-}

-- inverting a binary tree. So easy!
data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

t :: Tree Int
t = Node (Node (Leaf 4) 3 (Leaf 5)) 1 (Leaf 2)

invert :: Tree a -> Tree a
invert (Leaf a) = Leaf a
invert (Node l x r) = Node (invert r) x (invert l)