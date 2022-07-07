{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant bracket" #-}
module RomanNaturals where

import Data.List ( elemIndex, sort, sortBy, groupBy, nubBy)
import Text.Read ()

-- defining basic structures -----------

-- defining a new datatype Numeral which can take one of the seven characters assigned, and derives some basic useful datatypes
data Numeral = I | V | X | L | C | D | M deriving (Eq)
-- defining a NumeralStr which is a list of Numeral, much like String is a list of Char
type NumeralStr = [Numeral]
-----------------------------------------

-- defining the Ordering of numerals ---------------------

-- this list is in ascending order, and provides the basis for numeral ordering
numerals :: [Numeral]
numerals = [I, V, X, L, C, D, M]

showdict :: [(Numeral, String)]
showdict = zip numerals ["I", "V", "X", "L", "C", "D", "M"]

-- creating a way to compare two Numeral based on their position in the list above 
instance Ord Numeral where
    compare x y = compare (elemIndex x numerals) (elemIndex y numerals)

-- creating a way to display numerals
instance Show Numeral where
    -- showing an individual numeral is done by taking the first character in showdict that matches the numeral
    show n = head [c | (nu,c) <- showdict, nu == n]

    -- in order to display a list of numerals, much like how String is of type [Char], we display an empty string for an empty list and otherwise just display the numerals in order concated.
    showList [] = (++ "")
    showList (n:ns) = shows n . showList ns

instance Read Numeral where
    readsPrec _ c = [(n,cu) | (n, cu) <- showdict, cu == c]

readnum :: String -> Numeral
readnum c = head [n | (n, cu) <- showdict, cu == c]

readnum1 :: String -> [(Numeral, String)]
readnum1 c = [(n,cu) | (n, cu) <- showdict, cu == c]

readnumstr :: String -> NumeralStr
readnumstr "" = []
readnumstr (n:ns) = readnum [n] : readnumstr ns

-- creating a dictionary pairing Numeral with their respective Int value. Maybe this could be defined recursively? Notice that it is 1 * 5 * 2 * 5 * 2 ....
measures :: [Int]
measures = foldr ((:) . uncurry (*) ) [] pairs where
    pairs = zip (concat (replicate 4 [1,5])) [10^(i `div` 2) | i <- [0..]]

numeraldict :: [(Numeral, Int)]
numeraldict = zip numerals measures

-- list of possible Numeral pairs [i, j] where i < j. ex) IV is 4, IX is 9, VC is 95, etc etc. These are useful because we want to use these when constructing numerals from integers. We could construct 95 as LVL, but thats gross.
subpairs :: [NumeralStr]
subpairs = filter validnum [[i, j] | i <- numerals, j <- numerals, i < j]

subpairdict :: [(NumeralStr, Int)]
subpairdict = filter ((`notElem` measures) . snd) (sortBy sortpairs (zip subpairs (map convertn subpairs)))

-- oxforddict is a dictionary that contains all of the "primes" of building roman numerals: from these blocks, all numerals can be formed. Its worth noting that there are duplicates. ex) L is 50, but LC is also 50
oxforddict :: [(NumeralStr, Int)]
oxforddict = sortBy sortpairs (map (\(x,y) -> ([x],y)) numeraldict ++ subpairdict)

-- sort the pairs of NumeralStr and Int by comparing the integer values instead of the numeral values
sortpairs :: (NumeralStr, Int) -> (NumeralStr, Int) -> Ordering
sortpairs (x, i) (y, j) = compare i j
------------------------------------------------------------

-- basic operations on Numeral and NumeralStr to convert to integers -------------------------

-- converting a single numeral to a single integer. think of a numeral as a Char
numtoint :: Numeral -> Int
numtoint c = head [v | (k, v) <- numeraldict, k == c]

-- converting a NumeralStr to its integer value. As of now, does not check if the passed NumeralStr is valid
convertn :: NumeralStr -> Int
convertn [] = 0
convertn [x] = numtoint x
convertn (x:y:xs) = output + convertn xs where
    output | x < y = numtoint y - numtoint x | otherwise = numtoint y + numtoint x

inttonum :: Int -> Numeral
inttonum i = head [k | (k, v) <- numeraldict, v == i]

converti :: Int -> NumeralStr
converti 0 = []
converti i = fst (oxforddict !! (length l - 1)) ++ converti (i - last l) where
    l = filter (<=i) (map snd oxforddict)

{-
a roman numeral is syntax invalid if two numerals in a row are less than or equal to their succcessor.
ex) IIV is invalid because I and I are both less than V. 
ex) VIVI is invalid because 
    VIV is considered valid, even though XI is better, becacuse it does not break the syntax rule 
    notice that IVV and VIV are both considered valid, both are 9
    VVI is also valid even though X would be much better
-}

-- checking a trio for the rule described above
checktrio :: (Numeral, Numeral, Numeral) -> Bool
checktrio (x,y,z) =  not ((x<=y && y<z) || (x<y && x == z))

-- checking an entire string of numerals 1 trio at a time. Any non-trio is trivally True, surely this can be written more succintly but I just love those recursions
validnum :: NumeralStr -> Bool
validnum [] = True
validnum [_] = True
validnum [_, _] = True
validnum (x:xs) = checktrio (x,y,z) && validnum xs where
    [y, z] = take 2 xs

cleanstr :: NumeralStr -> NumeralStr
cleanstr x = if not (branchfilter x) then converti (convertn x) else x

branchfilter :: NumeralStr -> Bool
branchfilter x = (x == converti (convertn x))

----------------------------

-- defining binary operations on NumeralStr. This is where the fun begins -------------------

-- in general, fconvertslow provides the isomorphism between functions on two natural numbers and functions on two numeral strings.
-- It's tagged as slow because it requires three isomorphic conversions as well as the function operation. 
fconvertslow :: (Int -> Int -> Int) -> (NumeralStr -> NumeralStr -> NumeralStr)
fconvertslow f x y = converti ((convertn x) `f` (convertn y))

-- adding and multiplying by converting to integers and converting back. can be really tedious
addslow :: NumeralStr -> NumeralStr -> NumeralStr
addslow = fconvertslow (+)

multslow :: NumeralStr -> NumeralStr -> NumeralStr
multslow = fconvertslow (*)
-- in order to add two roman numerals together, the new oxford values are placed in the appropriate location, then the string is cleaned up
--add :: NumeralStr -> NumeralStr -> NumeralStr
------

-- Lets try to build some IO stuff

ioconverti :: IO NumeralStr

ioconverti = do
    putStr "Please enter an integer: "
    input <- getLine
    putStr "Here it is! "
    return (converti (read input))

guessgame :: IO Bool

guessgame = do
    let targeti = 55
    let targetn = converti targeti
    print numeraldict
    putStr "Try to guess what my number is! as a roman numeral if you will: "
    input <- getLine
    let answern = readnumstr input
    let answer = (answern == targetn)
    if answer 
        then return answer 
    else do
        putStrLn "Not quite!"
        guessgame