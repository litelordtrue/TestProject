module SuperAutoFunc where
import Data.Bits (Bits(xor))
import Data.Maybe

-- eventually, I want to have Fighter be a class, and have each fighter be an instance of that class? maybe not
data Fighter = Fighter {
    attack :: Int,
    health :: Int
} deriving (Show)

type Team = [Fighter]

pairtomaybe :: (a, a) -> (Maybe a, Maybe a)
pairtomaybe (x, y) = (Just x, Just y)

-- simattack recursively simulates an attack pattern from a to b until one of them dies. It really shouldn't do that. winner should do that, simattack should do a SINGLE attack and then determine if any of them died. 
simattack :: (Maybe Fighter, Maybe Fighter) -> (Maybe Fighter, Maybe Fighter)
-- simmattack should take two fighters
-- have them hit each other and find their new hps. 
-- then if any of them have health below zero, make them dead.
simattack (Just (Fighter a1 h1), Just (Fighter a2 h2)) | h1'>0 && h2'>0 = (Just f1', Just f2') | h1'>0 && h2'<=0 = (Just f1', Nothing) | h1'<=0 && h2'>0 = (Nothing, Just f2') where
    h1' = h1-a2
    f1' = Fighter a1 h1'
    h2' = h2-a1
    f2' = Fighter a2 h2'

simattack _ = (Nothing, Nothing)

-- calcbattle recursively applies simattack until a winner is determined. it returns a tuple ,because we need to know WHICH team to update
calcbattle :: (Maybe Fighter, Maybe Fighter) -> (Maybe Fighter, Maybe Fighter)
calcbattle (Just f1, Just f2) = calcbattle (simattack (Just f1, Just f2))
calcbattle x = x

-- determine the winner! defunct, I assume that pulling the Just Fighter from the return value of calcbattle would be easier
winner :: (Maybe Fighter, Maybe Fighter) -> Maybe Fighter
winner (Just f1, Just f2) = winner (simattack (Just f1, Just f2))
winner (Just f1, Nothing) = Just f1
winner (Nothing, Just f2) = Just f2
winner (Nothing, Nothing) = Nothing

-- some fighters!!
matchup1 :: (Maybe Fighter, Maybe Fighter)
matchup1 = pairtomaybe (head (zip team1 team2))

matchup2 :: (Maybe Fighter, Maybe Fighter)
matchup2 = pairtomaybe (last (zip team1 team2))


-- some teams!
team1 :: Team
team1 = [Fighter 3 4, Fighter 4 6, Fighter 8 6, Fighter 2 7]

team2 :: Team
team2 = [Fighter 100 100]


-- an insane function that exists in my head that i want to make

take2 :: (Team, Team) -> (Fighter, Fighter)
take2 (t1, t2) = (head t1, head t2)

-- take2 -> pairtomaybe -> calcbattle -> updateteams
-- updateteams (team1, team2) (calcbattle (pairtomaybe (take2 (team1, team2)))) returns the two new teams

updateteams :: (Team, Team) -> (Maybe Fighter, Maybe Fighter) -> (Team, Team)
updateteams (t1, t2) (Nothing, Nothing) = (drop 1 t1, drop 1 t2)
updateteams (t1, t2) (Nothing, Just (Fighter a h)) = (drop 1 t1, (Fighter a h) : (drop 1 t2))
updateteams (t1, t2) (Just (Fighter a h), Nothing) = ((Fighter a h) : (drop 1 t1), drop 1 t2) 
updateteams _ _ = ([], [])

-- full calculates the outcome of an entire battle between the two frontline units
full :: (Team, Team) -> (Team, Team)
full (t1, t2) = updateteams (t1, t2) (calcbattle (pairtomaybe (take2 (t1, t2))))

matchover :: (Team, Team) -> Bool
matchover ([], _) = True
matchover (_, []) = True
matchover _ = False

match :: (Team, Team) -> (Team, Team)
match (t1, t2) = until matchover full (t1, t2)

t1won :: (Team, Team) -> Maybe Bool
t1won ([], []) = Nothing
t1won ([], _) = Just False
t1won (_, []) = Just True
t1won _ = Nothing

-- by applying (t1won . match) to a tuple of teams, we are given the outcome of a match. corresponding: [Just True, Nothing, Just False] <-> [team 1 wins, there is a drew, team 2 wins]