module SuperAutoFunc where
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

shop1 :: Team
shop1 = [Fighter 1 2, Fighter 1 3, Fighter 2 4, Fighter 5 5, Fighter 1 3]

takejust :: (Maybe a, Maybe a) -> Maybe a
takejust (Nothing, Just x) = Just x
takejust (Just x, Nothing) = Just x
takejust _ = Nothing 

-- have the front two units duke it out, and return the team that is left.
updateteams :: (Team, Team) -> (Team, Team)
updateteams (t1, t2) | isNothing m1 && isNothing m2 = (t1', t2') | isNothing m1 && isJust m2 = (t1', f : t2') | isJust m1 && isNothing m2 = (f : t1', t2') where
    f1 = head t1
    f2 = head t2
    (m1, m2) = (calcbattle . pairtomaybe) (f1, f2)
    f = (fromJust . takejust) (m1, m2)
    t1' = drop 1 t1
    t2' = drop 1 t2
updateteams _ = ([], [])

-- this predicate decides when a match is over via asking if there exists an empty list as either team. If so, the match is over, otherwise, play on!
matchover :: (Team, Team) -> Bool
matchover ([], _) = True
matchover (_, []) = True
matchover _ = False

-- have a match until the match is over, return whats left
match :: (Team, Team) -> (Team, Team)
match (t1, t2) = until matchover updateteams (t1, t2)

-- take the outcome of a battle and determine who won
t1won :: (Team, Team) -> Maybe Bool
t1won ([], []) = Nothing
t1won ([], _) = Just False
t1won (_, []) = Just True
t1won _ = Nothing

-- by applying (t1won . match) to a tuple of teams, we are given the outcome of a match. corresponding: [Just True, Nothing, Just False] <-> [team 1 wins, there is a drew, team 2 wins]

showfighter :: Fighter -> String
showfighter (Fighter a h) = show a ++ " " ++ show h

shop :: Team -> IO Team
shop team = do
    putshop shop1 3 team
    return [Fighter 1 2]
-- lets just say that units cost 1 gold for now. 
putshop :: Team -> Int -> Team -> IO ()
putshop shop gold team = do
    putStr "Shop: "
    putStr (show shop)
    putStr "      "
    putStr "Gold: "
    print gold
    putStrLn ""
    putStr "Team: "
    print team

purchaseunit :: Team -> Team -> IO Team
purchaseunit shop team = do
    putStrLn ("Pick a unit to purchase:" ++ show [1..(length shop)])
    n <- getint
    return ((shop !! n) : team)

getint :: IO Int
getint = do
    n <- getLine
    putStr "Your new team: "
    return (read n :: Int)

shopphase :: Team -> Int -> Team -> IO Team
shopphase _ 0 team = return team
shopphase shop gold team = do
    putshop shop gold team
    let team' = purchaseunit shop team
    team'
