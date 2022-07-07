module SuperAutoFunc where
import Data.Bits (Bits(xor))

data Fighter = Fighter {
    attack :: Int,
    health :: Int
} | Dead deriving (Show)

simattack :: (Fighter, Fighter) -> (Fighter, Fighter)
simattack (Fighter a h,Fighter a' h') = (Fighter a (h-a'), Fighter a' (h'-a))
simattack _ = (Dead, Dead)

battle :: (Fighter, Fighter) -> Fighter
battle (Dead, x) = x
battle (x, Dead) = x
battle (Fighter a h, Fighter a' h') = Fighter a h

fighters :: [Fighter]
fighters = [Fighter 1 2, Fighter 3 4]

matchup :: (Fighter, Fighter)
matchup = (Fighter  1 2, Fighter 3 4)