module Lib where

data Player = Player1 String | Player2 String deriving (Eq)
instance Show Player where
    show (Player1 name) = name 
    show (Player2 name) = name 

isPlayer1 :: Player -> Bool
isPlayer1 (Player1 _) = True
isPlayer1 _ = False

isPlayer2 :: Player -> Bool
isPlayer2 = not . isPlayer1

scoreIn ::  Player -> [Player] -> [Player]
scoreIn = (:) 

data GamePoints = Love | Fifteen | Thirty | Forty deriving (Eq, Ord)
instance Show GamePoints where
    show Love = "Love" 
    show Fifteen = "15" 
    show Thirty = "30" 
    show Forty = "40" 

data Game = Game GamePoints GamePoints | Advantage Player | Deuce | Win Player
instance Show Game where
    show (Win p) = (show p) ++ " wins"
    show Deuce = "Deuce"
    show (Advantage p) = "Advantage " ++ show p
    show (Game p1 p2) = show p1 ++ "-" ++ show p2

data Set = Set Int Int
instance Show Set where
    show (Set a b) = (show a) ++ "-" ++ (show b)
data Match = Match Set Game
instance Show Match where
    show (Match set game) = (show set) ++ " " ++ (show game)

increasePoints :: GamePoints -> GamePoints
increasePoints Love = Fifteen
increasePoints Fifteen = Thirty
increasePoints _ = Forty

scoreForPlayer :: Match -> Player -> Match
scoreForPlayer (Match set Deuce) p = Match set (Advantage p)
scoreForPlayer (Match set (Advantage p)) o | p == o = Match set (Win p)
                                           | otherwise = Match set Deuce
scoreForPlayer (Match set (Game Forty Forty)) p = Match set (Advantage p)
scoreForPlayer (Match set (Game score1 score2)) (Player1 _) = Match set (Game (increasePoints score1) score2)
scoreForPlayer (Match set (Game score1 score2)) (Player2 _) = Match set (Game score1 (increasePoints score2))

score :: [Player] -> Match
score ps = foldl scoreForPlayer (Match (Set 0 0) (Game Love Love)) ps

printScore :: [Player] -> String
printScore p = (show $ score p)

