module Lib where

data PlayerName = Player1 | Player2 deriving (Show, Eq)

scoreIn ::  PlayerName -> [PlayerName] -> [PlayerName]
scoreIn = (:) 

type Winner = [Char]
type GamePoints = Int
type SetPoints = [GamePoints]
type MatchPoints = [SetPoints]

data Player = Player PlayerName MatchPoints

data Match = Match (Player, Player)

score2 :: Int -> [Char]
score2 0 = "0"
score2 1 = "15"
score2 2 = "30"
score2 3 = "40"

scoreForPlayer :: [PlayerName] -> PlayerName -> String
scoreForPlayer a p = score2 $ length $ filter (== p) a

score :: [PlayerName] -> [Char]
score [] = "0-0"
score a = (scoreForPlayer a Player1) ++ "-" ++ (scoreForPlayer a Player2)
