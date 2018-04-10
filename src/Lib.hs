module Lib where

import Data.Maybe

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

gameHasWinner :: Game -> Maybe Player
gameHasWinner (Win p) = Just p
gameHasWinner _ = Nothing

data Set = Set Int Int
instance Show Set where
    show (Set a b) = (show a) ++ "-" ++ (show b)

printSets :: [Set] -> String
printSets sets = unwords $ map show (reverse sets)

data Match = Match [Set] Game | MatchWin [Set] Player
instance Show Match where
    show (Match sets game) = (printSets sets) ++ " " ++ (show game)
    show (MatchWin sets player) = (printSets sets) ++ " " ++ (show player) ++ " wins the game !"

increasePoints :: GamePoints -> GamePoints
increasePoints Love = Fifteen
increasePoints Fifteen = Thirty
increasePoints _ = Forty

scoreGame :: Game -> Player -> Game
scoreGame Deuce p = Advantage p
scoreGame (Advantage p) o | p == o = Win p
                          | otherwise = Deuce
scoreGame (Game Forty Forty) p = Advantage p
scoreGame (Game Forty _) p@(Player1 _) = Win p
scoreGame (Game _ Forty) p@(Player2 _) = Win p
scoreGame (Game score1 score2) (Player1 _) = Game (increasePoints score1) score2
scoreGame (Game score1 score2) (Player2 _) = Game score1 (increasePoints score2)

increaseSet :: Set -> Player -> Set
increaseSet (Set s1 s2) (Player1 _) = (Set (s1+1) s2)
increaseSet (Set s1 s2) (Player2 _) = (Set s1 (s2+1))

doesPlayerWinsSet :: Set -> Player -> Bool
doesPlayerWinsSet (Set s1 s2) p =
    (s1 >= 5 && s1 > s2 && isPlayer1 p) || (s2 >= 5 && s2 > s1 && isPlayer2 p)

increaseSets :: [Set] -> Player -> [Set]
increaseSets [] _ = []
increaseSets (set:sets) p 
    | doesPlayerWinsSet set p = (Set 0 0) : ((increaseSet set p) : sets)
    | otherwise = (increaseSet set p) : sets

hasPlayerWonSet :: Player -> Set -> Bool
hasPlayerWonSet (Player1 _) (Set s1 s2) = s1 > s2 
hasPlayerWonSet (Player2 _) (Set s1 s2) = s2 > s1 

winSetsCount :: [Set] -> Player -> Int
winSetsCount sets p = 
    length $ filter (hasPlayerWonSet p) sets

doesPlayerWin :: Match -> Player -> Bool
-- You need to win 3 sets to win the match
doesPlayerWin (Match (current:sets) _) p = (winSetsCount sets p) >= 2 && doesPlayerWinsSet current p 
doesPlayerWin _ _ = False

scoreForPlayer :: Match -> Player -> Match 
scoreForPlayer match@(MatchWin _ _) _ = match
scoreForPlayer match@(Match sets game) player =
    if doesPlayerWin match player
        then MatchWin (tail $ increaseSets sets player) player
        else let newGame = scoreGame game player
                 in if isNothing $ gameHasWinner newGame
                    then Match sets newGame
                    else Match (increaseSets sets player) (Game Love Love)

score :: [Player] -> Match
score ps = foldl scoreForPlayer (Match [(Set 0 0)] (Game Love Love)) ps

printScore :: [Player] -> String
printScore p = (show $ score p)