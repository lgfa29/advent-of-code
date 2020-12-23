import Day22.Data
import Day22.Parser

import System.Environment

play :: [Game] -> Game -> Game
play _ g@(Game _ []) = g
play _ g@(Game [] _) = g
play history g@(Game p1@(x:xs) (y:ys))
  | g `elem` history = Game p1 []
  | shouldRecurse = if winner (play [] recursiveGame) == Player1 then player1Wins else player2Wins
  | comp == GT = player1Wins
  | comp == LT = player2Wins
  where
    shouldRecurse = length xs >= x && length ys >= y
    recursiveGame = Game (take x xs) (take y ys)
    comp = compare x y
    history' = g:history
    player1Wins = play history' (Game (xs++[x,y]) ys)
    player2Wins = play history' (Game xs  (ys++[y,x]))

winner :: Game -> Player
winner (Game p1 _) = if null p1 then Player2 else Player1

score :: [Int] -> Int
score cards = let
    cardsScore = zipWith (*) (reverse cards) [1..]
  in sum cardsScore

part2 :: Game -> Int
part2 g = let
    g' = play [] g
    w = winner g'
  in case w of
    Player1 -> score (player1 g')
    Player2 -> score (player2 g')

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v  -> print (part2 v)
    Left err -> print err
