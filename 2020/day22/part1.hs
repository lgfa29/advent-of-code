import Day22.Data
import Day22.Parser

import System.Environment

play :: Game -> Game
play g@(Game _ []) = g
play g@(Game [] _) = g
play (Game (x:xs) (y:ys))
  | comp == GT = play (Game (xs++[x,y]) ys)
  | comp == LT = play (Game xs  (ys++[y,x]))
  where
    comp = compare x y

winner :: Game -> Player
winner (Game p1 _) = if null p1 then Player2 else Player1

score :: [Int] -> Int
score cards = let
    cardsScore = zipWith (*) (reverse cards) [1..]
  in sum cardsScore

part1 :: Game -> Int
part1 g = let
    g' = play g
    w = winner g'
  in case w of
    Player1 -> score (player1 g')
    Player2 -> score (player2 g')

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
