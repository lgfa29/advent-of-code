import System.Environment
import qualified Data.Map as Map

import Day15.Parser
import Day15.Data

play :: Game -> [Int] -> [Int]
play g [] = let g' = speakNumber g in spoken g':play g' []
play g (x:xs) = let g' = readNumber g x in spoken g':play g' xs

readNumber :: Game -> Int -> Game
readNumber (Game m _ r) n = let
    m' = Map.insert n [r] m
  in
    Game m' n (r+1)

speakNumber :: Game -> Game
speakNumber (Game m prev r) = let
    prevRounds = Map.findWithDefault [] prev m
    n = if length prevRounds < 2
        then 0
        else head prevRounds - prevRounds!!1
    nRounds = Map.findWithDefault [] n m
    nRounds' = take 2 (r:nRounds)
    m' = Map.insert n nRounds' m
  in
    Game m' n (r+1)

part1 :: [Int] -> Int
part1 xs = let
    g = Game Map.empty 0 0
    results = play g xs
  in
    last (take 2020 results)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
