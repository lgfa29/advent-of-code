{-# LANGUAGE BangPatterns #-}

import System.Environment
import qualified Data.Map as Map

import Day15.Parser
import Day15.Data

playTill :: Int -> Game -> Game
playTill n g
  | Day15.Data.round g == n = g
  | otherwise = playTill n (speakNumber g)

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

part2 :: [Int] -> Int
part2 xs = let
    g = Game Map.empty 0 0
    g' = foldl readNumber g xs
    g'' = playTill 30000000 g'
  in
    spoken g''

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
