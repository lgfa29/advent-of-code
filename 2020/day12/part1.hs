import Data.Maybe
import qualified Data.List as List

import Day12.Parser
import Day12.Data

directions = [N, E, S, W]

go :: [Instruction] -> Ship -> Ship
go [] s = s
go (i:is) s = go is (move i s)

move :: Instruction -> Ship -> Ship
move i s = case action i of
             N -> applyN i s
             S -> applyS i s
             E -> applyE i s
             W -> applyW i s
             L -> applyL i s
             R -> applyR i s
             F -> applyF i s

applyN :: Instruction -> Ship -> Ship
applyN i s = Ship (east s) (north s + value i) (direction s)

applyS :: Instruction -> Ship -> Ship
applyS i s = Ship (east s) (north s - value i) (direction s)

applyE :: Instruction -> Ship -> Ship
applyE i s = Ship (east s + value i) (north s) (direction s)

applyW :: Instruction -> Ship -> Ship
applyW i s = Ship (east s - value i) (north s) (direction s)

applyF :: Instruction -> Ship -> Ship
applyF i s = case direction s of
               N -> applyN i s
               S -> applyS i s
               E -> applyE i s
               W -> applyW i s

applyL :: Instruction -> Ship -> Ship
applyL i = rotate (-(value i))

applyR :: Instruction -> Ship -> Ship
applyR i = rotate (value i)

rotate :: Int -> Ship -> Ship
rotate angle s = let
    i = fromJust (List.elemIndex (direction s) directions)
    d' = directions !! ((i + (angle `div` 90)) `mod` length directions)
  in
    Ship (east s) (north s) d'

manhattanDist :: Ship -> Ship -> Int
manhattanDist s s' = abs (east s' - east s) + abs (north s' - north s)

part1 :: [Instruction] -> Int
part1 is = let s = Ship 0 0 E in  manhattanDist s (go is s)

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
