import Day24.Parser
import Day24.Data

import System.Environment
import qualified Data.Map as Map

workAll :: Floor -> [[Direction]] -> Floor
workAll f [] = f
workAll f (d:ds) = let
    f' = work f (0, 0) d
  in Map.union (workAll f' ds) f'

work :: Floor -> (Int, Int) -> [Direction] -> Floor
work f c [] = flipTile f c
work f c (d:ds) = work f (move d c) ds

flipTile :: Floor -> (Int, Int) -> Floor
flipTile f c = let
    color' = case Map.lookup c f of
      Nothing -> Black
      Just color -> if color == White then Black else White
  in Map.insert c color' f

move :: Direction -> (Int, Int) -> (Int, Int)
move E (x, y) = (x+2, y)
move SE (x, y) = (x+1, y-1)
move SW (x, y) = (x-1, y-1)
move W (x, y) = (x-2, y)
move NW (x, y) = (x-1, y+1)
move NE (x, y) = (x+1, y+1)

countBlacks :: Floor -> Int
countBlacks = Map.foldr (\c acc -> if c == Black then acc+1 else acc) 0

part1 :: [[Direction]] -> Int
part1 ds = let
    final = workAll Map.empty ds
  in countBlacks final

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
