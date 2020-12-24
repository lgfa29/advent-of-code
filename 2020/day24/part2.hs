import Day24.Parser
import Day24.Data

import System.Environment
import qualified Data.Map as Map

nextDay :: Floor -> Floor
nextDay f = Map.foldrWithKey (\coord _ acc -> Map.insert coord (next f coord) acc) Map.empty (expandFloor f)

expandFloor :: Floor -> Floor
expandFloor f = let
    boundaries = Map.foldrWithKey (\coord _ acc -> Map.union acc (boundary f coord)) Map.empty f
  in Map.union f boundaries

boundary :: Floor -> (Int, Int) -> Floor
boundary f c = let
    b = Map.fromList (map (\c' -> (c', White)) (adjCoords c))
  in Map.difference b f

next :: Floor -> (Int, Int) -> Color
next f c
  | color == White && numBlack == 2 = Black
  | color == Black && numBlack == 0 || numBlack > 2 = White
  | otherwise = color
  where
    color = Map.findWithDefault White c f
    adjs = adjColors f c
    numBlack = length (filter (==Black) adjs)

workAll :: Floor -> [[Direction]] -> Floor
workAll f [] = f
workAll f (d:ds) = let
    f' = work f (0, 0) d
  in Map.union (workAll f' ds) f'

work :: Floor -> (Int, Int) -> [Direction] -> Floor
work f c [] = flipTile f c
work f c (d:ds) = work f (move c d) ds

flipTile :: Floor -> (Int, Int) -> Floor
flipTile f c = let
    color' = case Map.lookup c f of
      Nothing -> Black
      Just color -> if color == White then Black else White
  in Map.insert c color' f

move :: (Int, Int) ->  Direction -> (Int, Int)
move (x, y) E  = (x+2, y)
move (x, y) SE = (x+1, y-1)
move (x, y) SW = (x-1, y-1)
move (x, y) W  = (x-2, y)
move (x, y) NW = (x-1, y+1)
move (x, y) NE = (x+1, y+1)

adjCoords :: (Int, Int) -> [(Int, Int)]
adjCoords c = map (move c) [E ..]

adjColors :: Floor -> (Int, Int) -> [Color]
adjColors f c = map (\c -> Map.findWithDefault White c f) (adjCoords c)

countBlacks :: Floor -> Int
countBlacks = Map.foldr (\c acc -> if c == Black then acc+1 else acc) 0

part2 :: [[Direction]] -> Int
part2 ds = let
    f = workAll Map.empty ds
    f' = iterate nextDay f !! 100
  in countBlacks f'

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v  -> print (part2 v)
    Left err -> print err
