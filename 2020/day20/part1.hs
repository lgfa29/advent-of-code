import Day20.Data
import Day20.Parser

import System.Environment

isCorner :: [Tile] -> Tile -> Bool
isCorner tiles t = length (adjacents tiles t) == 2

adjacents :: [Tile] -> Tile -> [Tile]
adjacents tiles t = filter (\t' -> adjacent t t' && t /= t') tiles

adjacent :: Tile -> Tile -> Bool
adjacent t1 t2 = let
    v1 = variations t1
    v2 = variations t2
    maxV1 = length v1 - 1
    maxV2 = length v2 - 1
    combinations = [(v1!!i, v2!!j) | i <- [0..maxV1], j <- [0..maxV2]]
  in
    any (uncurry matchingBorders) combinations

borders :: Tile -> [String]
borders (Tile _ t) = let
    top = head t
    bottom = last t
    max = length t - 1
    left = [head (t!!y) | y <- [0..max]]
    right = [t!!y!!max | y <- [0..max]]
  in
    [top, right, left, bottom]

matchingBorders :: Tile -> Tile -> Bool
matchingBorders t1 t2 = let
    b1 = borders t1
    b2 = reverse (borders t2)
    zipped = zip b1 b2
  in
    any (uncurry (==)) zipped

variations :: Tile -> [Tile]
variations t = let
    r90 = rotate t
    r180 = rotate r90
    r270 = rotate r180
    rotations = [t, r90, r180, r270]
  in
    rotations ++ map flipX rotations ++ map flipY rotations

rotate :: Tile -> Tile
rotate (Tile tId t) = let
    max = length t - 1
    rotated = [[t!!(max-y)!!x | y <- [0..max]] | x <- [0..max]]
  in
    Tile tId rotated

flipX :: Tile -> Tile
flipX (Tile tId t) = let
    max = length t - 1
    flipped = [[t!!(max-y)!!x | x <- [0..max]] | y <- [0..max]]
  in
    Tile tId flipped

flipY :: Tile -> Tile
flipY (Tile tId t) = let
    max = length t - 1
    flipped = [[t!!y!!(max-x) | x <- [0..max]] | y <- [0..max]]
  in
    Tile tId flipped

part1 :: [Tile] -> Int
part1 tiles = let
    corners = filter (isCorner tiles) tiles
    cornersIds = map tileId corners
  in
    product cornersIds

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
