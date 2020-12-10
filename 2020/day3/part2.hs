import Text.ParserCombinators.Parsec

tree = '#'

inputParser = endBy line (char '\n')
line = many (oneOf ".#")

parseInput :: String -> Either ParseError [[Char]]
parseInput = parse inputParser "(input)"

slope :: [[Char]] -> (Int, Int) -> (Int, Int) -> Int -> Int
slope map coord v trees
  | y >= length map = trees
  | otherwise = let
                  nextX = (x + vx) `mod` length (head map)
                  nextY = y + vy
                in
                  if map !! y !! x == tree
                  then slope map (nextX, nextY) v (trees + 1)
                  else slope map (nextX, nextY) v trees
  where
    x = fst coord
    y = snd coord
    vx = fst v
    vy = snd v

slope1 map coord = slope map coord (1, 1)
slope2 map coord = slope map coord (3, 1)
slope3 map coord = slope map coord (5, 1)
slope4 map coord = slope map coord (7, 1)
slope5 map coord = slope map coord (1, 2)
slopes = [slope1, slope2, slope3, slope4, slope5]

main = do
  contents <- readFile "input.txt"
  let input = parseInput contents
  putStr "Part 2: "
  case input of
    Right v -> print (product [s v (0, 0) 0 | s <- slopes])
    Left err -> print err
