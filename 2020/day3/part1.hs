import Text.ParserCombinators.Parsec

tree = '#'

inputParser = endBy line (char '\n')
line = many (oneOf ".#")

parseInput :: String -> Either ParseError [[Char]]
parseInput = parse inputParser "(input)"

part1 :: [[Char]] -> (Int, Int) -> Int -> Int
part1 map coord trees
  | y >= length map = trees
  | otherwise = let
                  nextX = (x + 3) `mod` length (head map)
                  nextY = y + 1
                in
                  if map !! y !! x == tree
                  then part1 map (nextX, nextY) (trees + 1)
                  else part1 map (nextX, nextY) trees
  where
    x = fst coord
    y = snd coord

main = do
  contents <- readFile "input.txt"
  let input = parseInput contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v (0, 0) 0)
    Left err -> print err
