import Day11.Parser

empty = 'L'
occupied = '#'

step :: [String] -> [String]
step s = [[nextState s (x, y) | x <- [0..length (s!!y)-1]] | y <- [0..length s-1]]

runTillStable :: [String] -> [String]
runTillStable s = let s' = step s in
  if s == s' then s' else runTillStable s'

nextState :: [String] -> (Int, Int) -> Char
nextState s coord@(x, y)
  | c == empty && allEmpty s coord = '#'
  | c == occupied && fourOrMoreOccupied s coord = 'L'
  | otherwise = c
  where
    c = s!!y!!x

allEmpty :: [String] -> (Int, Int) -> Bool
allEmpty s coord = '#' `notElem` adjacents s coord

fourOrMoreOccupied :: [String] -> (Int, Int) -> Bool
fourOrMoreOccupied s coord = length (filter (== '#') (adjacents s coord)) >= 4

adjacents :: [String] -> (Int, Int) -> String
adjacents s (x, y) = map (\coord -> s!!snd coord!!fst coord) (adjacentCoords s (x, y))

adjacentCoords :: [String] -> (Int, Int) -> [(Int, Int)]
adjacentCoords s (x, y) = let
    maxX = length (head s)
    maxY = length s
  in
    filter (\c -> c /= (x, y) &&
                  fst c >= 0 && fst c < maxX &&
                  snd c >= 0 && snd c < maxY
           )
           [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1]]

countAll :: Char -> [String] -> Int
countAll c s = sum (map (length . filter (c==)) s)

part1 :: [String] -> Int
part1 s = countAll '#' (runTillStable s)

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
