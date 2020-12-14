import Day11.Parser

data Direction = N | NE | E | SE | S | SW | W | NW deriving (Enum, Show, Eq)

empty = 'L'
occupied = '#'
floor = '.'

step :: [String] -> [String]
step s = [[nextState s (x, y) | x <- [0..length (s!!y)-1]] | y <- [0..length s-1]]

runTillStable :: [String] -> [String]
runTillStable s = let s' = step s in
  if s == s' then s' else runTillStable s'

nextState :: [String] -> (Int, Int) -> Char
nextState s coord@(x, y)
  | c == empty && allEmpty s coord = '#'
  | c == occupied && fiveOrMoreOccupied s coord = 'L'
  | otherwise = c
  where
    c = s!!y!!x

allEmpty :: [String] -> (Int, Int) -> Bool
allEmpty s coord = '#' `notElem` adjacents s coord

fiveOrMoreOccupied :: [String] -> (Int, Int) -> Bool
fiveOrMoreOccupied s coord = length (filter (== '#') (adjacents s coord)) >= 5

adjacents :: [String] -> (Int, Int) -> String
adjacents s coord = map (\d -> rayTrace s d coord) [N, S, E, W, NE, SE, NW, SW]

rayTrace :: [String] -> Direction -> (Int, Int) -> Char
rayTrace s d coord
  | x' < 0 || x' >= length (head s) || y' < 0 || y' >= length s = Main.floor
  | c == empty || c == occupied = c
  | otherwise = rayTrace s d coord'
  where
    coord' = moveDirection d coord
    x' = fst coord'
    y' = snd coord'
    c = s!!y'!!x'

moveDirection :: Direction -> (Int, Int) -> (Int, Int)
moveDirection d (x, y) = case d of
                           N  -> (x, y-1)
                           S  -> (x, y+1)
                           E  -> (x+1, y)
                           W  -> (x-1, y)
                           NE -> (x+1, y-1)
                           SE -> (x+1, y+1)
                           SW -> (x-1, y+1)
                           NW -> (x-1, y-1)

countAll :: Char -> [String] -> Int
countAll c s = sum (map (length . filter (c==)) s)

part2 :: [String] -> Int
part2 s = countAll '#' (runTillStable s)

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
