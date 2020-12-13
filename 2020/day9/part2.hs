import Day9.Parser

hasComplement :: Int -> Int -> [Int] -> Bool
hasComplement x comp xs = comp - x `elem` xs

valid :: Int -> [Int] -> Bool
valid n xs = let
    preamble = take n xs
    x = xs !! n
    complements = filter (\i -> hasComplement i x preamble) preamble
  in
    not (null complements)

findInvalid :: Int -> [Int] -> Int
findInvalid n xs
  | valid n xs = findInvalid n (tail xs)
  | otherwise = xs !! n

findSet :: Int -> Int -> [Int] -> [Int]
findSet _ _ [] = []
findSet _ _ [_] = []
findSet s i xs
  | i >= length xs || sum search > s = findSet s 0 (tail xs)
  | sum search == s = search
  | otherwise = findSet s (i + 1) xs
  where
    search = take i xs

minMax :: [Int] -> (Int, Int)
minMax xs = (minimum xs, maximum xs)

part2 :: Int -> [Int] -> Int
part2 n xs = let
    invalid = findInvalid n xs
    range = minMax (findSet invalid 0 xs)
  in
  uncurry (+) range

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 25 v)
    Left err -> print err
