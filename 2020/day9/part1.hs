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

part1 :: Int -> [Int] -> Int
part1 = findInvalid

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 25 v)
    Left err -> print err
