parseInput :: String -> [Int]
parseInput input = [read l :: Int | l <- lines input]

complement :: Int  -> Int
complement x = 2020 - x

hasComplement :: Int -> [Int] -> Bool
hasComplement x xs = complement x `elem` xs

findComplements :: [Int] -> [Int]
findComplements [] = []
findComplements [_] = []
findComplements xs = let first = head xs in
                        if hasComplement first (tail xs)
                        then [first, complement first]
                        else findComplements (tail xs)

part1 :: [Int] -> Int
part1 xs = product (findComplements xs)

main = do
  contents <- readFile "input.txt"
  let input = parseInput contents
  putStr "Part 1: "
  print (part1 input)
