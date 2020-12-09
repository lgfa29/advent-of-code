parseInput :: String -> [Int]
parseInput input = [read l :: Int | l <- lines input]

complement :: Int -> Int -> Int
complement x comp = comp - x

hasComplement :: Int -> Int -> [Int] -> Bool
hasComplement x comp xs = complement x comp `elem` xs

findComplements :: [Int] -> Int -> Int -> [Int]
findComplements [] _ _ = []
findComplements [_] _ _ = []
findComplements xs 2 comp = let first = head xs in
                              if hasComplement first comp (tail xs)
                              then [first, complement first comp]
                              else findComplements (tail xs) 2 comp
findComplements xs n comp = let
                              first = head xs
                              subComponents = findComplements (tail xs) (n-1) (complement first comp)
                            in
                              if length subComponents == n-1
                              then first : subComponents
                              else findComplements (tail xs) n comp

part1 :: [Int] -> Int
part1 xs = product (findComplements xs 2 2020)

part2 :: [Int] -> Int
part2 xs = product (findComplements xs 3 2020)

main = do
  contents <- readFile "input.txt"
  let input = parseInput contents
  putStr "Part 1: "
  print (part1 input)
  putStr "Part 2: "
  print (part2 input)
