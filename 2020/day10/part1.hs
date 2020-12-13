import Day10.Parser
import qualified Data.Map as Map

adapters :: [Int] -> Map.Map Int Int
adapters = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

adaptersLeft :: Map.Map Int Int -> Int
adaptersLeft = Map.foldr (+) 0

useAdapter :: Int -> Map.Map Int Int -> Map.Map Int Int
useAdapter x  m
  | Map.findWithDefault 0 x m == 0 = m
  | otherwise = Map.insertWith (-) x 1 m

findAdapter :: Int -> Map.Map Int Int -> [Int]
findAdapter j m = let
    joltages = [j, j+1, j+2, j+3]
  in
    filter (\j -> Map.findWithDefault 0 j m > 0) joltages

dfs :: Map.Map Int Int -> [Int] -> [Int] -> [Int]
dfs m [] branch = if adaptersLeft m == 0 then branch else []
dfs m (x:xs) branch
  | adaptersLeft m == 0 = branch
  | otherwise = dfs m' (findAdapter x m' ++ xs) (branch ++ [x])
  where
    m' = useAdapter x m

diffs :: [Int] -> [Int]
diffs [] = []
diffs [_] = []
diffs (x:y:zs) = (y-x) : diffs (y:zs)

count :: Int -> [Int] -> Int
count x = foldl (\acc i -> if i == x then acc+1 else acc) 0

part1 :: [Int] -> Int
part1 xs = let
    m = adapters xs
    b = dfs m (findAdapter 0 m) []
    device = maximum xs + 3
    d = diffs (0:(b ++ [device]))
  in
    count 1 d * count 3 d

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
