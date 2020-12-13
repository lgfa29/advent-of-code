import Day10.Parser
import qualified Data.Map as Map

adapters :: [Int] -> Map.Map Int Int
adapters = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

adaptersLeft :: Map.Map Int Int -> Int
adaptersLeft = Map.foldr (+) 0

useAdapter :: Map.Map Int Int -> Int -> Map.Map Int Int
useAdapter m x
  | Map.findWithDefault 0 x m == 0 = m
  | otherwise = Map.insertWith (-) x 1 m

findAdapter :: Map.Map Int Int -> Int -> [Int]
findAdapter m x = let
    joltages = [x, x+1, x+2, x+3]
  in
    filter (\j -> Map.findWithDefault 0 j m > 0) joltages

-- Too Slow
-- dfs (== fst (Map.findMax m)) [findAdapter m 0] [(0, m)] []
dfs :: (Int -> Bool) -> [[Int]] -> [(Int, Map.Map Int Int)] -> [[Int]] -> [[Int]]
dfs _ [] _ acc = acc
dfs stop ([]:xs) b acc
  | stop (fst tip) = dfs stop xs (tail b) (cleanBranch:acc)
  | otherwise = dfs stop xs (tail b) acc
  where
    tip = head b
    cleanBranch = map fst b
dfs stop ((a:as):xs) b acc = let
    tip = head b
    state = snd tip
    state' = useAdapter state a
    frontier = findAdapter state' a
  in
    dfs stop (frontier:(as : xs)) ((a, state'):b) acc

-- Too Slow
permsRecusive :: Map.Map Int Int -> Int -> Int
permsRecusive state x = let
    state' = useAdapter state x
    options = findAdapter state' x
  in
    if null options then 1
    else sum (map (permsRecusive state) options)

-- Works
perms :: [Int] -> Int -> Int
perms state x = go state x
  where max = maximum state
        go state n = if n == max then 1 else sum (map (combs Map.!) (findAdapter' state n))
        combs = Map.fromList [(a, go state a) | a <- [0..max]]

findAdapter' :: [Int] -> Int -> [Int]
findAdapter' m x = let
    joltages = [x+1, x+2, x+3]
  in
    filter (`elem` m) joltages

part2 :: [Int] -> Int
part2 xs = perms xs 0

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
