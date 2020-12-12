import Day7.Parser
import Day7.Data
import qualified Data.Map as Map
import qualified Data.Set as Set

generatePairs :: [Rule] -> [(Bag, Bag)]
generatePairs = concatMap (\r -> map (\s -> (bag s, container r)) (contains r))

reducePairs :: [(Bag, Bag)] -> Map.Map Bag [Bag] -> Map.Map Bag [Bag]
reducePairs pairs m = foldl (\ m pair -> Map.insertWith (++) (fst pair) [snd pair] m) m pairs

reverseIndex :: [Rule] -> Map.Map Bag [Bag]
reverseIndex rs = reducePairs (generatePairs rs) Map.empty

findAllContainers :: Map.Map Bag [Bag] -> [Bag] -> Bag -> [Bag]
findAllContainers m acc b = let
    containers = Map.lookup b m
  in
    case containers of
      Nothing -> acc
      Just cs -> acc ++ cs ++ concatMap (findAllContainers m acc) cs

part1 :: [Rule] -> Bag -> Int
part1 rs b = let
    index = reverseIndex rs
    containers = findAllContainers index [] b
  in
    length (Set.fromList containers)

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v (Bag "shiny" "gold"))
    Left err -> print err
