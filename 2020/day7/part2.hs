import Day7.Parser
import Day7.Data
import qualified Data.Map as Map

index :: [Rule] -> Map.Map Bag [Statement]
index = foldl (\m rule -> Map.insert (container rule) (contains rule) m) Map.empty

volume :: Map.Map Bag [Statement] -> Bag -> Int
volume m b = let
    stmts = Map.lookup b m
  in
    case stmts of
      Nothing -> 0
      Just stmts' -> sum (map (\s -> num s + num s * volume m (bag s)) stmts')

part2 input = let idx = index input in volume idx

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v (Bag "shiny" "gold"))
    Left err -> print err
