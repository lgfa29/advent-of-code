import Day13.Parser

import qualified Data.List as List

departAfter :: Int -> Int -> Int
departAfter ts id = head (take 1 [x*id | x <- [0..], x*id >= ts])

part1 :: (Int, [Int]) -> Int
part1 (ts, ids) = let
    times = map (\id -> (id, departAfter ts id)) (filter (/= -1) ids)
    nearest = List.minimumBy (\a b -> compare (snd a) (snd b)) times
    busId = fst nearest
    depTime = snd nearest
  in
    busId * (depTime - ts)

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
