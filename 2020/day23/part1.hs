import Day23.Parser

import Data.Maybe
import System.Environment
import qualified Data.List as List

play :: [Int] -> [Int]
play (x:xs) = let
    picks = take 3 xs
    xs' = drop 3 xs
    dest = destination (x-1) xs'
    destIdx = fromJust (List.elemIndex dest xs')
  in take (destIdx+1) xs' ++ picks ++ drop (destIdx+1) xs' ++ [x]

destination :: Int -> [Int] -> Int
destination i xs
  | i < minimum xs = maximum xs
  | i `elem` xs = i
  | otherwise = destination (i-1) xs

part1 :: [Int] -> String
part1 xs = let
    xs' = iterate play xs !! 100
    idx1 = fromJust (List.elemIndex 1 xs')
    result = drop (idx1+1) xs' ++ take idx1 xs'
  in
    List.intercalate "" (map show result)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
