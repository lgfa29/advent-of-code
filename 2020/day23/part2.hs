import Day23.Data
import Day23.Parser

import System.Environment
import qualified Data.IntMap as IntMap
import qualified Data.Array as Array

play :: Game -> Game
play (Game x cups) = let
    pick1 = cups IntMap.! x
    pick2 = cups IntMap.! pick1
    pick3 = cups IntMap.! pick2
    picks = [pick1, pick2, pick3]
    dest = destination (length cups) (x-1) picks
    changes = IntMap.fromList [
        (x, cups IntMap.! pick3),
        (dest, pick1),
        (pick3, cups IntMap.! dest)
      ]
    cups' = IntMap.union changes cups
  in
    Game (cups' IntMap.! x) cups'

destination :: Int -> Int -> [Int] -> Int
destination len 0 picked = maxRemaining len picked
destination len i picked
  | i `elem` picked = destination len (i-1) picked
  | otherwise = i

maxRemaining :: Int -> [Int] -> Int
maxRemaining max picked
  | max `elem` picked = maxRemaining (max-1) picked
  | otherwise = max

fill :: Int -> [Int] -> [Int]
fill n xs = xs ++ [maximum xs+1..n]

makeGame :: [Int] -> Game
makeGame xs = let
    a = Array.listArray (0, length xs-1) xs
    m = IntMap.fromList [
          (current, next) |
          i <- [0..length a - 1],
          let current = a Array.! i,
          let next = a Array.! ((i+1) `mod` length a)
        ]
  in Game (head xs) m

part2 :: [Int] -> Int
part2 xs = let
    g = makeGame (fill 1000000 xs)
    g' = iterate play g !! 10000000
    after1 = cups g' IntMap.! 1
  in after1 * cups g' IntMap.! after1

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v  -> print (part2 v)
    Left err -> print err
