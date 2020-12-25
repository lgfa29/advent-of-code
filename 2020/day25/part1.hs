import Day25.Parser

import System.Environment

findLoopSize :: Int -> Int -> Int
findLoopSize answer sn = findLoopSize' answer sn 1 1

findLoopSize' :: Int -> Int -> Int -> Int -> Int
findLoopSize' answer sn v loops
  | v' == answer = loops
  | otherwise = findLoopSize' answer sn v' (loops+1)
  where v' = transform sn v

loop :: Int -> Int -> Int -> Int
loop loops sn v = iterate (transform sn) v !! loops

transform :: Int -> Int -> Int
transform sn v = v * sn `rem` 20201227

part1 :: (Int, Int) -> Int
part1 (k1, k2) = let
    ls = findLoopSize (min k1 k2) 7
  in loop ls (max k1 k2) 1

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
