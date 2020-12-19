import           System.Environment
import           Day16.Data
import           Day16.Parser

invalidFields :: Rules -> Ticket -> [Int]
invalidFields _ [] = []
invalidFields rules (x:xs) = if validField rules x
                             then invalidFields rules xs
                             else x:invalidFields rules xs

validField :: Rules -> Int -> Bool
validField rules x = foldr (\r acc -> acc || validFieldRule r x) False rules

validFieldRule :: Rule -> Int -> Bool
validFieldRule [] _ = False
validFieldRule ((min,max):rs) x = let
    valid = x >= min && x <= max
  in
    valid || validFieldRule rs x

part1 :: Input -> Int
part1 (Input rules _ tickets) = let
    invalid = foldl (\acc t -> acc ++ invalidFields rules t) [] tickets
  in
    sum invalid

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
