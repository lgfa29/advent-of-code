import qualified Data.Map as Map
import qualified Data.List as List
import           System.Environment
import           Day16.Data
import           Day16.Parser

fieldsOrder :: [[String]] -> [[String]] -> [[String]]
fieldsOrder = foldl pickFields

pickFields :: [[String]] -> [String] -> [[String]]
pickFields picked = foldl (\acc x -> pick picked x++acc) []

pick :: [[String]] -> String -> [[String]]
pick [] s = [[s]]
pick picked s = map (++[s]) (filter (s `notElem`) picked)

fields :: Rules -> [Int] -> [String]
fields rules xs = let
    fieldsResults = Map.map (`valid` xs) rules
    validFields = Map.filter (== True) fieldsResults
  in
    Map.keys validFields

valid :: Rule -> [Int] -> Bool
valid _ []         = True
valid rule (x:xs) = validFieldRule rule x && valid rule xs

validField :: Rules -> Int -> Bool
validField rules x = foldr (\r acc -> acc || validFieldRule r x) False rules

validFieldRule :: Rule -> Int -> Bool
validFieldRule [] _ = False
validFieldRule ((min,max):rs) x = let
    valid = x >= min && x <= max
  in
    valid || validFieldRule rs x

validTicket :: Rules -> Ticket -> Bool
validTicket _ []         = True
validTicket rules (x:xs) = validField rules x && validTicket rules xs

part2 :: Input -> Int
part2 input = let
    filtered = filter (validTicket (rules input)) (nearbyTickets input)
    transposed = List.transpose filtered
    possibleFields = map (fields (rules input)) transposed
    order = head (fieldsOrder [] possibleFields)
    labeledTicket = zip order (myTicket input)
    departureFields = filter (List.isPrefixOf "departure" . fst) labeledTicket
  in
    foldl (\acc t -> acc * snd t) 1 departureFields

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents

  case input of
    Right v  -> print (part2 v)
    Left err -> print err
