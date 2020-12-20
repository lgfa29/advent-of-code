import Day19.Parser
import Day19.Data

import Data.Maybe
import System.Environment
import qualified Data.Map as Map

match :: Rules -> Int -> String -> Bool
match rules i s = let
    rule = rules Map.! i
    results = apply rules rule s
  in
    case results of
      Nothing -> False
      Just results ->  "" `elem` results

apply :: Rules -> Rule -> String -> Maybe [String]
apply rules c@(SingleChar _) s = applySingleChar rules c s
apply rules r@(SubRule _) s = applySubRule rules r s

applyAll :: Rules -> [Int] -> String -> Maybe [String]
applyAll _ [] s = Just [s]
applyAll rules (x:xs) s = let
    r = rules Map.! x
    results = apply rules r s
  in
    case results of
      Nothing -> Nothing
      Just results -> let
          results' = mapMaybe (applyAll rules xs) results
        in Just (concat results')

updateRules :: Rules -> Rules
updateRules rules = let
    newRules = Map.fromList [
        (8, SubRule [[42], [42, 8]]),
        (11, SubRule [[42, 31], [42, 11, 31]])
      ]
  in
    Map.union newRules rules

applySingleChar :: Rules -> Rule -> String -> Maybe [String]
applySingleChar _ (SingleChar _) [] = Nothing
applySingleChar _ (SingleChar c) (s:ss) = if s == c then Just [ss] else Nothing

applySubRule :: Rules -> Rule -> String -> Maybe [String]
applySubRule _ (SubRule []) [] = Just [""]
applySubRule _ (SubRule []) s = Just [s]
applySubRule _ (SubRule _) [] = Nothing
applySubRule rules (SubRule rs) s = let
    results = mapMaybe (\rule -> applyAll rules rule s) rs
  in Just (concat results)

part1 :: (Rules, [String]) -> Int
part1 (rules, msgs) = foldl (\acc msg -> if match rules 0 msg then acc+1 else acc) 0 msgs

part2 :: (Rules, [String]) -> Int
part2 (rules, msgs) = part1 (updateRules rules, msgs)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v  -> print (part2 v)
    Left err -> print err
