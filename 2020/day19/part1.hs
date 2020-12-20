import Day19.Parser
import Day19.Data

import System.Environment
import qualified Data.Map as Map

match :: Rules -> Int -> String -> Bool
match rules i s = let
    rule = rules Map.! i
    result = apply rules rule s
  in
    case result of
      Nothing -> False
      Just s' -> null s'

apply :: Rules -> Rule -> String -> Maybe String
apply rules c@(SingleChar _) s = applySingleChar rules c s
apply rules r@(SubRule _) s = applySubRule rules r s

applyAll :: Rules -> [Int] -> String -> Maybe String
applyAll _ [] s = Just s
applyAll rules (x:xs) s = let
    r = rules Map.! x
    s' = apply rules r s
  in
    case s' of
      Nothing -> Nothing
      Just s' -> applyAll rules xs s'

applySingleChar :: Rules -> Rule -> String -> Maybe String
applySingleChar _ (SingleChar _) [] = Nothing
applySingleChar _ (SingleChar c) (s:ss) = if s == c then Just ss else Nothing

applySubRule :: Rules -> Rule -> String -> Maybe String
applySubRule _ (SubRule []) [] = Just ""
applySubRule _ (SubRule []) _ = Nothing
applySubRule rules (SubRule (x:xs)) s = let
    s' = applyAll rules x s
  in
    case s' of
      Nothing -> apply rules (SubRule xs) s
      Just s' -> Just s'

part1 :: (Rules, [String]) -> Int
part1 (rules, msgs) = foldl (\acc msg -> if match rules 0 msg then acc+1 else acc) 0 msgs

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
