import Day18.Data
import Day18.Parser
import System.Environment

evaluateExpression :: Expression -> Int
evaluateExpression [n@Number{}] = value n
evaluateExpression (RightParen:xs) = let
    leftParenIdx = findMatchingParen xs 1 0
    innerEx = evaluateExpression (take leftParenIdx xs)
    rest = drop (leftParenIdx + 1) xs
  in
    evaluateExpression (Number innerEx:rest)
evaluateExpression (n@Number{}:xs) = let
    left = value n
    op = head xs
    right = evaluateExpression (tail xs)
  in case op of
    OpPlus -> left + right
    OpMult -> left * right

findMatchingParen :: Expression -> Int -> Int -> Int
findMatchingParen (LeftParen:_) 1 i = i
findMatchingParen (LeftParen:xs) count i = findMatchingParen xs (count-1) (i+1)
findMatchingParen (RightParen:xs) count i = findMatchingParen xs (count+1) (i+1)
findMatchingParen (_:xs) count i = findMatchingParen xs count (i+1)

part1 :: [Expression] -> Int
part1 exps = let
    reversedExps = map reverse exps
  in
    sum (map evaluateExpression reversedExps)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v  -> print (part1 v)
    Left err -> print err
