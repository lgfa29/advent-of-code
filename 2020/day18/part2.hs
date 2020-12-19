import Day18.Data
import Day18.Parser
import System.Environment

evaluateExpression :: Expression -> Int
evaluateExpression [n@Number{}] = value n
evaluateExpression exp@(RightParen:_) = evaluateExpression (evaluateParens exp)
evaluateExpression exp@(Number{}:xs) = case head xs of
  OpPlus -> evaluateExpression (evaluateSum exp)
  OpMult -> evaluateExpression (evaluateMult exp)

evaluateSum :: Expression -> Expression
evaluateSum (n@Number{}:xs) = let
    left = value n
    rest = case head (tail xs) of
      RightParen -> evaluateParens (tail xs)
      Number _ -> tail xs
    result = left + value (head rest)
  in
    Number result :  tail rest

evaluateMult :: Expression -> Expression
evaluateMult (n@Number{}:xs) = let
    left = value n
    right = evaluateExpression (tail xs)
    result = left * right
  in
    [Number result]

evaluateParens :: Expression -> Expression
evaluateParens (RightParen:xs) = let
    leftParenIdx = findMatchingParen xs 1 0
    innerEx = evaluateExpression (take leftParenIdx xs)
    rest = drop (leftParenIdx + 1) xs
  in
    Number innerEx : rest

findMatchingParen :: Expression -> Int -> Int -> Int
findMatchingParen (LeftParen:_) 1 i = i
findMatchingParen (LeftParen:xs) count i = findMatchingParen xs (count-1) (i+1)
findMatchingParen (RightParen:xs) count i = findMatchingParen xs (count+1) (i+1)
findMatchingParen (_:xs) count i = findMatchingParen xs count (i+1)

part2 :: [Expression] -> Int
part2 exps = let
    reversedExps = map reverse exps
  in
    sum (map evaluateExpression reversedExps)

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v  -> print (part2 v)
    Left err -> print err
