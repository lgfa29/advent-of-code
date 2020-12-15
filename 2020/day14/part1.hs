import Day14.Parser
import Day14.Data

import System.Environment
import Data.Bits
import Data.Char
import qualified Data.Map as Map

runProgram :: Computer -> [Instruction] -> Computer
runProgram c [] = c
runProgram c xs = foldl runInstruction c xs

runInstruction :: Computer -> Instruction -> Computer
runInstruction c x = case op x of
                       Mask -> runMask c x
                       Mem -> runMem c x

runMask :: Computer -> Instruction -> Computer
runMask c@(Computer mem _) (Instruction o _ m)
  | o /= Mask = c
  | otherwise = Computer mem m

runMem :: Computer -> Instruction -> Computer
runMem c@(Computer mem mask) (Instruction o k v)
  | o /= Mem = c
  | otherwise = Computer (updateMemory k (read v) mask mem) mask

updateMemory :: Int -> Int -> String -> Map.Map Int Int -> Map.Map Int Int
updateMemory k v mask = Map.insert k (applyMask mask v)

applyMask :: String -> Int -> Int
applyMask mask v = let
    orMask =  buildOrMask mask
    andMask = buildAndMask mask
  in
    (.&.) ((.|.) v orMask) andMask

buildOrMask :: String -> Int
buildOrMask m = let
    orMask = map (\c -> if c == 'X' then '0' else c) m
  in
    binStrToInt orMask

buildAndMask :: String -> Int
buildAndMask m = let
    andMask = map (\c -> if c == 'X' then '1' else c) m
  in
    binStrToInt andMask

binStrToInt :: String -> Int
binStrToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

sumMemory :: Computer -> Int
sumMemory (Computer m _) = Map.foldl (+) 0 m

part1 :: [Instruction] -> Int
part1 program = let
    c = Computer Map.empty ""
    c' = runProgram c program
  in
    sumMemory c'

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 v)
    Left err -> print err
