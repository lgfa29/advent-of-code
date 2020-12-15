import Day14.Parser
import Day14.Data

import System.Environment
import Data.Char
import qualified Data.Map as Map

runProgram :: [Instruction] -> Computer -> Computer
runProgram [] c = c
runProgram xs c = foldl runInstruction c xs

runInstruction :: Computer -> Instruction -> Computer
runInstruction c x = case op x of
                       Mask -> runMask c x
                       Mem -> runMem c x

runMask :: Computer -> Instruction -> Computer
runMask c@(Computer mem _) (Instruction o _ m)
  | o /= Mask = c
  | otherwise = Computer mem m

runMem :: Computer -> Instruction -> Computer
runMem  c@(Computer mem mask) (Instruction o k v)
  | o /= Mem = c
  | otherwise = Computer (updateMemory k (read v) mask mem) mask

updateMemory :: Int -> Int -> String -> Map.Map Int Int -> Map.Map Int Int
updateMemory k v mask mem = let
    floatingAddr = applyMask mask k
    addrs = map binStrToInt (expandFloating floatingAddr)
  in
    foldl (\m k -> Map.insert k v m) mem addrs

applyMask :: String -> Int -> String
applyMask mask v = let
    binV = padBinStr (length mask) '0' (intToBinStr v)
  in
    zipWith applyMaskBit mask binV

applyMaskBit :: Char -> Char -> Char
applyMaskBit '0' c = c
applyMaskBit '1' _ = '1'
applyMaskBit 'X' _ = 'X'

expandFloating :: String -> [String]
expandFloating ['X'] = ["0", "1"]
expandFloating [c] = [[c]]
expandFloating ('X':xs) = map ('0' :) (expandFloating xs) ++ map ('1' :) (expandFloating xs)
expandFloating (x:xs) = map (x :) (expandFloating xs)

binStrToInt :: String -> Int
binStrToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

intToBinStr :: Int -> String
intToBinStr 0 = "0"
intToBinStr 1 = "1"
intToBinStr n = intToBinStr (n `quot` 2) ++ show (n `mod` 2)

padBinStr :: Int -> Char -> String -> String
padBinStr n c s = replicate (n - length s) c ++ s

sumMemory :: Computer -> Int
sumMemory (Computer m _) = Map.foldl (+) 0 m

part2 :: [Instruction] -> Int
part2 inst = let
    c = Computer Map.empty ""
    c' = runProgram inst c
  in
    sumMemory c'

main = do
  args <- getArgs
  let fileName = if null args then "input.txt" else head args
  contents <- readFile fileName
  let input = parse fileName contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
