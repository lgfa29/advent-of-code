import Day8.Data
import Day8.Parser
import qualified Data.Set as Set

runTillLoop :: Set.Set Int -> Device -> Either (DeviceState, Device) Device
runTillLoop hist d = let
    d' = step (Right d)
  in
  case d' of
    Left a -> Left (a, d)
    Right d' -> if Set.member (pc d') hist
                then Left (LOOP, d)
                else runTillLoop (Set.insert (pc d') hist) d'

part1 :: Device -> Int
part1 d = let
    d' = runTillLoop Set.empty d
  in
  acc (
    case d' of
      Left (_, d') -> d'
      Right d' -> d'
  )

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 1: "
  case input of
    Right v -> print (part1 (Device 0 0 v))
    Left err -> print err
