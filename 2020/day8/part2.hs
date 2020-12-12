import Day8.Data
import Day8.Parser
import qualified Data.Set as Set

generateFixes :: [Instruction] -> [[Instruction]]
generateFixes [] = []
generateFixes (x:xs) = map (:xs) (fix x) ++ map ([x]++) (generateFixes xs)

fix :: Instruction -> [Instruction]
fix x
  | op x == "nop" = [Instruction "jmp" (arg x)]
  | op x == "jmp" = [Instruction "nop" (arg x)]
  | otherwise = []

runTillLoop :: Set.Set Int -> Device -> Either (DeviceState, Device) Device
runTillLoop hist d = let
    d' = step (Right d)
  in
  case d' of
    Left a -> Left (a, d)
    Right d' -> if Set.member (pc d') hist
                then Left (LOOP, d)
                else runTillLoop (Set.insert (pc d') hist) d'

deviceFinished :: Either (DeviceState, Device) Device -> Bool
deviceFinished r = case r of
  Left e -> fst e == EOE
  Right _ -> False

part2 :: [Instruction] -> Int
part2 program = let
    fixes = generateFixes program
    runs = map (runTillLoop Set.empty . Device 0 0 ) fixes
    result = head (filter deviceFinished runs)
  in
    case result of
      Left a -> acc (snd a)
      Right _ -> minBound :: Int

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
