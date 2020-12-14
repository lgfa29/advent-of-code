import Day12.Parser
import Day12.Data

go :: [Instruction] -> ShipWithWaypoint -> ShipWithWaypoint
go [] s = s
go (i:is) s = go is (move i s)

move :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
move i s = case action i of
             N -> applyN i s
             S -> applyS i s
             E -> applyE i s
             W -> applyW i s
             L -> applyL i s
             R -> applyR i s
             F -> applyF i s

applyN :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyN i (ShipWithWaypoint s wp) =
    ShipWithWaypoint s (Waypoint (wpEast wp) (wpNorth wp + value i))

applyS :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyS i (ShipWithWaypoint s wp) =
    ShipWithWaypoint s (Waypoint (wpEast wp) (wpNorth wp - value i))

applyE :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyE i (ShipWithWaypoint s wp) =
    ShipWithWaypoint s (Waypoint (wpEast wp + value i) (wpNorth wp))

applyW :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyW i (ShipWithWaypoint s wp) =
    ShipWithWaypoint s (Waypoint (wpEast wp - value i) (wpNorth wp))

applyF :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyF i (ShipWithWaypoint s w) = let
    v = value i
    east' = east s + v*wpEast w
    north' = north s + v*wpNorth w
  in
    ShipWithWaypoint (Ship east' north' (direction s)) w

applyL :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyL i = rotate (-(value i))

applyR :: Instruction -> ShipWithWaypoint -> ShipWithWaypoint
applyR i = rotate (value i)

rotate :: Int -> ShipWithWaypoint -> ShipWithWaypoint
rotate angle (ShipWithWaypoint s wp) = let
    rad = fromIntegral angle * (pi/180)
    roundCos = round (cos rad)
    roundSin = round (sin rad)
    wpEast' = wpEast wp * roundCos + wpNorth wp * roundSin
    wpNorth' = -(wpEast wp * roundSin) + wpNorth wp * roundCos
  in
    ShipWithWaypoint s (Waypoint wpEast' wpNorth')

manhattanDist :: ShipWithWaypoint -> ShipWithWaypoint -> Int
manhattanDist (ShipWithWaypoint s _) (ShipWithWaypoint s' _) =
    abs (east s' - east s) + abs (north s' - north s)

part2 :: [Instruction] -> Int
part2 is = let
    sww = ShipWithWaypoint (Ship 0 0 E) (Waypoint 10 1)
  in
    manhattanDist sww (go is sww)

main = do
  contents <- readFile "input.txt"
  let input = parse "input.txt" contents
  putStr "Part 2: "
  case input of
    Right v -> print (part2 v)
    Left err -> print err
