module Day8.Data
( Instruction (..)
, Device (..)
, DeviceState(..)
, step
) where

data Instruction = Instruction { op :: String, arg :: Int }
data Device = Device { pc :: Int, acc :: Int, program :: [Instruction] }
data DeviceState = EOE | LOOP | ERR_OP deriving (Enum, Show, Eq)

instance Show Instruction where
  show (Instruction op arg) = op ++ " " ++ show arg

instance Show Device where
  show (Device pc acc _) = "PC: " ++ show pc ++ "\tACC: " ++ show acc

step :: Either DeviceState Device -> Either DeviceState Device
step input = case input of
  Left _ -> input
  Right d -> if pc d >= length (program d) then Left EOE else step' d

step' :: Device -> Either DeviceState Device
step' d@(Device pc _ program) = let
    current = program !! pc
    x = arg current
  in
    case op current of
      "acc" -> Right(applyAcc x d)
      "jmp" -> Right(applyJmp x d)
      "nop" -> Right(applyNop x d)
      _ -> Left ERR_OP

applyAcc :: Int -> Device -> Device
applyAcc arg (Device pc acc is) = Device (pc + 1) (acc + arg) is

applyJmp :: Int -> Device -> Device
applyJmp arg (Device pc acc is) = Device (pc + arg) acc is

applyNop :: Int -> Device -> Device
applyNop _ (Device pc acc is) = Device (pc + 1) acc is
