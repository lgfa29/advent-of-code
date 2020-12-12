module Day7.Data
(
Rule (..)
, Bag (..)
, Statement (..)
) where

data Rule = Rule { container :: Bag, contains :: [Statement] } deriving (Show)
data Bag = Bag { adj :: String, color :: String } deriving (Eq)
data Statement = Statement { num :: Int, bag :: Bag } deriving (Show)

instance Ord Bag where
  compare (Bag a b) (Bag c d) = compare (a ++ " " ++ b) (c ++ " " ++ d)

instance Show Bag where
  show (Bag a b) = a ++ " " ++ b
