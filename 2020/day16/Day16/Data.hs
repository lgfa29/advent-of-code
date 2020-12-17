module Day16.Data (
  Input (..)
, Rule (..)
, Rules (..)
, Ticket (..)
) where

import qualified Data.Map as Map

data Input = Input { rules         :: Rules
                   , myTicket      :: Ticket
                   , nearbyTickets :: [Ticket]
                   } deriving (Show)
type Rules = Map.Map String Rule
type Rule = [(Int, Int)]
type Ticket = [Int]
