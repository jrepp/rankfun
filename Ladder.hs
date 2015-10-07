module Ladder 
    where

import qualified Player as P

--
-- A placement withina single division
--
data Placement = Placement {
    division :: DivisionId,
    rank :: Rank,
    lastRank :: Rank,
    distribution :: Distribution
    } deriving (Show)

