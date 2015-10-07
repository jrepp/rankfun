-- Glicko2 rating module
-- mu = Rating
-- phi = Deviation
-- sigma = Volatility
--
module Types 
    where

type Rank = Int
type PlayerId = Int
type GameId = Int
type ServiceLabel = Int
type ServiceEpoch = Int
type RegionId = Int
type ProgramId = Int
type Mu = Double -- Rating center
type Phi = Double -- Rating deviation
type Sigma = Double -- Rating volatility
type Distribution = Double -- 0-1 in division
type WinLoss = Double -- 0-1
type TimeStamp = [Char]
type DivisionId = Int
type GameDuration = Double
type PeriodLength = Double
type Elo = Double
type EloDeviation = Double

-- A single rating value
data Rating = Rating  Mu Phi Sigma
    deriving (Ord, Eq, Show)

