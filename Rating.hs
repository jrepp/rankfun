module Rating 
    where

import Types

-- Simplified anonymous compute outcome, combination of rating and winloss delta
data ComputeOutcome = ComputeOutcome Rating WinLoss
    deriving (Show)

--
-- Global system volatility recommended .3-1.2
--
volatilityOverTime = 0.3

--
-- Elo Conversions
--
fromElo elo = (elo - 1500) /  173.7178
fromEloDeviation rd = (rd / 173.7178)

--
-- Given a rating and set of outcomes from the rating period produce a new
-- rating 
--
-- Glick Step 3 - Calculate estimate of variance from game outcomes
--
g :: Phi -> Phi
g phi = 1 / sqrt (1 + ((3 * (phi * phi)) / (pi * pi)))

innerE :: Mu -> Mu -> Phi -> Mu
innerE mu muJ phiJ = 1.0 / (1.0 + exp (- (g phiJ) * (mu - muJ)))

-- Rating value extractions
mu :: Rating -> Mu
mu (Rating u _ _) = u
phi :: Rating -> Phi
phi (Rating _ p _) = p
sigma :: Rating -> Sigma
sigma (Rating _ _ s) = s

--
-- Compute outcome extractions
--
outcomeRating :: ComputeOutcome -> Rating
outcomeRating (ComputeOutcome r _) = r
outcomeWinLoss :: ComputeOutcome -> WinLoss
outcomeWinLoss (ComputeOutcome _ wl) = wl

--
-- Inner function to evaluate v over j
--
innerV :: Rating -> ComputeOutcome -> Double
innerV r co = (g(pJ)^2 * ev) * (1 - ev)
                where 
                    u = mu r
                    orJ = outcomeRating co
                    uJ = mu orJ
                    pJ = phi orJ
                    ev = (innerE u uJ pJ)


--
-- Calculate variability from outcomes TODO: []-1 assuming matrix inverse?
--
v :: Rating -> [ComputeOutcome] -> [Double]
v rating outcomes = map (innerV rating) outcomes


--
-- Calculate the inner delta for the rating update
--
innerDelta :: Rating -> ComputeOutcome -> Mu
innerDelta r co = g(pJ) * (winLossJ - (innerE u uJ pJ))
                            where   orJ = outcomeRating co
                                    winLossJ = outcomeWinLoss co
                                    u = mu r
                                    uJ = mu orJ
                                    pJ = phi orJ


--
-- Calculate the delta improvement over the outcomes in the rating period
--
deltaImprovement :: Rating -> [ComputeOutcome] -> [Mu]
deltaImprovement rating outcomes = map (innerDelta rating) outcomes

--where   deltasq = delta^2
        --sigmasq = s^2
        --logsigmasq = ln (sigmasq)
        --A = logsigmasq
        --a = logsigmasq
        --B = innerB deltasq sigmasq - v
        --epsilon = 0.0000001


--innerB deltasq sigmasq = if deltasq > sigmasq then 
                            --log (deltasq - sigmasq - v)
                         --else
                            --iteration a r
--iteration :: Double -> Double
--iteration (f(a - k*r) < 0) k = succ k
--iteration = 1

-- Update phi 
updatePhi :: Phi -> Sigma -> Phi
updatePhi phi sigma = sqrt(phi^2 + sigma^2)

-- Clamp elo deviation
minEloDeviation :: EloDeviation -> EloDeviation
minEloDeviation rd = min rd 350

-- Given a number of rating periods t, calculate the clamped new rating
-- deviation in elo number space
eloDeviationDecay :: Integer -> Integer -> Integer
eloDeviationDecay t rd = min sqrt( (rd^2) + (t * (c^2)) ) 250
                            where c = 15 -- constant increase


