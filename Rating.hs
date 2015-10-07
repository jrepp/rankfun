module Rating 
    where

import Types

-- Simplified anonymous compute outcome, combination of rating and winloss delta
data ComputeOutcome = ComputeOutcome Rating WinLoss
    deriving (Show)

--
-- Global system volatility recommended .3-1.2
--
volatilityOverTime = 0.3 :: Double 
eloScale = 173.7178 :: Double
eloBase = 1500.0 :: Double 

--
-- Elo Conversions
--
fromElo :: Elo -> Mu
fromElo elo = (elo - eloBase) /  eloScale
toElo :: Mu -> Elo
toElo mu = (mu * eloScale) + eloBase
fromEloDeviation :: EloDeviation -> Phi
fromEloDeviation rd = (rd / eloScale)
toEloDeviation :: Phi -> EloDeviation
toEloDeviation phi = (phi * eloScale)

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
deltaImprovement :: Rating -> [ComputeOutcome] -> Mu
deltaImprovement rating outcomes = u + 
    (q / (1 / phisq) + (1 / dsq)) * (sum $ map (innerDelta rating) outcomes)
    where 
        u = mu rating
        phisq = (phi rating)^2
        q = log 10 / 400
        dsq = 1 -- TODO: substitute 

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
--
innerDsq :: Rating -> Rating -> WinLoss -> Double
innerDsq rating otherRating winLoss = (g $ pJ)^2 * 
    (innerE u uJ pJ) * 
    (1 - innerE u uJ pJ)
    where
        u = mu rating
        uJ = mu otherRating 
        pJ = phi otherRating

-- Update phi 
updatePhi :: Phi -> Sigma -> Phi
updatePhi phi sigma = sqrt (phi^2 + sigma^2)

-- Clamp elo deviation
minEloDeviation :: EloDeviation -> EloDeviation
minEloDeviation rd = min rd 350

-- Given a number of rating periods t, calculate the clamped new rating
-- deviation in elo number space
eloDeviationDecay :: PeriodLength -> EloDeviation -> EloDeviation
eloDeviationDecay t rd = min (sqrt (rd^2) + t * (c^2) ) 350
                            where c = 15.0 -- constant increase

--
-- Calculate a 95% confidence interval of player strength in
-- elo space
--
playerStrength :: Rating -> (Elo, Elo)
playerStrength r = ((elo - strength2), (elo + strength2))
                   where
                        elo = toElo $ mu r
                        strength2 = 2.0 * (toEloDeviation $ phi r)

-- Example data from paper
player1 = Rating (fromElo 1400) (fromEloDeviation 30) 0
player2 = Rating (fromElo 1550) (fromEloDeviation 100) 0
player3 = Rating (fromElo 1700) (fromEloDeviation 300) 0

ratingCentered = Rating 0.0 2.03822 0
player0 = Rating (fromElo 1500) (fromEloDeviation 200) 0


