module RatedPlayer
    where

import Types
import Rating as Rating
import Ladder as Ladder
import Player as Player

-- The binding of player handle to placement data
data Rated = Rated { 
    handle :: Player.Handle,
    rating :: Rating,
    placement :: Ladder.Placement
    } deriving (Show)

-- Basic constructors
unratedPlayer = RatedPlayer 
    (Handle 0 0 0)
    (Rating (Rating.fromElo 1500) (Rating.fromEloDeviation 360) 0.06)
    (Ladder.Placement 0 0 0 0)

proPlayer = RatedPlayer
    (Player.Handle 0 0 0)
    (Rating (R.fromElo 2800) (R.fromEloDeviation 50) 0.06)
    (Ladder.Placement 0 0 0 0)

