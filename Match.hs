module Match
    where

import Types
import qualified Game as Game
import qualified Player as Player

-- Prediction is just an alias for an expected outcome
type Prediction = Outcome
type Predictions = Outcomes

-- The result of a match pre/post conditions
data Result = Result {
    timestamp :: TimeStamp,
    game :: Game.Instance,
    duration :: GameDuration,
    winner :: (Player.Handle, Rating),
    loser :: (Player.Handle, Rating),
    prediction :: (Player.Handle, WinLoss)
    } deriving (Show)

