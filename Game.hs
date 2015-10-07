module Game
    where
    
import Types

--
-- A game that ran on a particular service
--
data Instance = Instance GameId ServiceLabel ServiceEpoch
    deriving (Eq, Show)

