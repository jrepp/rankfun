module Player
    where

import Types
import Rating as R


--
-- A globally unique locator for a player (game account)
--
data Handle = Handle {
    program :: ProgramId,
    region :: RegionId,
    id :: PlayerId
    }
    deriving (Eq, Show)

nohandle = Handle 0 0 0


