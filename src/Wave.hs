module Wave where
import Data.V2
type Wave = Vector (Vector Bool)
-- | Find the element with the lowest nonzero entropy 
  -- with this lowest he chooses a random *valid* tile for this index
  -- and bans all other tile possibilities,
  -- then he propagates this tile choice to the the neighbouring pixels
observe :: Wave -> Index
observe = undefined 
