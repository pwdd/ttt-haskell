module TTT.Validation (isValidMove) where

import Types
import TTT.Board as Board

isValidMove :: Board -> Int -> Bool
isValidMove board spot = isInRange && Board.isSpotAvailable board spot
  where isInRange = spot >= 0 && spot < length board

