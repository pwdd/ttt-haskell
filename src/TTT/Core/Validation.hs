module TTT.Core.Validation (isValidMove) where

import TTT.Core.Types

import TTT.Core.Board as Board (isSpotAvailable)

isValidMove :: Board -> Int -> Bool
isValidMove board spot = isInRange && Board.isSpotAvailable board spot
  where isInRange = spot >= 0 && spot < length board
