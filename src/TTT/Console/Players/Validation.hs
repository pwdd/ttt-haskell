module TTT.Console.Players.Validation (isValidMarker) where

import TTT.Core.Types

isValidMarker :: Marker -> Marker -> Bool
isValidMarker marker opponentMarker =
  (marker `elem` ['a'..'z'] || marker `elem` ['A'..'Z']) && marker /= opponentMarker
