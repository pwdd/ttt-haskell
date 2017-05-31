module TTT.Core.Validation ( playerRoleOptions
                           , isValidMove
                           , isValidBoardDimension
                           , isValidPlayerRole
                           ) where

import Data.Map.Strict as Map

import TTT.Core.Types

import TTT.Core.Board as Board (isSpotAvailable)

playerRoleOptions :: Map.Map String Int
playerRoleOptions = Map.fromList [("Human", 1), ("Computer", 2)]

isValidMove :: Board -> Int -> Bool
isValidMove board spot = isInRange && Board.isSpotAvailable board spot
  where isInRange = spot >= 0 && spot < length board

isValid :: Int -> Map.Map String Int -> Bool
isValid input options = input `elem` options

isValidBoardDimension :: Int -> Bool
isValidBoardDimension input = input == 3 || input == 4

isValidPlayerRole :: Int -> Map.Map String Int -> Bool
isValidPlayerRole = isValid

