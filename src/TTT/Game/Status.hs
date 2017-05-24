module TTT.Game.Status (
                          winCombos
                        , winningCombo
                        , hasRepeatedMarkers
                        , isDraw
                        , gameOver
                        , winnerMarker
                        ) where

import Types

import TTT.Board as Board

winCombos :: Int -> [[Int]]
winCombos boardDimension = concat [
    Board.rows boardDimension
  , Board.columns boardDimension
  , Board.diagonals boardDimension
  ]

hasRepeatedMarkers :: Board -> [Int] -> Bool
hasRepeatedMarkers board combo =
  first /= Board.emptySpot && all (== head markers) (tail markers)
  where markers = map (\index -> board !! index) combo
        first = head markers

winningCombo :: Board -> [Int]
winningCombo board =
  if null indices
     then []
     else head indices
       where boardDimension = Board.dimension board
             indices = filter (hasRepeatedMarkers board) (winCombos boardDimension)

isDraw :: Board -> Bool
isDraw board = Board.isFull board && null (winningCombo board)

gameOver :: Board -> Bool
gameOver board = isDraw board || not (null (winningCombo board))

winnerMarker :: Board -> Maybe Marker
winnerMarker board =
  if null combo
     then Nothing
     else Just $ board !! head combo
       where combo = winningCombo board

