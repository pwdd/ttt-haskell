module TTT.Core.Players.Computer.HardComputer (getSpot) where

import Data.Maybe
import Data.List
import TTT.Core.Game.GameContext

import TTT.Core.Board as Board (isEmpty, availableSpots, center)
import TTT.Core.Players.Computer.Negamax as Negamax (scores)

getSpot :: GameContext -> Int
getSpot gameContext@ GameContext { board = board
                                 , currentPlayer = currentPlayer
                                 , opponent = opponent
                                 , depth = depth
                                 }
  | Board.isEmpty board = Board.center $ length board
  | otherwise = spots !! best
  where
    spots = Board.availableSpots board
    scored = Negamax.scores gameContext
    maxValue = maximum scored
    best = fromJust $ elemIndex maxValue scored

