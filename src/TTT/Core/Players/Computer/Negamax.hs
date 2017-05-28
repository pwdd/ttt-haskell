module TTT.Core.Players.Computer.Negamax ( startDepth
                                         , baseDepth
                                         , maxDepth
                                         , boardAnalysis
                                         , scores
                                         ) where

import Control.Parallel.Strategies as Strategy
import Data.List

import TTT.Core.Types
import TTT.Core.Board as Board (availableSpots, isEmpty, placeMarker)
import TTT.Core.Game.GameContext
import TTT.Core.Game.Status as Game.Status (gameOver, winnerMarker)
import TTT.Core.Players.Player

startDepth = 0 :: Int
baseDepth = 100 :: Int

maxDepth :: Board -> Int
maxDepth board
  | length board == 9 = 10
  | otherwise = 4

score :: GameContext -> Int
score gameContext
  | isFinal (board gameContext) (depth gameContext) = boardAnalysis gameContext
  | otherwise = 0

isFinal :: Board -> Int -> Bool
isFinal board depth = Game.Status.gameOver board || depth >= maxDepth board

boardAnalysis :: GameContext -> Int
boardAnalysis gameContext@ GameContext { board = board
                                       , currentPlayer = currentPlayer
                                       , opponent = opponent
                                       , depth = depth }
  | winnerMarker == Just (marker currentPlayer) = baseDepth - depth
  | winnerMarker == Just (marker opponent) = depth - baseDepth
  | otherwise  = 0
  where winnerMarker = Game.Status.winnerMarker board

createNextBoards :: Board -> [Int] -> Marker -> [Board]
createNextBoards board availableSpots currentPlayerMarker =
  (\spot -> Board.placeMarker spot currentPlayerMarker board) <$> availableSpots

negateScores :: [Int] -> [Int]
negateScores = map negate

parallelsearch :: [Board] -> Player -> Player -> Int -> [Int]
parallelsearch nextBoards opponent currentPlayer depth =
  Strategy.parMap Strategy.rdeepseq
    (\board -> negamax GameContext { board = board
                                   , currentPlayer = opponent
                                   , opponent = currentPlayer
                                   , depth = (succ depth)
                                   }) nextBoards

scores :: GameContext -> [Int]
scores gameContext@ GameContext { board = board
                                , currentPlayer = currentPlayer
                                , opponent = opponent
                                , depth = depth
                                } = do
  let spots = Board.availableSpots board
  let nextBoards = createNextBoards board spots (marker currentPlayer)
  negateScores $ parallelsearch nextBoards opponent currentPlayer depth

negamax :: GameContext -> Int
negamax gameContext@ GameContext { board = board
                                 , currentPlayer = currentPlayer
                                 , opponent = opponent
                                 , depth = depth
                                 }
  | Game.Status.gameOver board || depth >= maxDepth board = boardAnalysis gameContext
  | otherwise = maximum (scores gameContext)

