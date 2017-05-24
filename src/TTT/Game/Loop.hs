module TTT.Game.Loop (loop) where

import Types
import TTT.Board as Board
import TTT.Game.Context
import TTT.Game.Status as Game.Status
import TTT.Messenger.Messenger
import TTT.Players.Player
import TTT.Prompt as Prompt

loop :: Context -> IO ()
loop context@ Context {
                        board = board
                      , currentPlayer = currentPlayer
                      , opponent = opponent
                      , messenger = messenger
                      , reader = reader
                      , printer = printer
                      } = do

  spot <- Prompt.getSpot reader (chooseANumber messenger) board
  let nextBoard = Board.placeMarker (spot :: Int) (marker currentPlayer) board

  printer $ strBoard messenger nextBoard

  if Game.Status.gameOver nextBoard
     then do
       printer $ strBoard messenger nextBoard
       printer $ finalMessage currentPlayer nextBoard messenger
     else
       loop (context {
                        board = nextBoard
                      , currentPlayer = opponent
                      , opponent = currentPlayer
                      , messenger = messenger
                      , reader = reader
                      , printer = printer
                      })

finalMessage :: Player -> Board -> Messenger -> String
finalMessage currentPlayer board messenger =
  if Game.Status.isDraw board
     then draw messenger
     else winner messenger (marker currentPlayer) (Game.Status.winningCombo board)

