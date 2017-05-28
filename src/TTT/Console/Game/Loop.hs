module TTT.Console.Game.Loop (loop) where

import TTT.Core.Types
import TTT.Core.Game.GameContext
import TTT.Core.Players.Player

import TTT.Core.Board as Board (isEmpty, placeMarker)
import TTT.Core.Game.Status as Game.Status (isDraw, gameOver, winningCombo)
import TTT.Core.Players.Computer.HardComputer as Computer (getSpot)

import TTT.Messenger.Messenger

import TTT.Console.IO.IOContext
import TTT.Console.IO.Prompt as Prompt (getSettings)
import TTT.Console.BoardRepresentation as BoardRepresentation (strBoard)
import TTT.Console.Players.User as User (getSpot)

loop :: IOContext -> GameContext -> IO ()
loop ioContext@ IOContext { printer = printer, reader = reader, messenger = messenger }
     gameContext@ GameContext { board = board
                              , currentPlayer = currentPlayer
                              , opponent = opponent
                              , depth = depth
                              } = do

  printer $ initialStateString messenger (Board.isEmpty board)
                                         (BoardRepresentation.strBoard board)

  spot <- getMove ioContext gameContext
  let nextBoard = Board.placeMarker (spot :: Int) (marker currentPlayer) board

  printer $ BoardRepresentation.strBoard nextBoard

  if Game.Status.gameOver nextBoard
     then printer $ finalMessage messenger (marker currentPlayer)
                                           (Game.Status.isDraw nextBoard)
                                           (Game.Status.winningCombo nextBoard)
     else
       loop ioContext (gameContext { board = nextBoard
                                   , currentPlayer = opponent
                                   , opponent = currentPlayer
                                   , depth = depth
                                   })

getMove :: IOContext -> GameContext -> IO Int
getMove ioContext@ IOContext { printer = printer
                             , reader = reader
                             , messenger = messenger
                             }
        gameContext@ GameContext { board = board
                                 , currentPlayer = currentPlayer
                                 , opponent = opponent
                                 , depth = depth
                                 }

  | not (isAI currentPlayer) = User.getSpot reader
                                            printer
                                            (chooseANumber messenger board)
                                            (invalidMove messenger)
                                            board
  | otherwise = return $ Computer.getSpot gameContext

