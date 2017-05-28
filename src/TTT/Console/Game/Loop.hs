module TTT.Console.Game.Loop (loop) where

import TTT.Core.Types
import TTT.Core.Game.GameContext
import TTT.Core.Messenger
import TTT.Core.Players.Player

import TTT.Core.Board as Board (isEmpty, placeMarker)
import TTT.Core.Game.Status as Game.Status (isDraw, gameOver, winningCombo)
import TTT.Core.Players.Computer.Negamax as Negamax (getSpot)

import TTT.Console.IO.IOContext
import TTT.Console.Players.Prompt as Prompt (getSpot)
import TTT.Console.BoardRepresentation as BoardRepresentation (strBoard)

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

  | not (isAI currentPlayer) = Prompt.getSpot reader
                                              (chooseANumber messenger board)
                                              (invalidMove messenger)
                                              board
  | otherwise = return $ Negamax.getSpot gameContext

