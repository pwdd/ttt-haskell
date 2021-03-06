module TTT.Console.Game.Loop (loop) where

import TTT.Core.Types
import TTT.Core.Game.GameContext
import TTT.Core.Players.Player

import TTT.Core.Board as Board (isEmpty, placeMarker)
import TTT.Core.Game.Status as Game.Status (isDraw, gameOver, winningCombo)
import TTT.Core.Players.Computer.HardComputer as Computer (getSpot)

import TTT.Messenger.Messenger

import TTT.Console.BoardRepresentation as BoardRepresentation (strBoard)
import TTT.Console.IO.IO as TTT.IO (clearScreen)
import TTT.Console.IO.IOContext
import TTT.Console.IO.Prompt as Prompt (getSettings)
import TTT.Console.Players.User as User (getSpot)

computerWaitingTime = 1000000
humanWaitingTime = 0

loop :: IOContext -> GameContext -> IO ()
loop ioContext@ IOContext { printer = printer
                          , reader = reader
                          , clear = clear
                          , messenger = messenger
                          }
     gameContext@ GameContext { board = board
                              , currentPlayer = currentPlayer
                              , opponent = opponent
                              , depth = depth
                              } = do

  firstEmptyBoard printer currentPlayer messenger board

  spot <- getMove ioContext gameContext
  let nextBoard = Board.placeMarker (spot :: Int) (marker currentPlayer) board

  clearTerminal currentPlayer clear
  printer $ movedTo messenger (marker currentPlayer) spot

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

firstEmptyBoard :: (String -> IO ()) -> Player -> Messenger -> Board -> IO ()
firstEmptyBoard printer currentPlayer messenger board
  | isAI currentPlayer = printer "\n"
  | otherwise = printer $ initialStateString messenger (Board.isEmpty board)
                                                       (BoardRepresentation.strBoard board)

clearTerminal :: Player -> (Int -> IO ()) -> IO ()
clearTerminal currentPlayer clearer
  | isAI currentPlayer = clearer computerWaitingTime
  | otherwise = clearer humanWaitingTime

getMove :: IOContext -> GameContext -> IO Int
getMove ioContext@ IOContext { printer = printer
                             , reader = reader
                             , clear = clear
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

