module TTT.Console.Game.Runner (play) where

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Game.GameContext

import TTT.Console.Game.Loop as Game.Loop (loop)
import TTT.Console.IO.IO as TTT.IO (reader, printer)
import TTT.Console.IO.IOContext as IOContext
import TTT.Console.Settings as Settings ( getMessengerNumber
                                        , createMessenger
                                        , getBoardDimension
                                        , getFirstPlayerRole
                                        , getSecondPlayerRole
                                        , createPlayer
                                        )

play :: IO ()
play = do
  messengerNumber <- Settings.getMessengerNumber
  let chosenMessenger = Settings.createMessenger messengerNumber
  boardDimension <- Settings.getBoardDimension chosenMessenger
  let initialBoard = Board.newBoard (boardDimension * boardDimension)
  firstPlayerRole <- Settings.getFirstPlayerRole chosenMessenger
  secondPlayerRole <- Settings.getSecondPlayerRole chosenMessenger
  let firstPlayer = Settings.createPlayer 'x' firstPlayerRole
  let secondPlayer = Settings.createPlayer 'o' secondPlayerRole
  let initialContext = GameContext { board = initialBoard
                                   , currentPlayer = firstPlayer
                                   , opponent = secondPlayer
                                   , depth = 0
                                   }
  let ioContext = IOContext { IOContext.reader = TTT.IO.reader
                            , IOContext.printer = TTT.IO.printer
                            , messenger = chosenMessenger
                            }
  Game.Loop.loop ioContext initialContext

