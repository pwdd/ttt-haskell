module TTT.Console.Game.Runner (play) where

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Game.GameContext
import TTT.Core.Messenger
import TTT.Core.Validation as Core.Validation (isValidBoardDimension)

import TTT.Console.Game.Loop as Game.Loop (loop)
import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.IO.IOContext as IOContext
import TTT.Console.IO.Prompt as Prompt ( getSettings
                                       , askMessenger
                                       , invalidMessenger)
import TTT.Console.Settings as Settings (firstPlayer, secondPlayer, createMessenger)
import TTT.Console.Messenger.Validation as Console.Validation (isValidMessenger)

play :: IO ()
play = do
  messengerNumber <- Prompt.getSettings TTT.IO.reader
                                        TTT.IO.printer
                                        Prompt.askMessenger
                                        Prompt.invalidMessenger
                                        Console.Validation.isValidMessenger
  let chosenMessenger = Settings.createMessenger messengerNumber
  boardDimension <- Prompt.getSettings TTT.IO.reader
                                       TTT.IO.printer
                                       (askBoardDimension chosenMessenger)
                                       (invalidBoardDimension chosenMessenger)
                                       Core.Validation.isValidBoardDimension
  let initialBoard = Board.newBoard (boardDimension * boardDimension)
  let initialContext = GameContext { board = initialBoard
                                   , currentPlayer = Settings.firstPlayer
                                   , opponent = Settings.secondPlayer
                                   , depth = 0
                                   }
  let ioContext = IOContext { IOContext.reader = TTT.IO.reader
                            , IOContext.printer = TTT.IO.printer
                            , messenger = chosenMessenger
                            }
  Game.Loop.loop ioContext initialContext

