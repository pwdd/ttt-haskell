module TTT.Console.Game.Runner (play) where

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Game.GameContext
import TTT.Core.Messenger

import TTT.Console.Game.Loop as Game.Loop (loop)
import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.IO.IOContext as IOContext
import TTT.Console.Players.Prompt as Prompt (getBoardDimension
                                            , getMessenger
                                            , askMessenger
                                            , invalidMessenger)
import TTT.Console.Settings as Settings (firstPlayer, secondPlayer, createMessenger)
import TTT.Console.Messenger.EnglishMessenger as EnglishMessenger
import TTT.Console.Messenger.PortugueseMessenger as PortugueseMessenger

play :: IO ()
play = do
  messengerNumber <- Prompt.getMessenger TTT.IO.reader
                                         Prompt.askMessenger
                                         Prompt.invalidMessenger
  let chosenMessenger = Settings.createMessenger messengerNumber
  boardDimension <- Prompt.getBoardDimension TTT.IO.reader
                                             (askBoardDimension chosenMessenger)
                                             (invalidBoardDimension chosenMessenger)
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

