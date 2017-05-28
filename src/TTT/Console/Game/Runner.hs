module TTT.Console.Game.Runner (play) where

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Game.GameContext
import TTT.Core.Messenger

import TTT.Console.Game.Loop as Game.Loop (loop)
import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.IO.IOContext as IOContext
import TTT.Console.Players.Prompt as Prompt (getBoardDimension)
import TTT.Console.Settings as Settings (firstPlayer, secondPlayer)
import TTT.Console.Messenger.EnglishMessenger as EnglishMessenger

play :: IO ()
play = do
  let chosenMessenger = Messenger { chooseANumber = EnglishMessenger.chooseANumber'
                                  , invalidMove = EnglishMessenger.invalidMove'
                                  , currentPlayerIs = EnglishMessenger.currentPlayerIs'
                                  , draw = EnglishMessenger.draw'
                                  , winner = EnglishMessenger.winner'
                                  , askBoardDimension = EnglishMessenger.askBoardDimension'
                                  , invalidBoardDimension = EnglishMessenger.invalidBoardDimension'
                                  }

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

