module TTT.Console.Settings (gameContext, ioContext) where

import TTT.Core.Types
import TTT.Core.Game.GameContext

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Messenger
import TTT.Core.Players.Player
import TTT.Core.Players.Computer.Negamax as Negamax

import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.IO.IOContext as IOContext
import TTT.Console.Messenger.EnglishMessenger as EnglishMessenger

boardLength = 9
firstPlayerMarker = 'x'
secondPlayerMarker = 'o'

englishMessenger = Messenger { chooseANumber = EnglishMessenger.chooseANumber'
                             , invalidMove = EnglishMessenger.invalidMove'
                             , currentPlayerIs = EnglishMessenger.currentPlayerIs'
                             , draw = EnglishMessenger.draw'
                             , winner = EnglishMessenger.winner'
                             , strBoard = EnglishMessenger.strBoard'
                             }

gameContext = GameContext { board = Board.newBoard $ boardLength
                          , currentPlayer = Player { marker = firstPlayerMarker , isAI = False }
                          , opponent = Player { marker = secondPlayerMarker , isAI = True }
                          , depth = Negamax.startDepth
                          }

ioContext = IOContext { IOContext.reader = TTT.IO.reader
                      , IOContext.printer = TTT.IO.printer
                      , IOContext.messenger = englishMessenger }

