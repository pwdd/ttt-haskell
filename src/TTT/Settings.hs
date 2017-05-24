module TTT.Settings (
                      firstPlayerMarker
                    , secondPlayerMarker
                    , boardLength
                    , englishMessenger
                    , startContext
                    ) where

import Types
import TTT.Board as Board
import TTT.Game.Context
import TTT.IO as TTT.IO
import TTT.Players.Player
import TTT.Messenger.Messenger
import TTT.Messenger.EnglishMessenger as EnglishMessenger

firstPlayerMarker = 'x'
secondPlayerMarker = 'o'
boardLength = 9

englishMessenger = Messenger {
                                chooseANumber = EnglishMessenger.chooseANumber'
                              , invalidMove = EnglishMessenger.invalidMove'
                              , currentPlayerIs = EnglishMessenger.currentPlayerIs'
                              , draw = EnglishMessenger.draw'
                              , winner = EnglishMessenger.winner'
                              , strBoard = EnglishMessenger.strBoard'
                              }

startContext = Context {
                          board = Board.newBoard boardLength
                        , currentPlayer = Player { marker = firstPlayerMarker, isAI = False }
                        , opponent = Player { marker = secondPlayerMarker, isAI = False }
                        , messenger = englishMessenger
                        , reader = TTT.IO.readln
                        , printer = TTT.IO.println
                        }

