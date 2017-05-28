module TTT.Console.Settings ( firstPlayer
                            , secondPlayer
                            , messengerOptions
                            , createMessenger
                            , englishMessenger
                            , portugueseMessenger
                            ) where

import TTT.Core.Players.Player
import TTT.Core.Messenger
import TTT.Console.Messenger.EnglishMessenger as EnglishMessenger
import TTT.Console.Messenger.PortugueseMessenger as PortugueseMessenger

english = 1
portuguese = 2

messengerOptions = [english, portuguese] :: [Int]

createMessenger :: Int -> Messenger
createMessenger messengerNumber
  | messengerNumber == portuguese = portugueseMessenger
  | otherwise = englishMessenger

firstPlayerMarker = 'x'
secondPlayerMarker = 'o'

firstPlayer = Player { marker = firstPlayerMarker, isAI = False }
secondPlayer = Player { marker = secondPlayerMarker, isAI = True  }

englishMessenger = Messenger { chooseANumber = EnglishMessenger.chooseANumber'
                             , invalidMove = EnglishMessenger.invalidMove'
                             , currentPlayerIs = EnglishMessenger.currentPlayerIs'
                             , draw = EnglishMessenger.draw'
                             , winner = EnglishMessenger.winner'
                             , askBoardDimension = EnglishMessenger.askBoardDimension'
                             , invalidBoardDimension = EnglishMessenger.invalidBoardDimension'
                             , initialStateString = EnglishMessenger.initialStateString'
                             , finalMessage = EnglishMessenger.finalMessage'
                             }

portugueseMessenger = Messenger { chooseANumber = PortugueseMessenger.chooseANumber'
                                , invalidMove = PortugueseMessenger.invalidMove'
                                , currentPlayerIs = PortugueseMessenger.currentPlayerIs'
                                , draw = PortugueseMessenger.draw'
                                , winner = PortugueseMessenger.winner'
                                , askBoardDimension = PortugueseMessenger.askBoardDimension'
                                , invalidBoardDimension = PortugueseMessenger.invalidBoardDimension'
                                , initialStateString = PortugueseMessenger.initialStateString'
                                , finalMessage = PortugueseMessenger.finalMessage'
                                }

