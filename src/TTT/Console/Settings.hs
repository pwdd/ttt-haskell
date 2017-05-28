module TTT.Console.Settings ( getMessengerNumber
                            , createMessenger
                            , getBoardDimension
                            , firstPlayer
                            , secondPlayer
                            , englishMessenger
                            , portugueseMessenger
                            ) where

import qualified Data.Map.Strict as Map

import TTT.Core.Types
import TTT.Core.Players.Player
import TTT.Core.Validation as Core.Validation (isValidBoardDimension)
import TTT.Messenger.Messenger
import TTT.Messenger.EnglishMessenger as EnglishMessenger
import TTT.Messenger.PortugueseMessenger as PortugueseMessenger
import TTT.Messenger.Validation as Console.Validation
import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.IO.Prompt as Prompt (getSettings)

askMessenger = "\nEnter 1 for English\nDigite 2 para Português\n"
invalidMessenger = ""

getMessengerNumber :: IO Int
getMessengerNumber = Prompt.getSettings TTT.IO.reader
                                        TTT.IO.printer
                                        askMessenger
                                        invalidMessenger
                                        Console.Validation.isValidMessenger

createMessenger :: Int -> Messenger
createMessenger messengerNumber
  | messengerNumber == Console.Validation.messengerOptions Map.! "Portuguese" = portugueseMessenger
  | otherwise = englishMessenger

getBoardDimension :: Messenger -> IO Int
getBoardDimension messenger = Prompt.getSettings TTT.IO.reader
                                                 TTT.IO.printer
                                                 (askBoardDimension messenger)
                                                 (invalidBoardDimension messenger)
                                                 Core.Validation.isValidBoardDimension

firstPlayer = Player { marker = 'x', isAI = False }
secondPlayer = Player { marker = 'o', isAI = True }

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

