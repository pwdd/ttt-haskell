module TTT.Console.Settings ( getMessengerNumber
                            , createMessenger
                            , getBoardDimension
                            , getFirstPlayerMarker
                            , getSecondPlayerMarker
                            , getFirstPlayerRole
                            , getSecondPlayerRole
                            , createPlayer
                            , englishMessenger
                            , portugueseMessenger
                            ) where

import qualified Data.Map.Strict as Map

import TTT.Core.Types
import TTT.Core.Players.Player
import TTT.Core.Validation as Core.Validation ( playerRoleOptions
                                              , isValidBoardDimension
                                              , isValidPlayerRole
                                              )
import TTT.Messenger.Messenger
import TTT.Messenger.EnglishMessenger as EnglishMessenger
import TTT.Messenger.PortugueseMessenger as PortugueseMessenger
import TTT.Messenger.Validation as Messenger.Validation
import TTT.Console.IO.IOContext
import qualified TTT.Console.IO.IO as TTT.IO (printer, reader, clearScreen)
import TTT.Console.Players.Validation as Console.Players.Validation (isValidMarker)
import TTT.Console.IO.Prompt as Prompt ( getSettings
                                       , getSettingsFromOptions
                                       , getMarker
                                       )

askMessenger = "\nEnter 1 for English\nDigite 2 para Português\n"
invalidMessenger = ""

messengerOptions :: Map.Map String Int
messengerOptions = Map.fromList [("English", 1), ("Portuguese", 2)]

getMessengerNumber :: IO Int
getMessengerNumber =
  Prompt.getSettingsFromOptions TTT.IO.reader
                                TTT.IO.printer
                                askMessenger
                                invalidMessenger
                                Messenger.Validation.isValidMessenger
                                messengerOptions

createMessenger :: Int -> Messenger
createMessenger messengerNumber
  | messengerNumber == messengerOptions Map.! "Portuguese" = portugueseMessenger
  | otherwise = englishMessenger

getBoardDimension :: Messenger -> IO Int
getBoardDimension messenger = Prompt.getSettings TTT.IO.reader
                                                 TTT.IO.printer
                                                 (askBoardDimension messenger)
                                                 (invalidBoardDimension messenger)
                                                 Core.Validation.isValidBoardDimension

getFirstPlayerMarker :: Messenger -> Marker -> IO Marker
getFirstPlayerMarker messenger =
  getPlayerMarker (askFirstPlayerMarker messenger)
                  (invalidPlayerMarker messenger)

getSecondPlayerMarker :: Messenger -> Marker -> IO Marker
getSecondPlayerMarker messenger =
  getPlayerMarker (askSecondPlayerMarker messenger)
                  (invalidPlayerMarker messenger)

getPlayerMarker :: String -> String -> Marker -> IO Marker
getPlayerMarker askMessage warnMessage =
  Prompt.getMarker TTT.IO.reader
                   TTT.IO.printer
                   askMessage
                   warnMessage
                   Console.Players.Validation.isValidMarker

getFirstPlayerRole :: Messenger -> IO Int
getFirstPlayerRole messenger =
  getPlayerRole messenger (askFirstPlayerRole messenger) (invalidPlayerRole messenger)

getSecondPlayerRole :: Messenger -> IO Int
getSecondPlayerRole messenger =
  getPlayerRole messenger (askSecondPlayerRole messenger) (invalidPlayerRole messenger)

getPlayerRole :: Messenger -> String -> String -> IO Int
getPlayerRole messenger askMessage warnMessage =
  Prompt.getSettingsFromOptions TTT.IO.reader
                                TTT.IO.printer
                                askMessage
                                warnMessage
                                Core.Validation.isValidPlayerRole
                                Core.Validation.playerRoleOptions

createPlayer :: Marker -> Int -> Player
createPlayer marker role =
  Player { marker = marker, isAI = defineAI role }
    where defineAI role = role == Core.Validation.playerRoleOptions Map.! "Computer"

englishMessenger = Messenger { chooseANumber = EnglishMessenger.chooseANumber'
                             , invalidMove = EnglishMessenger.invalidMove'
                             , draw = EnglishMessenger.draw'
                             , winner = EnglishMessenger.winner'
                             , askBoardDimension = EnglishMessenger.askBoardDimension'
                             , invalidBoardDimension = EnglishMessenger.invalidBoardDimension'
                             , askFirstPlayerMarker = EnglishMessenger.askFirstPlayerMarker'
                             , askSecondPlayerMarker = EnglishMessenger.askSecondPlayerMarker'
                             , invalidPlayerMarker = EnglishMessenger.invalidPlayerMarker'
                             , askFirstPlayerRole = EnglishMessenger.askFirstPlayerRole'
                             , askSecondPlayerRole = EnglishMessenger.askSecondPlayerRole'
                             , invalidPlayerRole = EnglishMessenger.invalidPlayerRole'
                             , initialStateString = EnglishMessenger.initialStateString'
                             , finalMessage = EnglishMessenger.finalMessage'
                             , movedTo = EnglishMessenger.movedTo'
                             }

portugueseMessenger = Messenger { chooseANumber = PortugueseMessenger.chooseANumber'
                                , invalidMove = PortugueseMessenger.invalidMove'
                                , draw = PortugueseMessenger.draw'
                                , winner = PortugueseMessenger.winner'
                                , askBoardDimension = PortugueseMessenger.askBoardDimension'
                                , invalidBoardDimension = PortugueseMessenger.invalidBoardDimension'
                                , askFirstPlayerRole = PortugueseMessenger.askFirstPlayerRole'
                                , askSecondPlayerRole = PortugueseMessenger.askSecondPlayerRole'
                                , askFirstPlayerMarker = PortugueseMessenger.askFirstPlayerMarker'
                                , askSecondPlayerMarker = PortugueseMessenger.askSecondPlayerMarker'
                                , invalidPlayerMarker = PortugueseMessenger.invalidPlayerMarker'
                                , invalidPlayerRole = PortugueseMessenger.invalidPlayerRole'
                                , initialStateString = PortugueseMessenger.initialStateString'
                                , finalMessage = PortugueseMessenger.finalMessage'
                                , movedTo = PortugueseMessenger.movedTo'
                                }

