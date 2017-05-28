module TTT.Console.Messenger.Validation (isValidMessenger) where

import TTT.Console.Settings as Settings (messengerOptions)

isValidMessenger :: Int -> Bool
isValidMessenger input = input `elem` Settings.messengerOptions

