module TTT.Console.Players.Prompt ( getInput
                                  , getSpot
                                  , getBoardDimension
                                  , getMessenger
                                  , isNumber
                                  , askMessenger
                                  , invalidMessenger
                                  ) where

import TTT.Core.Types

import TTT.Console.IO.IO as TTT.IO (printer)
import TTT.Core.Validation as Validation (isValidMove, isValidBoardDimension)
import TTT.Console.Messenger.Validation as MessengerValidation (isValidMessenger)

askMessenger = "\nEnter 1 for English\nDigite 2 para Português\n"
invalidMessenger = "\nPlease enter the number 1 for English\nPor favor, digite o número 2 para Português"

getInput :: IO String -> String -> IO String
getInput reader message = do
  TTT.IO.printer message
  reader

getMessenger :: IO String -> String -> String -> IO Int
getMessenger reader askMessage warnMessage = do
  input <- getInput reader askMessage
  if isNumber input && MessengerValidation.isValidMessenger (read input :: Int)
     then return (read input :: Int)
      else do
        printer warnMessage
        getMessenger reader askMessage warnMessage

getBoardDimension :: IO String -> String -> String -> IO Int
getBoardDimension reader askMessage warnMessage = do
  input <- getInput reader askMessage
  if isNumber input && Validation.isValidBoardDimension (read input :: Int)
     then return (read input :: Int)
      else do
        printer warnMessage
        getBoardDimension reader askMessage warnMessage

getSpot :: IO String -> String -> String -> Board -> IO Int
getSpot reader askMessage warnMessage board = do
  input <- getInput reader askMessage
  if isNumber input && Validation.isValidMove board (read input - 1)
     then return (inputToNumber input)
     else do
       printer warnMessage
       getSpot reader askMessage warnMessage board

isNumber :: String -> Bool
isNumber input =
  case reads input :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

inputToNumber :: (Num a, Read a) => String -> a
inputToNumber input = read input -1

