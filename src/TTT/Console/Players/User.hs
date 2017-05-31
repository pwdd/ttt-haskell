module TTT.Console.Players.User (getSpot) where

import TTT.Core.Types
import TTT.Core.Validation as Validation (isValidMove)

import TTT.Console.BoardRepresentation as BoardRepresentation (strBoard)
import TTT.Console.IO.Prompt as Prompt (getInput)
import TTT.Console.Utils.Helpers as Helpers (isNumber)

getSpot :: IO String -> (String -> IO ()) -> String -> String -> Board -> IO Int
getSpot reader printer askMessage warnMessage board = do
  input <- Prompt.getInput reader printer askMessage
  if Helpers.isNumber input && Validation.isValidMove board (inputToNumber input)
     then return (inputToNumber input)
     else do
       printer warnMessage
       printer $ BoardRepresentation.strBoard board
       getSpot reader printer askMessage warnMessage board

inputToNumber :: String -> Int
inputToNumber input = read input - 1

