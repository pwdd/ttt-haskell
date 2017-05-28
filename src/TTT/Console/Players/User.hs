module TTT.Console.Players.User (getSpot) where

import TTT.Core.Types
import TTT.Core.Validation as Validation (isValidMove)

import TTT.Console.IO.Prompt as Prompt (getInput)
import TTT.Console.Utils.Helpers as Helpers (isNumber)

getSpot :: IO String -> (String -> IO ()) -> String -> String -> Board -> IO Int
getSpot reader printer askMessage warnMessage board = do
  input <- Prompt.getInput reader printer askMessage
  if Helpers.isNumber input && Validation.isValidMove board (read input - 1)
     then return (inputToNumber input)
     else do
       printer warnMessage
       getSpot reader printer askMessage warnMessage board

inputToNumber :: (Num a, Read a) => String -> a
inputToNumber input = read input -1

