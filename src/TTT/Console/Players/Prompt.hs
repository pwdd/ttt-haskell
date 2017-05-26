module TTT.Console.Players.Prompt ( getInput
                                  , getSpot
                                  , isNumber
                                  ) where

import TTT.Core.Types

import TTT.Console.IO.IO as TTT.IO (printer)
import TTT.Core.Validation as Validation (isValidMove)

getInput :: IO String -> String -> IO String
getInput reader message = do
  TTT.IO.printer message
  reader

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

