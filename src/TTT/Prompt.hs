module TTT.Prompt (
                    getInput
                  , getSpot
                  , isNumber
                  ) where

import Types
import TTT.IO as TTT.IO
import TTT.Validation as Validation

getInput :: IO String -> String -> IO String
getInput readln message = do
  TTT.IO.println message
  readln

getSpot :: (Read b, Num b) => IO String -> String -> Board -> IO b
getSpot readln askMessage board = do
  input <- getInput readln askMessage
  if isNumber input && Validation.isValidMove board (read input - 1)
     then return (inputToNumber input)
     else getSpot readln askMessage board

isNumber :: String -> Bool
isNumber input =
  case reads input :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

inputToNumber :: (Num a, Read a) => String -> a
inputToNumber input = read input -1

