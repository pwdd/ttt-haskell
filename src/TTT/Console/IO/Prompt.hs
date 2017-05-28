module TTT.Console.IO.Prompt (getInput, getSettings) where

import TTT.Core.Types

import TTT.Console.Utils.Helpers as Helpers (isNumber)

getInput :: IO String -> (String -> IO ()) -> String -> IO String
getInput reader printer message = do
  printer message
  reader

getSettings :: IO String -> (String -> IO ()) -> String -> String -> (Int -> Bool) -> IO Int
getSettings reader printer askMessage warnMessage validation = do
  input <- getInput reader printer askMessage
  if Helpers.isNumber input && validation (read input)
     then return (read input)
      else do
        printer warnMessage
        getSettings reader printer askMessage warnMessage validation

