module TTT.Console.IO.Prompt ( getInput
                             , getSettings
                             , isNumber
                             , askMessenger
                             , invalidMessenger
                             ) where

import TTT.Core.Types

import TTT.Console.Utils.Helpers as Helpers (isNumber)

askMessenger = "\nEnter 1 for English\nDigite 2 para Português\n"
invalidMessenger = "\nPlease enter the number 1 for English\nPor favor, digite o número 2 para Português"

getInput :: IO String -> (String -> IO ()) ->String -> IO String
getInput reader printer message = do
  printer message
  reader

getSettings :: IO String -> (String -> IO ()) -> String -> String -> (Int -> Bool) -> IO Int
getSettings reader printer askMessage warnMessage validation = do
  input <- getInput reader printer askMessage
  if Helpers.isNumber input && validation (read input :: Int)
     then return (read input :: Int)
      else do
        printer warnMessage
        getSettings reader printer askMessage warnMessage validation

