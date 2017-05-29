module TTT.Console.IO.Prompt ( getInput
                             , getSettings
                             , getSettingsFromOptions
                             ) where

import System.IO
import Data.Map.Strict as Map

import TTT.Core.Types

import TTT.Console.Utils.Helpers as Helpers (isNumber)

getInput :: IO String -> (String -> IO ()) -> String -> IO String
getInput reader printer message = do
  printer message
  hFlush stdout
  reader

getSettings :: IO String -> (String -> IO ()) -> String -> String -> (Int -> Bool) -> IO Int
getSettings reader printer askMessage warnMessage validation = do
  input <- getInput reader printer askMessage
  if Helpers.isNumber input && validation (read input)
     then return (read input)
      else do
        printer warnMessage
        getSettings reader printer askMessage warnMessage validation

getSettingsFromOptions :: IO String ->
  (String ->
    IO ()) ->
      String ->
        String ->
          (Int -> Map.Map String Int-> Bool) ->
            Map.Map String Int ->
              IO Int
getSettingsFromOptions reader printer askMessage warnMessage validation messengerOptions = do
  input <- getInput reader printer askMessage
  if Helpers.isNumber input && validation (read input) messengerOptions
     then return (read input)
      else do
        printer warnMessage
        getSettingsFromOptions reader printer askMessage warnMessage validation messengerOptions

