module TTT.Console.IO.Prompt ( getInput
                             , getSettings
                             , getSettingsFromOptions
                             , getMarker
                             ) where

import System.IO
import Data.Map.Strict as Map

import TTT.Core.Types

import TTT.Console.IO.IO as TTT.IO (clearScreen)
import TTT.Console.Utils.Helpers as Helpers (isNumber)

waitingTime = 0

getInput :: IO a -> (String -> IO ()) -> String -> IO a
getInput reader printer message = do
  printer message
  hFlush stdout
  input <- reader
  TTT.IO.clearScreen waitingTime
  return input

getSettings :: IO String -> (String -> IO ()) -> String -> String -> (Int -> Bool) -> IO Int
getSettings reader printer askMessage warnMessage validation = do
  input <- getInput reader printer askMessage
  if Helpers.isNumber input && validation (read input)
     then return (read input)
      else do
        printer warnMessage
        getSettings reader printer askMessage warnMessage validation

getSettingsFromOptions :: IO String ->
  (String -> IO ()) ->
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

getMarker :: IO String ->
  (String -> IO ()) ->
    String ->
      String ->
        (Marker -> Marker -> Bool) ->
          Marker ->
            IO Marker
getMarker reader printer askMessage warnMessage validation opponentMarker = do
  input <- getInput reader printer askMessage
  if (length input == 1) && validation (head input :: Char)  opponentMarker
     then return (head input :: Char)
      else do
        printer warnMessage
        getMarker reader printer askMessage warnMessage validation opponentMarker

