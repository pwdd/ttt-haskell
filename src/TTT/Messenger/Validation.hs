module TTT.Messenger.Validation (messengerOptions, isValidMessenger) where

import qualified Data.Map.Strict as Map

messengerOptions :: Map.Map String Int
messengerOptions = Map.fromList [("English", 1), ("Portuguese", 2)]

isValidMessenger :: Int -> Bool
isValidMessenger input = input `elem` availableMessengers
  where availableMessengers = Map.elems messengerOptions

