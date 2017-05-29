module TTT.Messenger.Validation (isValidMessenger) where

import qualified Data.Map.Strict as Map

isValidMessenger :: Int -> Map.Map String Int -> Bool
isValidMessenger input validOptions = input `elem` availableMessengers
  where availableMessengers = Map.elems validOptions

