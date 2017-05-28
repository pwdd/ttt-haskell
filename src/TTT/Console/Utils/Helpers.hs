module TTT.Console.Utils.Helpers (markerToStr, isNumber) where

import TTT.Core.Types

markerToStr :: Marker -> String
markerToStr marker = " " ++ [marker]  ++ " "

isNumber :: String -> Bool
isNumber input =
  case reads input :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

