module TTT.Console.Utils.Helpers (isNumber) where

isNumber :: String -> Bool
isNumber input =
  case reads input :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

