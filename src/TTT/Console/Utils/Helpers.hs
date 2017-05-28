module TTT.Console.Utils.Helpers (markerToStr) where

import TTT.Core.Types

markerToStr :: Marker -> String
markerToStr marker = " " ++ [marker]  ++ " "

