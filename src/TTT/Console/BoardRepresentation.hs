module TTT.Console.BoardRepresentation (strBoard) where

import Data.List

import TTT.Core.Types
import TTT.Core.Board as Board
import TTT.Core.Utils.Helpers as Core.Helpers (chunks)
import TTT.Messenger.Utils.Helpers as Messenger.Helpers (markerToStr)

posToStr :: [Int] -> String
posToStr indices = show (succ <$> indices) :: String

makeRowSeparator :: Int -> String
makeRowSeparator boardDimension = "\n" ++ intercalate "|" (replicate boardDimension "---") ++"\n"

strBoard :: Board -> String
strBoard board = addSeparator (addPipe $ chunked board) where

  chunked board = Core.Helpers.chunks (Board.dimension board) $
    Messenger.Helpers.markerToStr <$> board

  addPipe = map $ intercalate "|"

  addSeparator piped = do
    let separator = makeRowSeparator $ Board.dimension board
    intercalate separator piped ++ "\n\n"

