module TTT.Console.Messenger.EnglishMessenger ( chooseANumber'
                                              , invalidMove'
                                              , currentPlayerIs'
                                              , draw'
                                              , winner'
                                              , strBoard'
                                              ) where

import Data.List as List

import TTT.Core.Types

import TTT.Core.Board as Board (dimension)
import TTT.Core.Utils.Helpers as Helpers (chunks)

chooseANumber' = "\nPlease enter a number from 1 to 9:\n"

invalidMove' = "\nYour choice is not valid. \n"

currentPlayerIs' :: Marker -> String
currentPlayerIs' marker = "\nCurrent player is '" ++ [marker] ++ "'\n"

draw' = "\nThe game tied\n"

markerToStr :: Marker -> String
markerToStr marker = " " ++ [marker]  ++ " "

posToStr :: [Int] -> String
posToStr indices = show (map succ indices) :: String

winner' :: Marker -> [Int] -> String
winner' marker indices =
  "\nPlayer" ++ markerToStr marker ++ "won on positions " ++ posToStr indices ++ "\n\n"

makeRowSeparator :: Int -> String
makeRowSeparator boardDimension = "\n" ++ intercalate "|" (replicate boardDimension "---") ++"\n"

strBoard' :: Board -> String
strBoard' board = addSeparator (addPipe $ chunked board) where

  chunked board = Helpers.chunks (Board.dimension board) $ markerToStr <$> board

  addPipe = map $ intercalate "|"

  addSeparator piped = do
    let separator = makeRowSeparator $ Board.dimension board
    intercalate separator piped ++ "\n\n"

