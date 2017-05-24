module TTT.Messenger.EnglishMessenger (
                                        chooseANumber'
                                      , invalidMove'
                                      , currentPlayerIs'
                                      , draw'
                                      , winner'
                                      , strBoard'
                                      ) where

import Data.List as List

import Types
import TTT.Board as Board
import TTT.Utils.Helpers as Helpers

chooseANumber' = "\nPlease enter a number from 1 to 9:\n"

invalidMove' = "\nYour choice is not valid. \n" ++ chooseANumber'

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

strBoard' :: Board -> String
strBoard' board = addSeparator (addPipe $ chunked board) where

  chunked board = Helpers.chunks (Board.dimension board) $ markerToStr <$> board

  addPipe = map $ intercalate "|"

  addSeparator piped = do
    let separator = "\n---|---|---\n"
    intercalate separator piped ++ "\n\n"

