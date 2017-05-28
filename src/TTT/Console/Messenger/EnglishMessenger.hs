module TTT.Console.Messenger.EnglishMessenger ( chooseANumber'
                                              , invalidMove'
                                              , currentPlayerIs'
                                              , draw'
                                              , winner'
                                              , askBoardDimension'
                                              , invalidBoardDimension'
                                              ) where

import Data.List as List

import TTT.Core.Types

import TTT.Core.Board as Board (dimension)
import TTT.Console.Utils.Helpers as Console.Helpers (markerToStr)

chooseANumber' :: Board -> String
chooseANumber' board = "\nPlease enter a number from 1 to " ++ (show (length board))  ++ ":\n"

invalidMove' = "\nYour choice is not valid. \n"

currentPlayerIs' :: Marker -> String
currentPlayerIs' marker = "\nCurrent player is '" ++ [marker] ++ "'\n"

draw' = "\nThe game tied\n"

posToStr :: [Int] -> String
posToStr indices = show (map succ indices) :: String

winner' :: Marker -> [Int] -> String
winner' marker indices =
  "\nPlayer" ++ Console.Helpers.markerToStr marker ++ "won on positions " ++ posToStr indices ++ "\n\n"

askBoardDimension' = "\nEnter the dimension of the board: \n3 - for 3x3\n4 - for 4x4\n"
invalidBoardDimension' = "\nBoard dimension not available\n"
