module TTT.Messenger.EnglishMessenger ( chooseANumber'
                                      , invalidMove'
                                      , currentPlayerIs'
                                      , draw'
                                      , winner'
                                      , askBoardDimension'
                                      , invalidBoardDimension'
                                      , askFirstPlayerRole'
                                      , askSecondPlayerRole'
                                      , invalidPlayerRole'
                                      , initialStateString'
                                      , finalMessage'
                                      ) where

import Data.List as List

import TTT.Core.Types

import TTT.Messenger.Utils.Helpers as Console.Helpers (markerToStr)

chooseANumber' :: Board -> String
chooseANumber' board = "\nPlease enter a number from 1 to " ++ show (length board)  ++ ":\n"

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

roleOptions = "\n1 - Human\n2 - Unbeatable Computer"

askFirstPlayerRole' = "\nIs the first player:" ++ roleOptions
askSecondPlayerRole' = "\nIs the second player:" ++ roleOptions
invalidPlayerRole' = "\nThis type of player is not available.\n"

initialStateString' :: Bool -> String -> String
initialStateString' isEmpty strBoard
  | isEmpty = "\n" ++ strBoard
  | otherwise = ""

finalMessage' :: Marker -> Bool -> [Int] -> String
finalMessage' currentPlayerMarker isDraw winningCombo
  | isDraw = draw'
  | otherwise = winner' currentPlayerMarker winningCombo
