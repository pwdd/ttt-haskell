module TTT.Messenger.EnglishMessenger ( chooseANumber'
                                      , invalidMove'
                                      , draw'
                                      , winner'
                                      , askBoardDimension'
                                      , invalidBoardDimension'
                                      , askFirstPlayerMarker'
                                      , askSecondPlayerMarker'
                                      , invalidPlayerMarker'
                                      , askFirstPlayerRole'
                                      , askSecondPlayerRole'
                                      , invalidPlayerRole'
                                      , initialStateString'
                                      , finalMessage'
                                      , movedTo'
                                      ) where

import Data.List as List

import TTT.Core.Types

import TTT.Messenger.Utils.Helpers as Console.Helpers (markerToStr)

chooseANumber' :: Board -> String
chooseANumber' board = "\nPlease enter a number from 1 to " ++ show (length board)  ++ ":\n"

invalidMove' = "\nYour choice is not valid. \n\n"

draw' = "\nThe game tied\n"

posToStr :: [Int] -> String
posToStr indices = show (map succ indices) :: String

winner' :: Marker -> [Int] -> String
winner' marker indices =
  "\nPlayer" ++ Console.Helpers.markerToStr marker ++ "won on positions " ++ posToStr indices ++ "\n\n"

askBoardDimension' = "\nEnter the dimension of the board: \n3 - for 3x3\n4 - for 4x4\n"
invalidBoardDimension' = "\nBoard dimension not available\n"

roleOptions = "\n1 - Human\n2 - Unbeatable Computer\n"

askFirstPlayerRole' = "\nIs the first player:" ++ roleOptions
askSecondPlayerRole' = "\nIs the second player:" ++ roleOptions
invalidPlayerRole' = "\nThis type of player is not available.\n"

askFirstPlayerMarker' = "\nEnter the letter that will represent the first player: \n"
askSecondPlayerMarker' = "\nEnter the letter that will represent the second player: \n"
invalidPlayerMarker' = "\nYour choice is not valid\n"

initialStateString' :: Bool -> String -> String
initialStateString' isEmpty strBoard
  | isEmpty = "\n" ++ strBoard
  | otherwise = ""

finalMessage' :: Marker -> Bool -> [Int] -> String
finalMessage' currentPlayerMarker isDraw winningCombo
  | isDraw = draw'
  | otherwise = winner' currentPlayerMarker winningCombo

movedTo' :: Marker -> Int -> String
movedTo' marker spot = "Player " ++ [marker] ++ " moved to " ++ show (spot + 1) ++ "\n\n"

