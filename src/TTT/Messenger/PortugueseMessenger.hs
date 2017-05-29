module TTT.Messenger.PortugueseMessenger ( chooseANumber'
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
chooseANumber' board = "\nEntre um número de 1 a " ++ show (length board)  ++ ":\n"

invalidMove' = "\nEscolha inválida. \n"

currentPlayerIs' :: Marker -> String
currentPlayerIs' marker = "\nÉ a vez do jogador '" ++ [marker] ++ "'\n"

draw' = "\nEmpate!\n"

posToStr :: [Int] -> String
posToStr indices = show (map succ indices) :: String

winner' :: Marker -> [Int] -> String
winner' marker indices =
  "\nO jogador" ++ Console.Helpers.markerToStr marker ++ "venceu nas casas " ++ posToStr indices ++ "\n\n"

askBoardDimension' = "\nDigite a dimensão do tabuleiro: \n3 - for 3x3\n4 - for 4x4\n"
invalidBoardDimension' = "\nNão existe tabuleiro nesse tamanho\n"

roleOptions = "\n1 - Humano\n2 - Computador Imbatível\n"

askFirstPlayerRole' = "\nO primeiro jogador é:" ++ roleOptions
askSecondPlayerRole' = "\nO segundo jogador é:"
invalidPlayerRole' = "\nEsse tipo de jogador não está disponível\n"

initialStateString' :: Bool -> String -> String
initialStateString' isEmpty strBoard
  | isEmpty = "\n" ++ strBoard
  | otherwise = ""

finalMessage' :: Marker -> Bool -> [Int] -> String
finalMessage' currentPlayerMarker isDraw winningCombo
  | isDraw = draw'
  | otherwise = winner' currentPlayerMarker winningCombo

