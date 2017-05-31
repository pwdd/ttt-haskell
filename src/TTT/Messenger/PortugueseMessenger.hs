module TTT.Messenger.PortugueseMessenger ( chooseANumber'
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
chooseANumber' board = "\nEntre um número de 1 a " ++ show (length board)  ++ ":\n"

invalidMove' = "\nEscolha inválida. \n\n"

draw' = "\nEmpate!\n"

posToStr :: [Int] -> String
posToStr indices = show (map succ indices) :: String

winner' :: Marker -> [Int] -> String
winner' marker indices =
  "\nO jogador" ++ Console.Helpers.markerToStr marker ++ "venceu nas casas " ++ posToStr indices ++ "\n\n"

askBoardDimension' = "\nDigite a dimensão do tabuleiro: \n3 - para 3x3\n4 - para 4x4\n"
invalidBoardDimension' = "\nNão existe tabuleiro nesse tamanho\n"

roleOptions = "\n1 - Humano\n2 - Computador Imbatível\n"

askFirstPlayerRole' = "\nO primeiro jogador é:" ++ roleOptions
askSecondPlayerRole' = "\nO segundo jogador é:" ++ roleOptions
invalidPlayerRole' = "\nEsse tipo de jogador não está disponível\n"

askFirstPlayerMarker' = "\nDigite a letra que vai servir the símbolo do primeiro jogador: \n"
askSecondPlayerMarker' = "\nDigite a letra que vai servir the símbolo do segundo jogador: \n"
invalidPlayerMarker' = "\nSua escolha não é válida\n"

initialStateString' :: Bool -> String -> String
initialStateString' isEmpty strBoard
  | isEmpty = "\n" ++ strBoard
  | otherwise = ""

finalMessage' :: Marker -> Bool -> [Int] -> String
finalMessage' currentPlayerMarker isDraw winningCombo
  | isDraw = draw'
  | otherwise = winner' currentPlayerMarker winningCombo

movedTo' :: Marker -> Int -> String
movedTo' marker spot = "O jogador " ++ [marker] ++ " moveu para a casa " ++ show (spot + 1) ++ "\n\n"

