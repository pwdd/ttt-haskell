module TTT.Messenger.Messenger (Messenger(..)) where

import TTT.Core.Types

data Messenger = Messenger { chooseANumber :: Board -> String
                           , invalidMove :: String
                           , currentPlayerIs :: Marker -> String
                           , draw :: String
                           , winner :: Marker -> [Int] -> String
                           , askBoardDimension :: String
                           , invalidBoardDimension :: String
                           , askFirstPlayerRole :: String
                           , askSecondPlayerRole :: String
                           , invalidPlayerRole :: String
                           , initialStateString :: Bool -> String -> String
                           , finalMessage :: Marker -> Bool -> [Int] -> String
                           }

