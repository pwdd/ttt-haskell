module TTT.Messenger.Messenger (Messenger(..)) where

import TTT.Core.Types

data Messenger = Messenger { chooseANumber :: Board -> String
                           , invalidMove :: String
                           , draw :: String
                           , winner :: Marker -> [Int] -> String
                           , askBoardDimension :: String
                           , invalidBoardDimension :: String
                           , askFirstPlayerMarker :: String
                           , askSecondPlayerMarker :: String
                           , invalidPlayerMarker :: String
                           , askFirstPlayerRole :: String
                           , askSecondPlayerRole :: String
                           , invalidPlayerRole :: String
                           , initialStateString :: Bool -> String -> String
                           , finalMessage :: Marker -> Bool -> [Int] -> String
                           , movedTo :: Marker -> Int -> String
                           }

