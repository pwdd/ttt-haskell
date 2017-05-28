module TTT.Core.Messenger (Messenger(..)) where

import TTT.Core.Types

data Messenger = Messenger { chooseANumber :: Board -> String
                           , invalidMove :: String
                           , currentPlayerIs :: Marker -> String
                           , draw :: String
                           , winner :: Marker -> [Int] -> String
                           , askBoardDimension :: String
                           , invalidBoardDimension :: String
                           }

