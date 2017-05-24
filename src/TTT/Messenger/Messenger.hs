module TTT.Messenger.Messenger (Messenger(..)) where

import Types

data Messenger = Messenger {
                              chooseANumber :: String
                            , invalidMove :: String
                            , currentPlayerIs :: Marker -> String
                            , draw :: String
                            , winner :: Marker -> [Int] -> String
                            , strBoard :: Board -> String
                            }

