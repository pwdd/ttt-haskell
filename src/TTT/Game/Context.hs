module TTT.Game.Context (Context(..)) where

import Types
import TTT.Players.Player
import TTT.Messenger.Messenger

data Context = Context {
                         board :: Board
                       , currentPlayer :: Player
                       , opponent :: Player
                       , messenger :: Messenger
                       , reader :: IO String
                       , printer :: String -> IO ()
                       }

