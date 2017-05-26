module TTT.Core.Game.GameContext (GameContext(..)) where

import TTT.Core.Types
import TTT.Core.Players.Player

data GameContext = GameContext { board :: Board
                               , currentPlayer :: Player
                               , opponent :: Player
                               , depth :: Int
                               }

