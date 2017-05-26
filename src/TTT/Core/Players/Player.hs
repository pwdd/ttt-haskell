module TTT.Core.Players.Player (Player(..)) where

import TTT.Core.Types

data Player = Player { marker :: Marker
                     , isAI :: Bool
                     }

