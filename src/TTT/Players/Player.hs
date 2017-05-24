module TTT.Players.Player (Player(..)) where

import Types

data Player = Player { marker :: Marker, isAI :: Bool } deriving Show

