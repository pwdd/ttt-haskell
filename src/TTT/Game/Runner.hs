module TTT.Game.Runner (play) where

import TTT.Game.Loop as Game.Loop
import TTT.Settings as Settings

play = Game.Loop.loop Settings.startContext

