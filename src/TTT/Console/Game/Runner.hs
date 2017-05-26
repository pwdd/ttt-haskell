module TTT.Console.Game.Runner (play) where

import TTT.Console.Game.Loop as Game.Loop (loop)
import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.Settings as Settings (ioContext, gameContext)

play :: IO ()
play = Game.Loop.loop Settings.ioContext Settings.gameContext

