module TTT.Console.Settings ( firstPlayer
                            , secondPlayer
                            ) where

import TTT.Core.Types
import TTT.Core.Game.GameContext

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Messenger
import TTT.Core.Players.Player
import TTT.Core.Players.Computer.Negamax as Negamax

import TTT.Console.IO.IO as TTT.IO (printer, reader)
import TTT.Console.IO.IOContext as IOContext
import TTT.Console.Messenger.EnglishMessenger as EnglishMessenger
import TTT.Console.Players.Prompt as Prompt

firstPlayerMarker = 'x'
secondPlayerMarker = 'o'

firstPlayer = Player { marker = firstPlayerMarker, isAI = False }
secondPlayer = Player { marker = secondPlayerMarker, isAI = True  }

