module TTT.Console.Game.LoopSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (generate, elements)

import TTT.Core.Types

import TTT.Core.Board as Board (newBoard)
import TTT.Core.Game.GameContext
import TTT.Core.Players.Player

import TTT.Console.Game.Loop as Game.Loop
import TTT.Console.IO.IOContext
import TTT.Messenger.Messenger

main :: IO ()
main = hspec spec

fakeCurrentPlayerMessage :: Marker -> String
fakeCurrentPlayerMessage s = ""

fakeWinner :: Marker -> [Int] -> String
fakeWinner m i = ""

fakeStrBoard :: Board -> String
fakeStrBoard b = ""

fakeMessenger = Messenger { chooseANumber = \fakeBoard -> ""
                          , invalidMove = ""
                          , draw = ""
                          , winner = fakeWinner
                          , askBoardDimension = ""
                          , invalidBoardDimension = ""
                          , askFirstPlayerMarker = ""
                          , askSecondPlayerMarker = ""
                          , invalidPlayerMarker = ""
                          , askFirstPlayerRole = ""
                          , askSecondPlayerRole = ""
                          , invalidPlayerRole = ""
                          , initialStateString = \a b -> ""
                          , finalMessage = \a b c -> ""
                          , movedTo = \a b -> ""
                          }

indices = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

mockReader :: IO String
mockReader = (generate . elements) indices

mockPrinter :: String -> IO ()
mockPrinter s = return ()

testBoard = Board.newBoard 9
testCurrentPlayer = Player { marker = 'x', isAI = False }
testOpponent = Player { marker = 'o', isAI = False }
testAI = Player { marker = 'o', isAI = True }

testHumanXHumanContext = GameContext { board = testBoard
                                     , currentPlayer = testCurrentPlayer
                                     , opponent = testOpponent
                                     , depth = 0
                                     }

testHumanXComputerContext = GameContext { board = testBoard
                                        , currentPlayer = testCurrentPlayer
                                        , opponent = testAI
                                        , depth = 0
                                        }

mockClear :: Int -> IO ()
mockClear a = return ()

mockIOContext = IOContext { printer = mockPrinter
                          , reader = mockReader
                          , clear = mockClear
                          , messenger = fakeMessenger
                          }
spec :: Spec
spec =
  describe "loop" $ do
    it "runs the game when both players are humans" $
      Game.Loop.loop mockIOContext testHumanXHumanContext `shouldReturn` ()

    it "runs the game when it is human vs computer" $
      Game.Loop.loop mockIOContext testHumanXComputerContext `shouldReturn` ()

