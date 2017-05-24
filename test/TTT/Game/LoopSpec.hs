module TTT.Game.LoopSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (generate, elements)

import Types
import TTT.Board as Board
import TTT.Game.Context
import TTT.Game.Loop as Game.Loop
import TTT.Messenger.Messenger
import TTT.Players.Player

main :: IO ()
main = hspec spec

fakeCurrentPlayerMessage :: Marker -> String
fakeCurrentPlayerMessage s = ""

fakeWinner :: Marker -> [Int] -> String
fakeWinner m i = ""

fakeStrBoard :: Board -> String
fakeStrBoard b = ""

fakeMessenger = Messenger {
                            chooseANumber = ""
                          , invalidMove = ""
                          , currentPlayerIs = fakeCurrentPlayerMessage
                          , draw = ""
                          , winner = fakeWinner
                          , strBoard = fakeStrBoard
                          }

indices = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

mockReader :: IO String
mockReader = (generate . elements) indices

testBoard = Board.newBoard 9
testCurrentPlayer = Player { marker = 'x', isAI = False }
testOpponent = Player { marker = 'o', isAI = False }

testContext = Context {
                        board = testBoard
                      , currentPlayer = testCurrentPlayer
                      , opponent = testOpponent
                      , messenger = fakeMessenger
                      , reader = mockReader
                      , printer = putStr
                      }

spec :: Spec
spec =
  describe "loop" $
    it "runs the game" $
      Game.Loop.loop testContext `shouldReturn` ()

