module TTT.Core.Players.Computer.HardComputerSpec where

import Test.Hspec

import TTT.Core.Game.GameContext
import TTT.Core.Board as Board (emptySpot, newBoard)
import TTT.Core.Players.Player
import TTT.Core.Players.Computer.Negamax as Negamax (startDepth)
import TTT.Core.Players.Computer.HardComputer as Computer

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let x = 'x'
  let o = 'o'
  let e = Board.emptySpot
  let depth = Negamax.startDepth
  let playerX = Player { marker = x, isAI = True }
  let playerO = Player { marker = o, isAI = True }

  describe "getSpot" $ do
    it "blocks opponent from winning" $ do
      let board = [ o, o, e
                  , x, e, e
                  , x, e, e
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Computer.getSpot context `shouldBe` 2

    it "wins when it has the chance" $ do
      let board = [ o, o, e
                  , x, x, e
                  , e, e, e
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Computer.getSpot context `shouldBe` 5

    it "does not let opponent to create a fork" $ do
      let board = [ x, e, e
                  , e, o, e
                  , e, e, o
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      let result = Computer.getSpot context
      result `elem` [2, 6] `shouldBe` True

    it "places a marker in the middle if computer starts the game in a 3x3 board" $ do
      let board = Board.newBoard 9
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Computer.getSpot context `shouldBe` 4

    it "places a marker close to the middle if computer starts the game in a 4x4 board" $ do
      let board = Board.newBoard 16
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Computer.getSpot context `shouldBe` 6

