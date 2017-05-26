module TTT.Core.Players.Computer.NegamaxSpec where

import Test.Hspec

import TTT.Core.Game.GameContext
import TTT.Core.Players.Player
import TTT.Core.Players.Computer.Negamax as Negamax
import TTT.Core.Board as Board (emptySpot)

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

  describe "boardAnalysis" $ do
    it "is returns 100 if current player wins" $ do
      let board = [ x, x, x
                  , o, o, x
                  , o, x, o
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Negamax.boardAnalysis context `shouldBe` 100

    it "is returns -100 if opponent wins" $ do
      let board = [ x, x, x
                  , o, o, x
                  , o, x, o
                  ]
      let context = GameContext { board = board, currentPlayer = playerO, opponent = playerX, depth = 0 }
      Negamax.boardAnalysis context `shouldBe` (-100)

    it "is returns 0 if games ends in tie" $ do
      let board = [ x, x, o
                  , o, o, x
                  , x, o, x
                  ]
      let context = GameContext { board = board, currentPlayer = playerO, opponent = playerX, depth = 0 }
      Negamax.boardAnalysis context `shouldBe` 0

  describe "getSpot" $ do
    it "blocks opponent from winning" $ do
      let board = [ o, o, e
                  , x, e, e
                  , x, e, e
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Negamax.getSpot context `shouldBe` 2

    it "wins when it has the chance" $ do
      let board = [ o, o, e
                  , x, x, e
                  , e, e, e
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      Negamax.getSpot context `shouldBe` 5

    it "does not let opponent to create a fork" $ do
      let board = [ x, e, e
                  , e, o, e
                  , e, e, o
                  ]
      let context = GameContext { board = board, currentPlayer = playerX, opponent = playerO, depth = 0 }
      let result = Negamax.getSpot context
      result `elem` [2, 6] `shouldBe` True

