module TTT.Core.Game.StatusSpec (main, spec) where

import Test.Hspec

import TTT.Core.Game.Status as GameStatus
import TTT.Core.Board as Board (columns, diagonals, emptySpot, newBoard, rows)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let e = Board.emptySpot
  let x = 'x'
  let o = 'o'

  describe "winCombos" $ do
    let combos = GameStatus.winCombos 3

    it "contains rows" $
      combos `shouldContain` Board.rows 3

    it "contains columns" $
      combos `shouldContain` Board.columns 3

    it "contains diagonals" $
      combos `shouldContain` Board.diagonals 3

  describe "winningCombo" $ do

    it "is empty when board has only empty spots" $
      null (GameStatus.winningCombo $ Board.newBoard 3) `shouldBe` True

    it "contains indices of row with equal markers" $ do
      let board = [ e, e, e
                  , x, x, x
                  , e, e, e
                  ]

      GameStatus.winningCombo board `shouldBe` [3, 4, 5]


    it "contains indices of column with equal markers" $ do
      let board = [ e, e, x
                  , e, e, x
                  , e, e, x
                  ]

      GameStatus.winningCombo board `shouldBe` [2, 5, 8]

    it "contains indices of diagonal with equal markers" $ do
      let board = [ e, e, x
                  , e, x, e
                  , x, e, e
                  ]

      GameStatus.winningCombo board `shouldBe` [2, 4, 6]

  describe "isDraw" $ do

    it "is false is board is empty" $ do
      let board = Board.newBoard 3
      GameStatus.isDraw board `shouldBe` False

    it "is false if board is not full" $ do
      let board = [ e, x, x
                  , x, x, x
                  , x, x, x
                  ]

      GameStatus.isDraw board `shouldBe` False

    it "is false if board is full and there is a winner" $ do
      let board = [ x, o, x
                  , o, x, o
                  , o, o, x
                  ]

      GameStatus.isDraw board `shouldBe` False

    it "is false if board is full and there is no winner" $ do
      let board = [ x, o, x
                  , o, x, o
                  , o, x, o
                  ]

      GameStatus.isDraw board `shouldBe` True

  describe "gameOver" $ do

    it "is false if board is empty" $ do
      let board = Board.newBoard 3
      GameStatus.gameOver board `shouldBe` False

    it "is false if board is some spots taken and no winner" $ do
      let board = [ x, o, x
                  , e, e, e
                  , e, e, e
                  ]

      GameStatus.gameOver board `shouldBe` False

    it "is true if there is a draw" $ do
      let board = [ x, o, x
                  , o, x, o
                  , o, x, o
                  ]

      GameStatus.gameOver board `shouldBe` True

    it "is true if there is a winner" $ do
      let board = [ x, e, e
                  , o, x, e
                  , o, e, x
                  ]

      GameStatus.gameOver board `shouldBe` True

  describe "winnerMarker" $ do

    it "is nothing if board is empty" $ do
      let board = Board.newBoard 3
      GameStatus.winnerMarker board `shouldBe` Nothing

    it "is nothing if there is no winner" $ do
      let board = [ x, o, x
                  , o, x, o
                  , o, x, o
                  ]

      GameStatus.winnerMarker board `shouldBe` Nothing

    it "is the marker in the winning positions" $ do
      let board = [ x, o, x
                  , o, x, o
                  , o, o, x
                  ]

      GameStatus.winnerMarker board `shouldBe` Just x

