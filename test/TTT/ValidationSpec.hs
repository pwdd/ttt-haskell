module TTT.ValidationSpec (main, spec) where

import Test.Hspec

import TTT.Board as Board (emptySpot, newBoard)
import TTT.Validation as Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "isValidMove" $ do
    let x = 'x'
    let o = 'o'
    let e = Board.emptySpot
    let newBoard = Board.newBoard 3
    let board = [
                  x, x, e
                , e, o, e
                , e, e, e
                ]

    it "is false if arg is smaller than 0" $
      Validation.isValidMove newBoard (-1) `shouldBe` False

    it "is false if arg is equal or greater than board length" $
      Validation.isValidMove newBoard 9 `shouldBe` False

    it "is false if spot is taken" $
      Validation.isValidMove board 0 `shouldBe` False

    it "is true if spot is available" $
      Validation.isValidMove board 2 `shouldBe` True

