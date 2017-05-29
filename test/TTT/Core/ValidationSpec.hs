module TTT.Core.ValidationSpec (main, spec) where

import Test.Hspec

import Data.Map.Strict as Map

import TTT.Core.Board as Board (emptySpot, newBoard)
import TTT.Core.Validation as Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "isValidMove" $ do
    let x = 'x'
    let o = 'o'
    let e = Board.emptySpot
    let newBoard = Board.newBoard 3
    let board = [ x, x, e
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

  describe "isValidBoardDimension" $ do
    it "is true for 3 and 4" $
      Validation.isValidBoardDimension 3 && Validation.isValidBoardDimension 4 `shouldBe` True

    it "is false for any other number" $
      Validation.isValidBoardDimension 42 `shouldBe` False

  describe "isValidPlayerRole" $ do
    let fakePlayerRoleOptions = Map.fromList [("RoleOne", 1), ("RoleTwo", 2)]

    it "is true if arg is a value on playerRoleOptions" $
      Validation.isValidPlayerRole 1 fakePlayerRoleOptions `shouldBe` True

    it "is false if arg is not a value on playerRoleOptions" $
      Validation.isValidPlayerRole 3 fakePlayerRoleOptions `shouldBe` False

