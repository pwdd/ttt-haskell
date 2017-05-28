module TTT.Console.BoardRepresentationSpec (main, spec) where

import Test.Hspec

import TTT.Core.Board as Board (emptySpot, newBoard)
import TTT.Console.BoardRepresentation as BoardRepresentation

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "strBoard" $ do
    let e = Board.emptySpot
    let x = 'x'
    let o = 'o'

    it "represents an empty board" $ do
      let board = Board.newBoard 9
      BoardRepresentation.strBoard board `shouldBe`
        "   |   |   \n---|---|---\n   |   |   \n---|---|---\n   |   |   \n\n"

    it "is a visual representation of any board" $ do
      let board = [
                    x, o, e, e
                  , x, e, o, e
                  , e, x, x, e
                  , x, x, x, x
                  ]
      BoardRepresentation.strBoard board `shouldBe`
        " x | o |   |   \n---|---|---|---\n x |   | o |   \n---|---|---|---\n   | x | x |   \n---|---|---|---\n x | x | x | x \n\n"

