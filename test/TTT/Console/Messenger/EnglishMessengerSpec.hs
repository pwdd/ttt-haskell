module TTT.Console.Messenger.EnglishMessengerSpec (main, spec) where

import Test.Hspec

import TTT.Console.Messenger.EnglishMessenger as Messenger (strBoard')
import TTT.Core.Board as Board (emptySpot, newBoard)

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
      Messenger.strBoard' board `shouldBe`
        "   |   |   \n---|---|---\n   |   |   \n---|---|---\n   |   |   \n\n"

    it "is a visual representation of any board" $ do
      let board = [
                    x, o, e
                  , x, e, o
                  , e, x, x
                  ]
      Messenger.strBoard' board `shouldBe`
        " x | o |   \n---|---|---\n x |   | o \n---|---|---\n   | x | x \n\n"

