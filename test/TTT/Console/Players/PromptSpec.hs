module TTT.Console.Players.PromptSpec (main, spec) where

import Test.Hspec

import TTT.Console.Players.Prompt as Prompt
import TTT.Core.Board as Board (newBoard)

main :: IO ()
main = hspec spec

mockReader = return

spec :: Spec
spec = do
  describe "getInput" $
    it "returns user input" $
      Prompt.getInput (mockReader "0") "" `shouldReturn` "0"

  describe "getSpot" $
    it "returns valid spot if input is valid" $
      Prompt.getSpot (mockReader "5") "" "" (Board.newBoard 9) `shouldReturn` 4

