module TTT.PromptSpec (main, spec) where

import Test.Hspec

import TTT.Prompt as Prompt
import TTT.Board as Board (newBoard)

main :: IO ()
main = hspec spec

mockReadln = return

spec :: Spec
spec = do
  describe "getInput" $
    it "returns user input" $
      Prompt.getInput (mockReadln "0") "" `shouldReturn` "0"

  describe "getSpot" $
    it "returns valid spot if input is valid" $
      Prompt.getSpot (mockReadln "5") "" (Board.newBoard 9) `shouldReturn` 4

