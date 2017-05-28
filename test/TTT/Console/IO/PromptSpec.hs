module TTT.Console.IO.PromptSpec (main, spec) where

import Test.Hspec

import TTT.Console.IO.Prompt as Prompt
import TTT.Core.Board as Board (newBoard)

main :: IO ()
main = hspec spec

mockReader :: IO String
mockReader = return "0"

mockPrinter :: String -> IO ()
mockPrinter s = return ()

spec :: Spec
spec = do
  describe "getInput" $
    it "returns user input" $
      Prompt.getInput mockReader mockPrinter "" `shouldReturn` "0"

