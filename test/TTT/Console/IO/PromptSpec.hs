module TTT.Console.IO.PromptSpec (main, spec) where

import Test.Hspec

import Data.Map.Strict as Map

import TTT.Console.IO.Prompt as Prompt
import TTT.Core.Board as Board (newBoard)

main :: IO ()
main = hspec spec

mockReader :: IO String
mockReader = return "0"

mockPrinter :: String -> IO ()
mockPrinter s = return ()

mockOptions :: Map.Map String Int
mockOptions = Map.fromList []

spec :: Spec
spec = do
  describe "getInput" $
    it "returns user input" $
      Prompt.getInput mockReader mockPrinter "" `shouldReturn` "0"

  describe "getSettings" $ do
    it "returns an Int" $
      Prompt.getSettings mockReader mockPrinter "" "" (\someInput -> True) `shouldReturn` 0

  describe "getSettingsFromOptions" $
    it "returns an Int" $
      Prompt.getSettingsFromOptions mockReader
                                    mockPrinter
                                    ""
                                    ""
                                    (\someInput options -> True)
                                    mockOptions
        `shouldReturn` 0

