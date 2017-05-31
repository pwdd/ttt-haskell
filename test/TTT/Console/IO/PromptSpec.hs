module TTT.Console.IO.PromptSpec (main, spec) where

import Test.Hspec

import Data.Map.Strict as Map

import TTT.Console.IO.Prompt as Prompt
import TTT.Core.Board as Board (newBoard)

main :: IO ()
main = hspec spec

mockReaderInt :: IO String
mockReaderInt = return "0"

mockReaderChar :: IO String
mockReaderChar = return "a"

mockPrinter :: String -> IO ()
mockPrinter s = return ()

mockOptions :: Map.Map String Int
mockOptions = Map.fromList []

spec :: Spec
spec = do
  describe "getInput" $
    it "returns user input" $
      Prompt.getInput mockReaderInt mockPrinter "" `shouldReturn` "0"

  describe "getSettings" $
    it "returns an Int" $
      Prompt.getSettings mockReaderInt mockPrinter "" "" (\someInput -> True) `shouldReturn` 0

  describe "getSettingsFromOptions" $
    it "returns an Int" $
      Prompt.getSettingsFromOptions mockReaderInt
                                    mockPrinter
                                    ""
                                    ""
                                    (\someInput options -> True)
                                    mockOptions
        `shouldReturn` 0

  describe "getMarker" $
    it "returns a Char" $
      Prompt.getMarker mockReaderChar
                       mockPrinter
                       ""
                       ""
                       (\someInput options -> True)
                       'b'
        `shouldReturn` 'a'

