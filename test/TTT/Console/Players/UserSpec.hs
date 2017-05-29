module TTT.Console.Players.UserSpec (main, spec) where

import Test.Hspec

import TTT.Core.Board as Board (emptySpot, newBoard)
import TTT.Console.Players.User as User

main :: IO ()
main = hspec spec

mockReader :: IO String
mockReader = return "2"

mockPrinter :: String -> IO ()
mockPrinter s = return ()

spec :: Spec
spec =
  describe "getSpot" $
    it "returns a valid index on the board" $
      User.getSpot mockReader mockPrinter "" "" (Board.newBoard 3) `shouldReturn` 1

