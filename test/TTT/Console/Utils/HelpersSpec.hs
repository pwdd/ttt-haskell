module TTT.Console.Utils.HelpersSpec (main, spec) where

import Test.Hspec

import TTT.Console.Utils.Helpers as Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "isNumber" $ do
    it "is False if input cannot be casted into a number" $
      Helpers.isNumber "foo" `shouldBe` False

    it "is True if input can be casted into a number" $
      Helpers.isNumber "42" `shouldBe` True

