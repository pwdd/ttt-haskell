module TTT.Core.Utils.HelpersSpec (main, spec) where

import Test.Hspec

import TTT.Core.Utils.Helpers as Helpers

main :: IO ()
main = hspec spec

spec =
  describe "chunks" $ do
    it "is the partition of a list when chunk size is factor of list length" $
      Helpers.chunks 3 [0..8] `shouldBe` [[0, 1, 2], [3, 4, 5], [6, 7, 8]]

    it "includes the remainder element when chunk size is not a factor of list length" $
      Helpers.chunks 2 [0..2] `shouldBe` [[0, 1], [2]]

    it "is the original list if list is smaller than chunk size" $
      Helpers.chunks 4 [0..2] `shouldBe` [[0, 1, 2]]

