module TTT.Console.Players.ValidationSpec (main, spec) where

import Test.Hspec

import TTT.Console.Players.Validation as Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "isValidMarker" $ do
    it "is true if marker is a lower case char" $
      Validation.isValidMarker 'a' ' ' `shouldBe` True

    it "is true if marker is a upper case char" $
      Validation.isValidMarker 'A' ' ' `shouldBe` True

    it "is false if marker is an empty char" $
      Validation.isValidMarker ' ' 'x' `shouldBe` False

    it "is false if marker is a number" $
      Validation.isValidMarker '1' 'x' `shouldBe` False

    it "is false if marker is a special char" $
      Validation.isValidMarker '#' 'x' `shouldBe` False

    it "is false if marker is already taken" $
      Validation.isValidMarker 'x' 'x' `shouldBe` False

