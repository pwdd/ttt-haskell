module TTT.Messenger.ValidationSpec (main, spec) where

import Test.Hspec

import Data.Map.Strict as Map
import TTT.Messenger.Validation as Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "isValidMessenger" $ do
    let fakeMessengers = Map.fromList [("LanguageOne", 1), ("LanguageTwo", 2), ("LanguageThree", 3)]
    it "is true if input is among values of messengers map" $
      Validation.isValidMessenger 2 fakeMessengers `shouldBe` True

    it "is false if input is not among values of the messengers map" $
      Validation.isValidMessenger 5 fakeMessengers `shouldBe` False

