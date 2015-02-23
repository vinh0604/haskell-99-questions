module HaskellNinetyNineQuestions.PartOneSpec (main, spec) where

import Test.Hspec
import HaskellNinetyNineQuestions.PartOne

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "myLast" $ do
    it "returns the last item" $ do
      myLast "abc" `shouldBe` "c"
    it "return an empty list when apply on empty list" $ do
      myLast "" `shouldBe` ""
