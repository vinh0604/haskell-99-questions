module Problems.PartOneSpec (main, spec) where

import Test.Hspec
import Problems.PartOne

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "myLast" $ do
    it "returns the last item" $ do
      myLast "abc" `shouldBe` 'c'
    it "return undefined when apply on empty list" $ do
      myLast "" `shouldBe` undefined
