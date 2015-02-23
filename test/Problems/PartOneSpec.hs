module Problems.PartOneSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Problems.PartOne

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "myLast" $ do
    it "returns the last item" $ do
      myLast "abc" `shouldBe` 'c'
    it "throw error call when apply on empty list" $ do
      evaluate (myLast "") `shouldThrow` anyErrorCall
