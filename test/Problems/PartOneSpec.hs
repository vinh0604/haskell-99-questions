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

  describe "myButLast" $ do
    it "returns the last but one element of a list" $ do
      myButLast "abc" `shouldBe` 'b'
    it "throw error call when apply on empty or single-element list" $ do
      evaluate (myButLast "a") `shouldThrow` anyErrorCall
      evaluate (myButLast "") `shouldThrow` anyErrorCall

  describe "elementAt" $ do
    it "returns element at provided index in the list" $ do
      elementAt "abc" 1 `shouldBe` 'a'
      elementAt "abc" 2 `shouldBe` 'b'
    it "throw error if index is outbound" $ do
      evaluate (elementAt "abc" 0) `shouldThrow` anyErrorCall
      evaluate (elementAt "abc" 4) `shouldThrow` anyErrorCall
