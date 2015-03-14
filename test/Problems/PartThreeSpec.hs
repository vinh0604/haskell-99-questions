module Problems.PartThreeSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Problems.PartThree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "insertAt" $ do
    it "inserts new element at index" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"
    it "inserts new element to list tail if index is equal length plus 1" $ do
      insertAt 'X' "abcd" 5 `shouldBe` "abcdX"
    it "throws error if index is out of bound" $ do
      evaluate (insertAt 'X' "abcd" 0) `shouldThrow` anyErrorCall
      evaluate (insertAt 'X' "abcd" 6) `shouldThrow` anyErrorCall
