module Problems.PartTwoSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Problems.PartTwo

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "encodeModified" $ do
    it "encode consecutive duplicates into list of tuple (N E) and keep non duplicates as is" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
    it "returns empty list for empty list" $ do
      encodeModified "" `shouldBe` []
  describe "decodeModified" $ do
    it "decode encoded list back to original list" $ do
      decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"
    it "returns empty list for empty list" $ do
      decodeModified [] `shouldBe` ""
  describe "encodeDirect" $ do
    it "behaves like encodeModified" $ do
      encodeDirect "aaaabccaadeeee" `shouldBe` encodeModified "aaaabccaadeeee"
  describe "dupli" $ do
    it "duplicates each element one in the list" $ do
      dupli "abcc" `shouldBe` "aabbcccc"
    it "returns empty list on empty list" $ do
      dupli "" `shouldBe` []
  describe "repli" $ do
    it "replicate each item in the list with specific time" $ do
      repli "abcc" 3 `shouldBe` "aaabbbcccccc"
    it "returns empty list when replicate 0 times" $ do
      repli "abcc" 0 `shouldBe` []
