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
    it "returns empty list when replicate less than or equal 0 times" $ do
      repli "abcc" 0 `shouldBe` []
      repli "abcc" (-1) `shouldBe` []
  describe "dropEvery" $ do
    it "drop every element after a specific time" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
    it "returns original list if when posistion is less  than or equal 0" $ do
      dropEvery "abcd" 0 `shouldBe` "abcd"
      dropEvery "abcd" (-1) `shouldBe` "abcd"
  describe "split" $ do
    it "split origin on specific position" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")
    it "returns original list as the second and empty list as the first when position is less than or equal 0" $ do
      split "abcd" 0 `shouldBe` ([], "abcd")
      split "abcd" (-1) `shouldBe` ([], "abcd")
    it "returns original list as the first and empty list as the second when position is greater than or equal list length" $ do
      split "abcd" 4 `shouldBe` ("abcd", [])
      split "abcd" 5 `shouldBe` ("abcd", [])
  describe "slice" $ do
    it "extract elements of the list with index range, start from 1" $ do
      slice "abcdefghik" 3 7 `shouldBe` "cdefg"
    it "extract from first element if the from index is less than 1" $ do
      slice "abcdefghik" (0) 3 `shouldBe` "abc"
    it "returns empty list if to index is less than from index" $ do
      slice "abcdefghik" 3 2 `shouldBe` []
