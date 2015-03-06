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

  describe "myLength" $ do
    it "returns 0 on empty list" $ do
      myLength "" `shouldBe` 0
    it "returns count of element in list" $ do
      myLength "a" `shouldBe` 1
      myLength "ab" `shouldBe` 2

  describe "myReverse" $ do
    it "returns empty list on empty list" $ do
      myReverse "" `shouldBe` ""
    it "returns new list with element of original list in reversed order" $ do
      myReverse "abc" `shouldBe` "cba"
      myReverse [0,2,1] `shouldBe` [1,2,0]

  describe "isPalindrome" $ do
    it "returns false on empty list" $ do
      isPalindrome "" `shouldBe` False
    it "returns true on single-element list" $ do
      isPalindrome [1] `shouldBe` True
    it "returns true on palindrome list" $ do
      isPalindrome [1,2,2,1] `shouldBe` True
      isPalindrome [1,2,1] `shouldBe` True
    it "returns false on non palindrome list" $ do
      isPalindrome [1,2] `shouldBe` False
      isPalindrome [1,2,2] `shouldBe` False

  describe "flatten" $ do
    it "returns single-element list when call on elemet" $ do
      flatten (Elem 1) `shouldBe` [1]
    it "returns flatted list of when call on list" $ do
      flatten (List [Elem 1, List [Elem 2, Elem 3]]) `shouldBe` [1,2,3]
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]

  describe "compress" $ do
    it "removes consecutive duplicates and keep the orginal order" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"
    it "returns empty list for empty list" $ do
      compress "" `shouldBe` []

  describe "pack" $ do
    it "pack consecutive duplicates into sublist" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
    it "returns empty list for empty list" $ do
      pack "" `shouldBe` []
