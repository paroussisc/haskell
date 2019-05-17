module FirstTenSpec where

import Test.Hspec
import FirstTen

spec :: Spec
spec = do
  describe "Question 1" $ do
    it "Question 1..." $ do
      (myLast [1,2,3,4]) `shouldBe` (4)

  describe "Question 2" $ do
    it "Question 2..." $ do
      (myButLast ['a'..'z']) `shouldBe` ('y')

  describe "Question 3" $ do
    it "Question 3..." $ do
      (elementAt "haskell" 5) `shouldBe` ('e')

  describe "Question 4" $ do
    it "Question 4..." $ do
      (myLength [123, 456, 789]) `shouldBe` (3)

  describe "Question 5" $ do
    it "Question 5..." $ do
      (myReverse [1,2,3,4]) `shouldBe` ([4,3,2,1])

  describe "Question 6" $ do
    it "Question 6..." $ do
      (isPalindrome [1,2,4,8,16,8,4,2,1]) `shouldBe` (True)

  describe "Question 7" $ do
    it "Question 7..." $ do
      (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])) `shouldBe` ([1,2,3,4,5])

  describe "Question 8" $ do
    it "Question 8..." $ do
      (compress "aaaabccaadeeee") `shouldBe` ("abcade")

  describe "Question 9" $ do
    it "Question 9..." $ do
      (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
             'a', 'd', 'e', 'e', 'e', 'e']) `shouldBe` (["aaaa","b","cc","aa","d","eeee"])

  describe "Question 10" $ do
    it "Question 10..." $ do
      (encode "aaaabccaadeeee") `shouldBe` ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])
