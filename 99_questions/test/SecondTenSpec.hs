module SecondTenSpec where

import           SecondTen
import           Test.Hspec

spec :: Spec
spec = do
  describe "Question 11" $ do
    it "Modified encoder..." $ do
      ([Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']) == (encodeModified "aaaabccaadeeee")

  describe "Question 12" $ do
    it "Decode..." $ do
      (decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']) == ("aaaabccaadeeee")

  describe "Question 13" $ do
    it "Encode directly..." $ do
      ([Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']) == (encodeDirect "aaaabccaadeeee")

  describe "Question 14" $ do
    it "Duplicate..." $ do
      (dupli [1, 2, 3]) == ([1,1,2,2,3,3])

  describe "Question 15" $ do
    it "Replicate..." $ do
      (repli "abc" 3) == ("aaabbbccc")
