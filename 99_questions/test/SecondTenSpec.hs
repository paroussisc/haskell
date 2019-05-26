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

  describe "Question 16" $ do
    it "Drop em..." $ do
      (dropEvery "abcdefghik" 3) == ("abdeghk")

  describe "Question 17" $ do
    it "Split em..." $ do
      (split "abcdefghik" 3) == (("abc", "defghik"))

  describe "Question 18" $ do
    it "Slice em..." $ do
      (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7) == ("cdefg")

  describe "Question 19" $ do
    it "Rotate em forward..." $ do
      (rotate ['a','b','c','d','e','f','g','h'] 3) == ("defghabc")
    it "Rotate em back..." $ do
      (rotate ['a','b','c','d','e','f','g','h'] (-2)) == ("ghabcdef")

  describe "Question 20" $ do
    it "Remove at..." $ do
      (removeAt 2 "abcd") == ("acd")
