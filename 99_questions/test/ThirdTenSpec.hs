module ThirdTenSpec where

import           Test.Hspec
import           ThirdTen

spec :: Spec
spec = do
  describe "Question 21" $ do
    it "Insert at..." $ do
      (insertAt 'X' "abcd" 2) == ("abXcd")

  describe "Question 22" $ do
    it "Here's a range..." $ do
      (range 4 9) == ([4,5,6,7,8,9])

  describe "Question 26" $ do
    it "Permutations..." $ do
      (combinations 2 "abc") == (["ab", "ac", "bc"])

  describe "Question 28a" $ do
    it "Size sort..." $ do
      (lsort ["abc","de","fgh","de","ijkl","mn","o"]) == (["o","de","de","mn","abc","fgh","ijkl"])


  describe "Question 28b" $ do
    it "Size sort..." $ do
      (lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]) == (["o","ijkl","abc","fgh","de","de","mn"])
