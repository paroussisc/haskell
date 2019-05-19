module SecondTenSpec where

import           SecondTen
import           Test.Hspec

spec :: Spec
spec = do
  describe "Question 11" $ do
    it "Modified encoder..." $ do
      ([Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']) == (encodeModified "aaaabccaadeeee")
