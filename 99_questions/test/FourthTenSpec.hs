module FourthTenSpec where

import           FourthTen
import           Test.Hspec

spec :: Spec
spec = do
  describe "Question 31" $ do
    it "Insert at..." $ do
      (isPrime 7) == (True)
