module LogicCodesSpec where

  import           LogicCodes
  import           Test.Hspec

  spec :: Spec
  spec = do
    describe "Question 46" $ do
      it "and..." $ do
        (and' True True) == (True)
      it "and..." $ do
        (and' False True) == (False)
      it "and..." $ do
        (and' True False) == (False)
      it "and..." $ do
        (and' False False) == (False)

      it "or..." $ do
        (or' True True) == (True)
      it "or..." $ do
        (or' False True) == (True)
      it "or..." $ do
        (or' True False) == (True)
      it "or..." $ do
        (or' False False) == (False)

      it "nand..." $ do
        (nand' True True) == (False)
      it "nand..." $ do
        (nand' False True) == (True)
      it "nand..." $ do
        (nand' True False) == (True)
      it "nand..." $ do
        (nand' False False) == (True)

      it "nor..." $ do
        (nor' True True) == (False)
      it "nor..." $ do
        (nor' False True) == (False)
      it "nor..." $ do
        (nor' True False) == (False)
      it "nor..." $ do
        (nor' False False) == (True)

      it "xor..." $ do
        (xor' True True) == (False)
      it "xor..." $ do
        (xor' False True) == (True)
      it "xor..." $ do
        (xor' True False) == (True)
      it "xor..." $ do
        (xor' False False) == (False)

    describe "Question 49" $ do
      it "gray..." $ do
        (gray 3) == (["000","001","011","010","110","111","101","100"])
      it "gray..." $ do
        (gray 4) == (["0000","0001","0011","0010","0110","0111","0101","0100",
                      "1100","1101","1111","1110","1010","1011","1001","1000"])
