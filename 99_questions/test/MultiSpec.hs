module MultiSpec where

import           Multi
import           Test.Hspec

spec :: Spec
spec = do
  describe "" $ do
    it "..." $ do
      (nnodes tree2) == (2)

    it "71" $ do
      (ipl tree5) == (9)

    it "71" $ do
      (ipl tree4) == (2)

    it "72" $ do
      (bottom_up tree5) == ("gfcdeba")
