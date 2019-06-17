module MultiSpec where

import           Multi
import           Test.Hspec

spec :: Spec
spec = do
  describe "" $ do
    it "..." $ do
      (nnodes tree2) == (2)
