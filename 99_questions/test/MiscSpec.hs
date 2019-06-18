module MiscSpec where

import           Misc
import           Test.Hspec

spec :: Spec
spec = do
  describe "" $ do
    it "..." $ do
      (head $ queens 8) == ([1,5,8,6,3,7,2,4])
