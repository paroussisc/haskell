module TreesSpec where

import           Test.Hspec
import           Trees

spec :: Spec
spec = do
  describe "" $ do
    it "..." $ do
      (symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)) == (False)
    it "..." $ do
      (symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))) == (True)

    it "57a" $ do
      (construct [3, 2, 5, 7, 1]) == (Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty)))
    it "57b" $ do
      (symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]) == (True)
    it "57c" $ do
      (symmetric . construct $ [3, 2, 5, 7, 1]) == (True)

    it "58" $ do
      (symCbalTrees 5) == ([Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))])

    it "61" $ do
      (countLeaves $ head $ cbalTree 4) == (2)

    it "61A" $ do
      (leaves $ Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))(Branch 2 Empty Empty)) == ([4,2])

    it "62" $ do
      (internals $ Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))(Branch 2 Empty Empty)) == ([1,2])

    it "62B" $ do
      (atLevel tree4 2) == ([2,2])

    it "59" $ do
      (take 4 $ hbalTree 'x' 3) == ([Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
                                     Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
                                     Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
                                     Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)])
