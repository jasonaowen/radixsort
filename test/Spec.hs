import Test.Hspec
import Test.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
  describe "radixsort library" $ do
    it "sorts an empty list" $ do
      radixsort 2 ([]::[Int]) `shouldBe` []
    it "sorts a single-element list" $ do
      radixsort 2 [0] `shouldBe` [0]
    it "sorts a two-element in-order list" $ do
      radixsort 2 [0, 1] `shouldBe` [0, 1]
    it "sorts a two-element out-of-order list" $ do
      radixsort 2 [1, 0] `shouldBe` [0, 1]
    it "sorts a list with duplicate elements" $ do
      radixsort 2 [1, 0, 1] `shouldBe` [0, 1, 1]
    it "sorts a list with larger log_2 values" $ do
      radixsort 2 [2, 1] `shouldBe` [1, 2]
    it "sorts a list with even larger log_2 values" $ do
      radixsort 2 [2, 8] `shouldBe` [2, 8]
    it "sorts a list with several values" $ do
      radixsort 2 [2, 1, 12, 7, 3, 8] `shouldBe` [1, 2, 3, 7, 8, 12]
    it "sorts a list with 3 buckets" $ do
      radixsort 3 [2, 1, 12, 7, 3, 8] `shouldBe` [1, 2, 3, 7, 8, 12]
    it "sorts a list with 4 buckets" $ do
      radixsort 4 [2, 1, 12, 7, 3, 8] `shouldBe` [1, 2, 3, 7, 8, 12]
    it "sorts a large list with large numbers using many buckets" $ do
      radixsort 50 [1024,1023..1] `shouldBe` [1..1024]
  describe "list appending" $ do
    it "appends to the first list (index 0)" $ do
      appendToListAtIndex [[]] 0 1 `shouldBe` [[1]]
    it "appends to the second list (index 1)" $ do
      appendToListAtIndex [[], []] 1 1 `shouldBe` [[], [1]]
    it "appends to a non-empty list" $ do
      appendToListAtIndex [[0]] 0 1 `shouldBe` [[0, 1]]
