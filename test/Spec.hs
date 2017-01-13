import Test.Hspec
import Test.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
  describe "radixsort library" $ do
    it "sorts an empty list" $ do
      radixsort ([]::[Int]) `shouldBe` []
    it "sorts a single-element list" $ do
      radixsort [0] `shouldBe` [0]
    it "sorts a two-element in-order list" $ do
      radixsort [0, 1] `shouldBe` [0, 1]
    it "sorts a two-element out-of-order list" $ do
      radixsort [1, 0] `shouldBe` [0, 1]
    it "sorts a list with duplicate elements" $ do
      radixsort [1, 0, 1] `shouldBe` [0, 1, 1]
    it "sorts a list with larger log_2 values" $ do
      radixsort [2, 1] `shouldBe` [1, 2]
    it "sorts a list with even larger log_2 values" $ do
      radixsort [2, 8] `shouldBe` [2, 8]
    it "sorts a list with several values" $ do
      radixsort [2, 1, 12, 7, 3, 8] `shouldBe` [1, 2, 3, 7, 8, 12]
