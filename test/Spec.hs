import Test.Hspec
import Test.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
  describe "radixsort library" $ do
    it "runs the tests" $ do
      True `shouldBe` True
