module Main where

import           Test.Hspec

absolute :: Int -> Int
absolute _ = 0

main :: IO ()
main = hspec $ describe "absolute" $ do
    it "returns the original number when given a positive input"
        $          absolute 1
        `shouldBe` 1

    it "returns a positive number when given a negative input"
        $          absolute (-1)
        `shouldBe` 1

    it "returns zero when given zero" $ absolute 0 `shouldBe` 0
