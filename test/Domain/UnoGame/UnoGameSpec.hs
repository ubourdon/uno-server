module Domain.UnoGame.UnoGameSpec where 

import Test.Hspec

test1 :: IO ()
test1 = hspec $ do
  describe "Prelude.read" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)