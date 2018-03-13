import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

p1 = Player1
p2 = Player2

main :: IO ()
main = hspec $ do
  describe "player scoring" $ do
    it "should save score of player" $ do
      let game = p1 `scoreIn` []
      head game `shouldBe` p1
    it "should have a zero score for a starting game" $ do
      let game = []
      score game `shouldBe` "0-0"
    it "should have a 15-0 score if player1 score" $ do
      let game = [p1]
      score game `shouldBe` "15-0"
    it "should have a 30-0 score if player1 score 2 times" $ do
      let game = [p1, p1]
      score game `shouldBe` "30-0"
    it "should have a 40-0 score if player1 score 3 times" $ do
      let game = [p1, p1, p1]
      score game `shouldBe` "40-0"
    it "should have a 15-15 score if player1 and player2 score" $ do
      let game = [p1, p2]
      score game `shouldBe` "15-15"
    it "should have a 15-40 score if player1 and player2 score" $ do
      let game = [p2, p2, p1, p2]
      score game `shouldBe` "15-40"
    it "should have avantage Player1 score if player1 and player2 score" $ do
      let game = [p2, p2, p1, p2, p1, p1, p1]
      score game `shouldBe` "Avantage Player1"
    --it "should give Nadal as the winner if he is the only to scored" $ do
      --let game = "Nadal" `scoreIn` []
      --score game `shouldBe` "Nadal"
