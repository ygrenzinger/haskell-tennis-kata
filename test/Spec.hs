import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

p1 = Player1 "Nadal"
p2 = Player2 "Federer"

main :: IO ()
main = hspec $ do
  describe "player scoring" $ do
    it "should save score of player" $ do
      let game = p1 `scoreIn` []
      head game `shouldBe` p1
    it "should have a zero score for a starting game" $ do
      let game = []
      printScore game `shouldBe` "0-0 Love-Love"
    it "should have a 15-Love score" $ do
      let game = [p1]
      printScore game `shouldBe` "0-0 15-Love"
    it "should have a 30-Love score" $ do
      let game = [p1, p1]
      printScore game `shouldBe` "0-0 30-Love"
    it "should have a 40-Love score" $ do
      let game = [p1, p1, p1]
      printScore game `shouldBe` "0-0 40-Love"
    it "should have a 15-15 score" $ do
      let game = [p1, p2]
      printScore game `shouldBe` "0-0 15-15"
    it "should have a 15-40 score" $ do
      let game = [p2, p2, p1, p2]
      printScore game `shouldBe` "0-0 15-40"
    it "should have avantage Player1" $ do
      let game = [p2, p2, p1, p2, p1, p1, p1]
      printScore game `shouldBe` "0-0 Advantage Nadal"
    it "should have deuce" $ do
      let game = [p2, p2, p1, p2, p1, p1, p1, p2]
      printScore game `shouldBe` "0-0 Deuce"
    it "should gain advantage" $ do
      let game = [p2, p2, p1, p2, p1, p1, p1, p2, p2]
      printScore game `shouldBe` "0-0 Advantage Federer"
    it "should have Federer flawlessy win the game" $ do
      let game = take 4 $ (repeat p2)
      printScore game `shouldBe` "0-1 Love-Love"
    it "should have Federer win the game" $ do
      let game = [p2, p2, p1, p2, p1, p1, p1, p2, p2, p2]
      printScore game `shouldBe` "0-1 Love-Love"
    it "should have Nadal win the game" $ do
      let game = [p2, p2, p1, p2, p1, p1, p1, p1]
      printScore game `shouldBe` "1-0 Love-Love"
    it "should have Nadal win the set" $ do
      let game = take 24 $ (repeat p1)
      printScore game `shouldBe` "6-0 0-0 Love-Love"
    it "should have Federer win the set" $ do
      let game = take 24 $ (repeat p2)
      printScore game `shouldBe` "0-6 0-0 Love-Love"
    it "should have no winner so far" $ do
      let game1 = (take 20 $ (repeat p1)) ++ (take 20 $ (repeat p2)) ++ (take 4 $ (repeat p1)) ++ (take 4 $ (repeat p2)) ++ (take 4 $ (repeat p1))
      printScore game1 `shouldBe` "7-6 Love-Love"
      let game2 = (take 20 $ (repeat p1)) ++ (take 20 $ (repeat p2)) ++ (take 4 $ (repeat p2)) ++ (take 4 $ (repeat p1)) ++ (take 4 $ (repeat p2))
      printScore game2 `shouldBe` "6-7 Love-Love"
    it "should have Nadal win the set" $ do
      let game = (take 20 $ (repeat p1)) ++ (take 20 $ (repeat p2)) ++ (take 4 $ (repeat p1)) ++ (take 4 $ (repeat p2)) ++ (take 8 $ (repeat p1))
      printScore game `shouldBe` "8-6 0-0 Love-Love"
    it "should win the match" $ do
      let game = (take 72 $ (repeat p1))
      printScore game `shouldBe` "6-0 6-0 6-0 Nadal wins the game !"