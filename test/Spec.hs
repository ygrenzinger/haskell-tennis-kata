import Lib

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

import Test.QuickCheck

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
    it "should respect sets invariants" $ do
      quickCheckWith Test.QuickCheck.stdArgs {Test.QuickCheck.maxSuccess = 3000}  props_setAreValid

-- https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-- https://www.schoolofhaskell.com/user/griba/quick-check-generator-of-pair-List-index-where-index-within-list-range
-- https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
-- http://codingstruggles.com/haskell/arbitrary-length-lists-quickcheck.html


-- invariants
-- set (s1 || s2) < 6 or |s1 - s2| <= 2


instance Arbitrary Player where
  arbitrary = do
    point <- elements [p1, p2]
    return point    

randomPlayerPoint :: Gen Player
randomPlayerPoint = do
  point <- elements [p1, p2]
  return point

randomMatch :: Gen [Player]
randomMatch = do
  n <- choose (72, 400)
  match <- sequenceA (take n (repeat $ randomPlayerPoint))
  return match

instance Arbitrary Match where
  arbitrary = do
    match <- randomMatch
    return (score match)

props_setAreValid :: Match -> Bool
props_setAreValid (Match sets _) = all isValidSet sets  
props_setAreValid (MatchWin sets _) = 
  let setLength = length sets
  in setLength >= 3 && setLength <= 5

isValidSet :: Set -> Bool
isValidSet (Set s1 s2) = s1 <= 6 && s2 <= 6 || abs (s1 - s2) <= 2
