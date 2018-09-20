module TennisSpec (spec) where

import Test.Hspec
import Tennis

runGame = foldl (flip ($)) newTennisGame

spec :: Spec
spec = do
    describe "Tennis" $ do
        it "can score a game where nobody has scored any points" $ do
            let game = runGame []
                in scoreGame game `shouldBe` "Love-Love"
        it "can score a game where player one has scored a point" $ do
            let game = runGame [ wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Fifteen-Love"
        it "can score a game where player one has scored two points" $ do
            let game = runGame [ wonPoint PlayerOne,
                                 wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Thirty-Love"
        it "can score a game where player one has scored three points" $ do
            let game = runGame [ wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Forty-Love"
        it "can score a game where player two has scored three points" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Love-Forty"
        it "can score a game where player two has scored three points" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Love-Forty"
        it "can score a simple deuce" $ do
            let game = runGame [ wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Deuce"
        it "can score a simple advantage for player one" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Advantage Player One"
        it "can score a simple advantage for player two" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Advantage Player Two"
        it "can score a win for player one" $ do
            let game = runGame [ wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Player One Wins"
        it "can score a win for player two" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Player Two Wins"
        it "can score a win from advantage for player one" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Player One Wins"
        it "can score a win from advantage for player two" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Player Two Wins"
        it "can score returning to deuce from advantage for player one" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo ]
                in scoreGame game `shouldBe` "Deuce"
        it "can score returning to deuce from advantage for player two" $ do
            let game = runGame [ wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne,
                                 wonPoint PlayerTwo,
                                 wonPoint PlayerOne ]
                in scoreGame game `shouldBe` "Deuce"