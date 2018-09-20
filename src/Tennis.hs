module Tennis (newTennisGame, wonPoint, scoreGame, Player(..)) where

data TennisGame = Points Score Score | Deuce | Advantage Player | Win Player
data Score = Love | Fifteen | Thirty | Forty
data Player = PlayerOne | PlayerTwo

newTennisGame = Points Love Love

incrementPoint Love = Fifteen
incrementPoint Fifteen = Thirty
incrementPoint Thirty = Forty

wonPoint player Deuce = Advantage player
wonPoint PlayerTwo (Points Forty Thirty) = Deuce
wonPoint PlayerOne (Points Thirty Forty) = Deuce
wonPoint PlayerOne (Advantage PlayerOne) = Win PlayerOne
wonPoint PlayerOne (Advantage PlayerTwo) = Deuce 
wonPoint PlayerTwo (Advantage PlayerOne) = Deuce
wonPoint PlayerTwo (Advantage PlayerTwo) = Win PlayerTwo
wonPoint PlayerOne (Points Forty _) = Win PlayerOne
wonPoint PlayerTwo (Points _ Forty) = Win PlayerTwo
wonPoint PlayerOne (Points point other) = Points (incrementPoint point) other
wonPoint PlayerTwo (Points other point) = Points other (incrementPoint point)

toString Love = "Love"
toString Fifteen = "Fifteen"
toString Thirty = "Thirty"
toString Forty = "Forty"

namePlayer PlayerOne = "Player One"
namePlayer PlayerTwo = "Player Two"

scoreGame (Win player) = namePlayer player ++ " Wins" 
scoreGame Deuce = "Deuce"
scoreGame (Advantage player) = "Advantage " ++ namePlayer player
scoreGame (Points playerOneScore playerTwoScore) = 
    foldl (++) mempty [
        (toString playerOneScore), 
        "-", 
        (toString playerTwoScore) 
    ]
