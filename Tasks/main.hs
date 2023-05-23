-- You can add more imports as you need them.
import Data.Char (digitToInt, isDigit, toUpper)

-- Data types
-- You can also add more types to use
data Color = Red | Black deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Show, Eq)
data Move = Draw | Discard Card deriving (Show, Eq)
-- Task 3
data State = ...

-- Task 1
cardColor :: ...

-- Task 2
cardValue :: ...

-- Task 4
convertSuit :: ...

-- Task 5
convertRank :: ...

-- Task 6
convertCard :: ...

-- Task 7
convertMove :: ...

-- Task 8
removeCard :: ...

-- Task 9
allSameColor :: ...

-- Task 10
sumCards :: ...

-- Task 11
score :: ...

-- Task 12
runGame :: ...



-- Helper functions to read input from user and run the runGame function
readAnyList :: (String -> a) -> IO [a]
readAnyList f = do
    line <- getLine
    if line == "."
        then return []
        else do
            let newList = f line : []
            remainingList <- readAnyList f
            return $ newList ++ remainingList

readCards :: IO [Card]
readCards = readAnyList (\line -> convertCard (line !! 0) (line !! 1))

readMoves :: IO [Move]
readMoves = readAnyList (\line -> 
    case length line of
        1 -> convertMove (line !! 0) ' ' ' '
        3 -> convertMove (line !! 0) (line !! 1) (line !! 2)
        _ -> error "Move is not valid.")


main :: IO ()
main = do putStrLn "Enter cards:"
          cards <- readCards
          -- You can uncomment to see the list of cards read by the user
          --putStrLn (show cards)
          putStrLn "Enter moves:"
          moves <- readMoves
          -- You can uncomment to see the list of cards read by the user
          --putStrLn (show moves)
          putStrLn "Enter goal:"
          line <- getLine
          let goal = read line :: Int
          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)