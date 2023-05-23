{-
--Name: Mark Ian Braun
--Martikelnummer: 8175858
Group: 04
-}
import Data.List
import Data.Ord

-- You can add more imports as you need them.
import Data.Char (digitToInt, isDigit, toUpper)

-- Data types
-- You can also add more types to use
data Color = Red | Black deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Show, Eq)
data Move = Draw | Discard Card deriving (Show, Eq)
--3
data GameState = GameState {move :: [Move], hCards :: [Card], cardList:: [Card], userscore :: Int} deriving (Eq, Show)

-- Task 1
--1
--input: cardColor (Card Hearts Jack)
--output: Red  
cardColor :: Card -> Color
cardColor (Card {suit = s, rank = _}) = 
    if (s==Hearts || s == Diamonds) then Red else Black

--2
--input: cardValue (Card Hearts Jack)
--output: 10
cardValue :: Card -> Int
cardValue (Card {suit = _, rank = r}) 
    |r == Num 2 = 2
    |r == Num 3 = 3
    |r == Num 4 = 4
    |r == Num 5 = 5
    |r == Num 6 = 6
    |r == Num 7 = 7
    |r == Num 8 = 8
    |r == Num 9 = 9
    |r == Num 10 = 10
    |r == Jack = 10
    |r == Queen = 10
    |r == King = 10
    |r == Ace = 11
    |otherwise = error "NOT AVAILABLE"

--4
--input: convertSuit 'c'
--output Clubs
convertSuit :: Char -> Suit
convertSuit c
    |c =='d'||c== 'D' = Diamonds 
    |c =='s'||c== 'S' = Spades 
    |c =='h'||c== 'H' = Hearts 
    |c =='c'||c== 'C' = Clubs 
    | otherwise = error "no corresponding Suit found"

--5
--input: convertRank '2'
--output: Num 2
convertRank :: Char -> Rank
convertRank r
    |r == '1' = Ace
    |r == '2' = Num 2
    |r == '3' = Num 3
    |r == '4' = Num 4
    |r == '5' = Num 5
    |r == '6' = Num 6
    |r == '7' = Num 7
    |r == '8' = Num 8
    |r == '9' = Num 9
    |r == 't' ||r == 'T' = Num 10
    |r == 'j' ||r == 'J' = Jack
    |r == 'q' ||r == 'Q' = Queen
    |r == 'k' ||r == 'K' = King
    |otherwise = error "no corresponding Rank found"

--6
--convertCard 'd' '1'
--output: Card {suit = Diamonds, rank = Ace}
convertCard :: Char -> Char -> Card
convertCard s r = Card {suit = (convertSuit s), rank = (convertRank r)}

--7
--convertMove 'd' 'd' '1'
--Draw
--convertMove 'r' 'd' '1'
--Discard (Card {suit = Diamonds, rank = Ace})
convertMove :: Char -> Char -> Char -> Move
convertMove m s r = 
        if (m == 'd' || m == 'D') 
            then Draw
                else if ( m== 'r' || m == 'R') 
                    then Discard (Card {suit = (convertSuit s), rank = (convertRank r)})
                        else error "wrong move"

--8
--removeCard [(Card Hearts Jack), (Card Spades Ace), (Card Hearts Jack)] (Card Hearts Jack)
--[Card {suit = Spades, rank = Ace},Card {suit = Hearts, rank = Jack}]
removeCard :: [Card] -> Card -> [Card]
removeCard cs c = if c `elem` cs then (delete c cs) else error "not in list"


--9
--allSameColor [(Card Hearts Jack), (Card Spades Ace), (Card Diamonds Jack)]
--False
--allSameColor [(Card Hearts Jack), (Card Diamonds Jack)]                   
--True
allSameColor :: [Card] -> Bool
allSameColor [] = error "empty List" 
allSameColor [_] = True
allSameColor [x,y] = 
     if (cardColor x == cardColor y)
         then True 
     else False
allSameColor (x:y:xs) = 
    if (cardColor x == cardColor y) 
        then allSameColor (y:xs)
     else False

--for runGame
allSameColor' :: [Card] -> Bool
allSameColor' [] = error "empty Held Cards List" --error for the runGame function
allSameColor' [_] = True
allSameColor' [x,y] = 
     if (cardColor x == cardColor y)
         then True 
     else False
allSameColor' (x:y:xs) = 
    if (cardColor x == cardColor y) 
        then allSameColor' (y:xs)
     else False

--10
--sumCards [(Card Hearts Jack), (Card Diamonds Jack)]
--20
sumCards :: [Card] -> Int
sumCards xs = sum (helperList xs)
    where 
        helperList [] = []
        helperList (x:xs) = cardValue x : helperList xs

--11
--score [(Card Hearts Jack), (Card Diamonds Jack)] 40
--10
score :: [Card] -> Int -> Int
score xs g = 
    let preliminarySum =  if ((sumCards xs) > g )
                            then 3 * (sumCards xs - g)
                          else g - sumCards xs
    in
       if ((allSameColor xs) == True) 
            then preliminarySum `div` 2
       else preliminarySum

--12 
runGame :: [Card] -> [Move] -> Int -> Int
runGame (c:cs) ms g  = userscore $ helperprocessMoves start ms
    where
        start = GameState {move = ms, hCards = [], cardList = c:cs, userscore = 0}
        helperprocessMoves :: GameState -> [Move] -> GameState
        helperprocessMoves gstate [] = gstate
        helperprocessMoves gstate (m:ms) = helperprocessMoves (processMove gstate m) ms
       
        processMove :: GameState -> Move -> GameState
        processMove gstate m =
            case m of 
                Discard c -> 
                    if c `elem` hCards gstate
                        then let 
                            newHlist = removeCard (hCards gstate) c
                            updatedState = gstate {hCards = newHlist, userscore = score newHlist g}
                        in updatedState
                        
                        else error "Card not found in List"
                
                Draw ->
                    if ((userscore gstate) > g || null (cardList gstate)) then 
                        let finalscore = score (hCards gstate) g
                        in gstate {userscore = finalscore}
                    else 
                        let drawnCard = head (cardList gstate)
                            newHlist = drawnCard : hCards gstate
                            updatedState = gstate {hCards = newHlist, cardList = tail (cardList gstate), userscore = score newHlist g}
                        in updatedState
                        --gstate {move = ms, hCards = [head (cardList gstate)] ++ hCards gstate, cardList = tail (cardList gstate),  userscore = userscore gstate  + 1 }
-- runGame([(Card{suit = Spades, rank =Num 10}), (Card{suit = Hearts, rank = Num 9}), (Card{suit = Spades, rank =Num 10})]) [Draw,Draw] g 
--runGame[ (Card {suit = Spades, rank = Num 10}),(Card {suit = Hearts, rank = Num 9}),(Card {suit = Spades, rank = Num 10})][Draw, Draw, Discard (Card {suit = Spades, rank = Num 10})]42
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