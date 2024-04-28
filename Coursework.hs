

-- Coursework - Functional Programming
-- Students:
-- CSY22083
-- CSY22042
-- CSY22077
-- CSY22053
-- CSY22057


-- BLACKJACK  GAME IMPLEMENTATION 

-- importing Data.Maybe for later usage

import Data.Maybe

-- Types

-- Suit, the suit of a card: hearts, spades, diamonds, and clubs.

data Suit = Hearts | Spades | Diamonds | Clubs 
    deriving (Eq, Show, Enum, Bounded)

-- Rank, the rank of a card: numeric and its value (2-10), jack, queen, king, and ace.

data Rank = Numeric Int | Jack | Queen | King | Ace 
    deriving (Eq, Show)

validNumericRank :: Int -> Bool
validNumericRank n = n >= 2 && n <= 10

-- Card, the card itself which has a rank and a suit, (using a record to name the components might be useful).

data Card = Card { rank :: Rank, suit :: Suit } 
    deriving (Eq, Show)

-- Hand, the hand of a player which is a list of cards, a type synonym will suffice.

type Hand = [Card]

-- Player, either bank or guest.

data Player = Bank | Guest 
    deriving (Eq, Show)

-- Functionality 

--1
-- Define a function size :: Hand -> Integer that returns the number of cards in the hand.

size :: Hand -> Integer
size [] = 0
size (_:xs) = 1 + size xs

--2
-- Define a function faceCards :: Hand -> Integer that returns the number of face cards in the hand.

faceCards :: Hand -> Integer
faceCards [] = 0      
faceCards (Card {rank = Jack}:xs)  = 1 + faceCards xs
faceCards (Card {rank = Queen}:xs) = 1 + faceCards xs
faceCards (Card {rank = King}:xs)  = 1 + faceCards xs
faceCards (_:xs)                  = faceCards xs

--3
-- Define a function valueCard :: Card -> Integer that returns the numeric value of the card based on the rank.

valueRank :: Rank -> Integer
valueRank (Numeric n) = fromIntegral n
valueRank Jack        = 10
valueRank Queen       = 10
valueRank King        = 10
valueRank Ace         = 11

--4
-- Define a function valueCard :: Card -> Integer that returns the numeric value of the card based on the rank.

valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

--5
-- Define a function numberOfAces :: Hand -> Integer that returns the number of aces in a hand.

numberOfAces :: Hand -> Integer
numberOfAces [] = 0
numberOfAces (Card {rank = Ace}:xs) = 1 + numberOfAces xs
numberOfAces (_:xs) = numberOfAces xs

--6
-- Define a function valueHand :: Hand -> Integer that calculates the total value of a whole hand (add up each card's value).

valueHand :: Hand -> Integer
valueHand [] = 0
valueHand (card:xs) = valueCard card + valueHand xs

--7
-- Define a function value :: Hand -> Integer that calculates the total value of a hand but if the value
-- exceeds 21, turn the hand's aces into 1s instead of 11s.

value :: Hand -> Integer
value hand = adjustValue (valueHand hand) (numberOfAces hand)

-- Function to adjust the value of the hand if it exceeds 21

adjustValue :: Integer -> Integer -> Integer
adjustValue total aces
  | total > 21 && aces > 0   = adjustValue (total - 10) (aces - 1)  
  | otherwise                = total

--8
-- Define a function isBlackjack :: Hand -> Bool that determines whether the hand forms a blackjack. A
-- blackjack is hand with 2 cards that has the value of 21.

isBlackjack :: Hand -> Bool
isBlackjack hand = case hand of
    [Card Ace _, Card (Numeric 10) _] -> True
    [Card (Numeric 10) _, Card Ace _] -> True
    [Card Ace _, Card Jack _] -> True
    [Card Jack _, Card Ace _] -> True
    [Card Ace _, Card Queen _] -> True
    [Card Queen _, Card Ace _] -> True
    [Card Ace _, Card King _] -> True
    [Card King _, Card Ace _] -> True
    _ -> False


--9
-- Define a function gameOver :: Hand -> Bool that checks if the given hand loses (value greater than 21).

gameOver :: Hand -> Bool
gameOver hand = valueHand hand > 21

--10
-- Define a function winner :: Hand -> Hand -> Player given the guest hand and the bank hand returns
-- the player who won. Tie goes to the bank.

winner :: Hand -> Hand -> Player
winner guestHand bankHand -- all possible scenarios
    | guestValue > 21 && bankValue > 21 = Bank   
    | guestValue > 21                   = Bank   
    | bankValue > 21                    = Guest  
    | guestValue == bankValue           = Bank   
    | guestValue > bankValue            = Guest  
    | otherwise                         = Bank   
  where
    guestValue = valueHand guestHand
    bankValue = valueHand bankHand

--11
-- Define an operator (<+) :: Hand -> Hand -> Hand that places the first hand on top of the other and
-- returns the resulting hand.

infixr 5 <+  -- Set operator for combining two hands
(<+) :: Hand -> Hand -> Hand
hand1 <+ hand2 = hand1 ++ hand2

--12
-- Define a function handSuit :: Suit -> Hand that given a suit, returns a hand with all 13 cards of that suit.

handSuit :: Suit -> Hand
handSuit suit = [Card {rank = r, suit = suit} | r <- ranks]
  where
    ranks = map Numeric [2..10] ++ [Jack, Queen, King, Ace]

--13
-- Define a value fullDeck :: Hand that consists of the 52 card complete deck.

fullDeck :: Hand
fullDeck = [Card {rank = r, suit = s} | s <- suits, r <- ranks]
  where
    suits = [minBound .. maxBound] :: [Suit]
    ranks = map Numeric [2..10] ++ [Jack, Queen, King, Ace]

--14
-- Define a function draw :: Hand -> Hand -> (Hand, Hand) that given a deck and a hand, draws a card
-- from the deck and returns the remaining deck and the new hand. Throw an error if the deck is empty.

draw :: Hand -> Hand -> Maybe (Hand, Hand)
draw [] _ = Nothing  -- Empty deck - return Nothing
draw (topCard:restOfDeck) hand = Just (restOfDeck, topCard : hand)


--15
-- Define a function playBank :: Hand -> Hand -> Hand that given the deck and the current bank's hand
-- plays a move for the bank. The bank's logic is to draw if the current score is less than 16.

playBank :: Hand -> Hand -> Maybe Hand
playBank deck bankHand
    | valueHand bankHand >= 16 = Just bankHand
    | otherwise = case draw deck bankHand of
        Just (newDeck, newHand) -> playBank newDeck newHand
        Nothing -> Just bankHand 



----------TESTING-------------

-- Sample test cases for valueRank
valueRankTestCases :: [(Rank, Integer)]
valueRankTestCases = 
    [ (Ace, 11)
    , (King, 10)
    , (Queen, 10)
    , (Jack, 10)
    , (Numeric 5, 5)
    , (Numeric 10, 10)
    , (Numeric 2, 2)
    ]

-- Sample test cases for isBlackjack
isBlackjackTestCases :: [(Hand, Bool)]
isBlackjackTestCases = 
    [ ([Card Ace Hearts, Card (Numeric 10) Diamonds], True)
    , ([Card King Spades, Card Jack Clubs], False)  
    , ([Card (Numeric 5) Clubs, Card (Numeric 5) Hearts, Card Ace Diamonds], False)
    , ([Card Ace Diamonds, Card King Spades], True)
    ]

-- Sample test cases for gameOver
gameOverTestCases :: [(Hand, Bool)]
gameOverTestCases = 
    [ ([Card (Numeric 10) Clubs, Card King Spades, Card Queen Hearts], True)
    , ([Card (Numeric 5) Clubs, Card (Numeric 5) Hearts], False)
    ]

-- Sample test cases for valueHand (Mandatory)
valueHandTestCases :: [(Hand, Integer)]
valueHandTestCases = 
    [ ([Card Ace Hearts, Card (Numeric 10) Diamonds], 21)
    , ([Card King Spades, Card Queen Clubs], 20)
    , ([Card (Numeric 2) Clubs, Card (Numeric 3) Diamonds], 5)
    , ([Card (Numeric 7) Hearts, Card (Numeric 5) Spades, Card (Numeric 5) Clubs], 17)
    ]

-- Generic test function
runTest :: (Eq b, Show b, Show a) => (a -> b) -> (a, b) -> IO ()
runTest function (input, expected) =
    if function input == expected
    then putStrLn $ "Pass! " ++ show input
    else putStrLn $ "Fail! " ++ show input ++ ". Expected " ++ show expected ++ ", got " ++ show (function input)


-- Test runner for all defined tests
runAllTests :: IO ()
runAllTests = do
    putStrLn "Testing valueRank:"
    mapM_ (runTest valueRank) valueRankTestCases
    putStrLn "Testing isBlackjack:"
    mapM_ (runTest isBlackjack) isBlackjackTestCases
    putStrLn "Testing gameOver:"
    mapM_ (runTest gameOver) gameOverTestCases
    putStrLn "Testing valueHand:"
    mapM_ (runTest valueHand) valueHandTestCases

main :: IO ()
main = runAllTests


--------Employee Queries-------

-- Provided code

type EId = String

data Date = Date { year :: Int, month :: Int, day :: Int }
    deriving (Show, Eq)

instance Ord Date where
   compare :: Date -> Date -> Ordering
   compare (Date y1 m1 d1) (Date y2 m2 d2)
    | y1 /= y2 = compare y1 y2
    | m1 /= m2 = compare m1 m2
    | otherwise = compare d1 d2

data WorkPermit = Permit { number :: String, expiryDate :: Date }
    deriving (Show, Eq)

data Employee = Emp
  {
    empId :: EId,
    joinedOn :: Date,
    permit :: Maybe WorkPermit,
    leftOn :: Maybe Date
  }
  deriving (Show, Eq)

type EmployeeMap = [(EId, Employee)]

differenceInYears :: Date -> Date -> Int
differenceInYears (Date year1 _ _) (Date year2 _ _) = abs $ year1 - year2

-- Continuing code

--1
-- A function employeesToIdMap :: [Employee] -> EmployeeMap that pairs each employee with their id.

employeesToIdMap :: [Employee] -> EmployeeMap
employeesToIdMap employees = [(empId emp, emp) | emp <- employees]

--2
-- A function getWorkPermit :: EId -> EmployeeMap -> Maybe WorkPermit that given an employee id
-- returns the work permit.

getWorkPermit :: EId -> EmployeeMap -> Maybe WorkPermit
getWorkPermit empId empMap = lookup empId empMap >>= permit


--3
-- A function witNoPermit :: [Employee] -> [Employee] that returns employees that do not have a
-- permit.

withNoPermit :: [Employee] -> [Employee]
withNoPermit employees = [emp | emp <- employees, isNothing (permit emp)]

--4
-- A function withExpiredPermit :: [Employee] -> Date -> [EId] that given the current date returns the
-- ids of employees with an expired permit. You can compare dates with the operators (<, >=).

withExpiredPermit :: [Employee] -> Date -> [EId]
withExpiredPermit employees currentDate = 
    [ empId emp
    | emp <- employees
    , Just permit <- [permit emp]  
    , expiryDate permit < currentDate  
    ]

--5
-- A function avgYearsWorked :: [Employee] -> Double that returns the average years an employee
-- worked in the company. Consider only employees who have left. (In Haskell if you want to do real
-- number division to get a double using / and you have integers, there might need to be a conversion
-- using the built-in function fromIntegral which makes an integer a more general number. 

avgYearsWorked :: [Employee] -> Double
avgYearsWorked employees =
    let left = [emp | emp <- employees, Just leftDate <- [leftOn emp]]
        yearsWorked = [differenceInYears (joinedOn emp) leftDate | emp <- left, Just leftDate <- [leftOn emp]]
        totalYears = sum (map fromIntegral yearsWorked)
        count = fromIntegral (length yearsWorked)
    in if count > 0 then totalYears / count else 0.0