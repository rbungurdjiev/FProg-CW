

-- Coursework - Functional Programming
-- Students:
-- CSY22083
-- CSY22042
-- CSY22077
-- CSY22053
-- CSY22057


-- BLACKJACK  GAME IMPLEMENTATION 

-- Types

-- Suit, the suit of a card: hearts, spades, diamonds, and clubs.

data Suit = Hearts | Spades | Diamonds | Clubs 
    deriving (Eq, Show)

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
    [card1, card2] -> valueHand hand == 21  
    _              -> False


--9
-- Function to determine if a hand is over (loses by busting)
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

--10
-- Function to determine the winner between guest and bank hands
winner :: Hand -> Hand -> Player
winner guestHand bankHand
    | guestBust = Bank                -- Guest busts, bank wins
    | bankBust = Guest                -- Bank busts, guest wins
    | guestValue > bankValue = Guest  -- Guest has higher value, guest wins
    | otherwise = Bank                -- Tie or bank has higher value, bank wins
    where
        guestValue = value guestHand    -- Calculate total value of guest's hand
        bankValue = value bankHand      -- Calculate total value of bank's hand
        guestBust = gameOver guestHand  -- Check if guest busts
        bankBust = gameOver bankHand    -- Check if bank busts

--11
-- Define an operator (<+) to concatenate two hands
infixr 5 <+  -- Set fixity and precedence level
(<+) :: Hand -> Hand -> Hand
hand1 <+ hand2 = hand1 ++ hand2

--12
-- Function to generate a hand with all 13 cards of a given suit
handSuit :: Suit -> Hand
handSuit suit = [Card rank suit | rank <- allRanks]
    where
        allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

--13
-- Define a complete deck of 52 cards using list comprehensions
fullDeck :: Hand
fullDeck = [Card rank suit | suit <- allSuits, rank <- allRanks]
    where
        allSuits = [Hearts, Spades, Diamonds, Clubs]  -- List of all suits
        allRanks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]  -- List of all ranks

--14
-- Function to draw a card from a deck and add it to a hand
draw :: Hand -> Hand -> (Hand, Hand)
draw deck hand
        | null deck = error "Cannot draw from an empty deck."  -- Error handling for empty deck
        | otherwise = (tail deck, (head deck) : hand)          -- Return the updated deck and hand


--15

-- Function to play a move for the bank. The bank draws cards until its score is at least 16
playBank :: Hand -> Hand -> Hand
playBank deck bankHand
        | value bankHand < 16 = playBank deck' bankHand'  -- Keep drawing if value less than 16
        | otherwise = bankHand                            -- Stop drawing if value is 16 or more
    where
        (deck', bankHand') = draw deck bankHand           -- Draw one card from the deck to the hand

----------------------------------------------------------
--Fitst install

--cabal update
--cabal install HUnit
--cabal install --lib HUnit


----------TESTING-------------
import Test.HUnit
import Data.List

-- Test data
aceCard :: Card
aceCard = Card Ace Hearts
kingCard :: Card
kingCard = Card King Spades
numericCard5 :: Card
numericCard5 = Card (Numeric 5) Diamonds
handWithAces :: [Card]
handWithAces = [aceCard, numericCard5, aceCard]
handWithoutAces :: [Card]
handWithoutAces = [kingCard, numericCard5]

-- Test cases
testValueRank = TestList [
    TestCase (assertEqual "Ace should be 11" 11 (valueRank Ace)),
    TestCase (assertEqual "King should be 10" 10 (valueRank King)),
    TestCase (assertEqual "Numeric 5 should be 5" 5 (valueRank (Numeric 5)))
  ]

testValueCard = TestList [
    TestCase (assertEqual "Value of Ace of Hearts should be 11" 11 (valueCard aceCard)),
    TestCase (assertEqual "Value of King of Spades should be 10" 10 (valueCard kingCard)),
    TestCase (assertEqual "Value of Numeric 5 of Diamonds should be 5" 5 (valueCard numericCard5))
  ]

testValueHand = TestList [
    TestCase (assertEqual "Value of hand without Aces" 15 (valueHand handWithoutAces)),
    TestCase (assertEqual "Value of hand with Aces before adjustment" 27 (valueHand handWithAces))
  ]

testValue = TestList [
    TestCase (assertEqual "Adjusted value with Aces" 17 (value handWithAces)),
    TestCase (assertEqual "Value without adjustment needed" 15 (value handWithoutAces))
  ]

-- Running all tests
runTests = runTestTT $ TestList [testValueRank, testValueCard, testValueHand, testValue]
