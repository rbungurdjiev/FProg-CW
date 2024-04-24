

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
size hand = fromIntegral (length hand)
--2
-- Define a function faceCards :: Hand -&gt; Integer that returns the number of face cards in the hand.

faceCards :: Hand -> Integer
faceCards hand = fromIntegral . length $ filter isFaceCard hand
  where
    isFaceCard :: Card -> Bool
    isFaceCard (Card {rank = Jack})  = True
    isFaceCard (Card {rank = Queen}) = True
    isFaceCard (Card {rank = King})  = True
    isFaceCard _                     = False

--3
    -- Function to determine the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric n) = fromIntegral n
valueRank Jack        = 10
valueRank Queen       = 10
valueRank King        = 10
valueRank Ace         = 11

--4
-- Function to determine the value of a card based on its rank
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

--5
-- Function to count the number of Aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces hand = fromIntegral . length $ filter isAce hand
    where
        isAce :: Card -> Bool
        isAce (Card {rank = Ace}) = True
        isAce _                   = False

--6
-- Function to calculate the total value of a hand
valueHand :: Hand -> Integer
valueHand hand = let
        totalInitial = sum (map valueCard hand)  -- Calculate initial total value without ace adjustments
        aceCount = numberOfAces hand            -- Count the number of Aces in the hand
    in adjustForAces totalInitial aceCount    -- Adjust total value if needed

-- Helper function to adjust the total value of the hand based on the number of Aces
adjustForAces :: Integer -> Integer -> Integer
adjustForAces total aces
        | total > 21 && aces > 0 = adjustForAces (total - 10) (aces - 1)  -- Convert an Ace from 11 to 1
        | otherwise = total

--7
-- Function to calculate the total value of a hand, adjusting aces if the total exceeds 21
value :: Hand -> Integer
value hand = adjustForAces (sum (map valueCard hand)) (numberOfAces hand)

--8
-- Function to determine if a hand is a blackjack
isBlackjack :: Hand -> Bool
isBlackjack hand = length hand == 2 && value hand == 21

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

