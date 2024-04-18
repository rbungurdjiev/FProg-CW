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