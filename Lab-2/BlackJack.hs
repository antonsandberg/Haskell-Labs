module BlackJack where
import Cards
    ( Hand(..),
      Suit(Clubs, Hearts, Spades, Diamonds),
      Rank(Ace, Jack, Numeric),
      Card(..),
      rank,
      size )
import RunGame ( Player(..) )
import Test.QuickCheck ()


-- Creating some example hands for checking
hand2 :: Hand
hand2 = Add (Card (Numeric 2) Hearts)
                (Add (Card Jack Spades) Empty)
hand3 :: Hand
hand3 = Add (Card (Numeric 5) Hearts)
                (Add (Card Jack Spades) Empty)
card2 :: Card
card2 = Card (Numeric 2) Hearts
acesHand :: Hand
acesHand = Add (Card Ace Hearts)
                (Add (Card Ace Spades) Empty)

-------------------------------------------------------------------------
-- A0
-------------------------------------------------------------------------
-- Completing the sequence by recursively calculate the size
-- Answer should be [2, 2, 2, 2, 2]
sizeSteps :: [Integer]
sizeSteps = [size hand2
            , size (Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]
-------------------------------------------------------------------------
-- A1
-------------------------------------------------------------------------

-- Pattern matching wether it's a numeric value or a ranked card 
-- Since we don't want to print "Numeric 10" but "10"
-- We display card without unicode
displayCard :: Card -> String
displayCard (Card (Numeric v) s)    = show v ++ " of " ++ show s ++ "\n"
displayCard (Card r s)              = show r ++ " of " ++ show s ++ "\n"

-- Using displayCard to add together the hand (clashed with displayHand
-- in the other file so renaming it)
displayHand :: Hand -> String
displayHand Empty      = ""
displayHand (Add c h)  = displayCard c ++ displayHand h

-------------------------------------------------------------------------
-- A2
-------------------------------------------------------------------------
-- Calculating value, Assume Ace = 11
initialValue :: Hand -> Integer
initialValue Empty      = 0 
initialValue (Add c h)  = valueRank (rank c) + initialValue h

-- Simple function to determine each cards numeric value
-- using pattern matching
valueRank :: Rank -> Integer
valueRank (Numeric r)   = r
valueRank Ace           = 11
valueRank _             = 10  

-- Simple recursive way to check number of aces
numberOfAces :: Hand -> Integer
numberOfAces Empty                  = 0
numberOfAces (Add (Card Ace _) h)   = 1 + numberOfAces h
numberOfAces (Add _ h)              = 0 + numberOfAces h

-- Use our numberOfAces function to compute our correct value
value :: Hand -> Integer
value Empty = 0
value h     | initialValue h > 21 = initialValue h - 10*numberOfAces h
            | otherwise = initialValue h

-------------------------------------------------------------------------
-- A3
-------------------------------------------------------------------------
-- Check if a hand is bust
gameOver :: Hand -> Bool
gameOver h  | value h > 21  = True  
            | otherwise     = False

-------------------------------------------------------------------------
-- A4
-------------------------------------------------------------------------
-- Check if the guest or the bank has won, by using the rules of the game
-- If guest is bust -> Bank
-- If bank is bust and guest isn't -> Guest
-- Now we know that neither are bust: can check values
-- The guest's only win condition is if it has a strictly higher score -> Guest
-- otherwise we have that -> Bank

winner :: Hand -> Hand -> Player
winner guest _     | gameOver guest = Bank
winner guest bank  | not (gameOver guest) && gameOver bank = Guest              
                   | value guest > value bank = Guest
                   | otherwise = Bank