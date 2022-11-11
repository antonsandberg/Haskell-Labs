{- Lab 2A
   Date: 2022-11-09
   Authors: Zozk Mohamed and Anton Sandberg
   Lab group: - 48
 -}

module BlackJack where
import Cards
    ( Hand(..),
      Suit(Clubs, Hearts, Spades, Diamonds),
      Rank(Ace, King, Queen, Jack, Numeric),
      Card(..),
      rank,
      size )
import RunGame ( Player(..) )
import Test.QuickCheck ()
import System.Random

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

-- Using displayCard to add together the hand 
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
-- Check if a hand is bust (hand value over 21)
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


-- We created some example hands for checking our functions
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
-- B1
-------------------------------------------------------------------------
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) hand Empty = hand
(<+) Empty hand = hand
(<+) (Add c1 h1) h2 = Add c1 (h1 <+ h2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
      p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
      size (p1<+p2) == size p1 + size p2

-------------------------------------------------------------------------
-- B2
-------------------------------------------------------------------------
-- It's a mess, but its works!
fullDeck :: Hand 
fullDeck = startFullDeck Empty

startFullDeck :: Hand -> Hand 
startFullDeck hand = deckHelper listOfHands hand
  where listOfHands = [Add c Empty | c <- 
                      [Card r s | r <- [Ace, King, Queen, Jack], s <- allSuits] ++ 
                      [Card r s | r <- [Numeric n | n <- [2..10]], s <- allSuits] ]
        allSuits = [Spades, Hearts, Diamonds, Clubs]

deckHelper :: [Hand] ->  Hand -> Hand 
deckHelper [] hand = hand
deckHelper (x:xs) hand = x <+ (deckHelper xs hand)


-----------------------------------------------
-- B3 
-----------------------------------------------

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty p1 = error  "The deck is empty."
draw (Add c deck) p1 = (deck, Add c p1) -- Drawing the first card into the p1 hand


-----------------------------------------------
-- B4 
-----------------------------------------------
second :: (a, b) -> b
second (x,y) = y

first :: (a, b) -> a
first (x,y) = x

playBank :: Hand -> Hand
playBank deck = second $ playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> (Hand, Hand) 
playBankHelper deck hand | value hand < 16 = playBankHelper smallerDeck biggerHand
                         | otherwise = (deck, hand)
  where (smallerDeck, biggerHand) = draw deck hand

-----------------------------------------------
-- B5
-----------------------------------------------
-- Wants a g a deck and an empty hand
-- and spits out a shuffled hand
-- Use removeNthCard to grab a random card from current deck
-- and adding it to new hand where it will shuffled

--getRandom :: Int -> Int -> IO Int
--getRandom lo hi = getStdRandom (randomR (lo, hi))

--mkStdGen :: Int -> StdGen
--let gen = mkStdGen 2022

-- deck first shuffled hand later returns shuffled hand
shuffleDeck :: StdGen -> Hand -> Hand -> Hand
--shuffleDeck g Empty fullHand = fullHand 
--shuffleDeck g deck newDeck = Add looseCard newDeck
 -- where (looseCard, deck) = removeNthCard n deck
  --      (n, g1) = randomR (0, size deck - 1) g


shuffleDeck g deck newDeck 
            | size deck == 0 = newDeck
            | otherwise = shuffleDeck g' deck' (Add looseCard newDeck)
            where (looseCard, deck') = removeNthCard n deck'
                  (n, g') = randomR(0, size deck -1) g

-- Want to add cards into the new hand from the "deck"
-- until we are at the right index, then remove that card
-- from deck and add into tuple
-- 
removeNthCard :: Int -> Hand -> (Card, Hand)
removeNthCard n hand | n < 0 = error "n is too low!"
removeNthCard n hand | n > size hand - 1 = error "n is too high!" 
removeNthCard n hand = removeNthCardHelper 
                        (hands !! n, take (n-1) hands ++ drop (n+1) hands)
  where hands = buildListOfHands hand

-- Removes a layering, gives us (Card, Hand)
-- instead of (Hand, Hands)
removeNthCardHelper :: (Hand, [Hand]) -> (Card, Hand)
removeNthCardHelper (Add c1 Empty, hands) = (c1, largeHand) 
  where largeHand = deckHelper hands Empty

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g h c = undefined
  --c `belongsTo` h == c `belongsTo` shuffleDeck g h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle = undefined
--prop_size_shuffle g h = size (shuffleDeck g h) == size h 

-- Build a list of hands 
buildListOfHands :: Hand -> [Hand]
buildListOfHands Empty = []
buildListOfHands (Add card hand) = Add card Empty:buildListOfHands hand






------------------------------------------------------------
-- B6
------------------------------------------------------------
{-
implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation
-}