{- Lab 2A + 2B
   Date: 2022-11-16
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
import RunGame
import Test.QuickCheck ()
import System.Random ( StdGen, Random(randomR) )

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
displayHand (Add c h)  = displayCard c ++ BlackJack.displayHand h

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
-- Creating the function
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = Empty
(<+) hand Empty = hand
(<+) Empty hand = hand
(<+) (Add c1 h1) h2 = Add c1 (h1 <+ h2)

-- Test functions
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
      p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 =
      size (p1<+p2) == size p1 + size p2


-------------------------------------------------------------------------
-- B2
-------------------------------------------------------------------------
-- Builds a deck that have all cards

fullDeck :: Hand 
fullDeck = startFullDeck Empty

-- Combining all the different suits and values together
startFullDeck :: Hand -> Hand 
startFullDeck = deckHelper listOfHands
  where listOfHands = [Add c Empty | c <- 
                      [Card r s | r <- [Ace, King, Queen, Jack], s <- allSuits] ++ 
                      [Card r s | r <- [Numeric n | n <- [2..10]], s <- allSuits] ]
        allSuits = [Spades, Hearts, Diamonds, Clubs]

-- Using a helper function to make the list of hands to one hand
deckHelper :: [Hand] ->  Hand -> Hand 
deckHelper xs hand = foldr (<+) hand xs


-------------------------------------------------------------------------
-- B3 
-------------------------------------------------------------------------
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty p1 = error  "The deck is empty."
draw (Add c deck) p1 = (deck, Add c p1) -- Drawing the first card into the p1 hand


-------------------------------------------------------------------------
-- B4 
-------------------------------------------------------------------------

playBank :: Hand -> Hand
playBank deck = snd $ playBankHelper deck Empty

-- Check if the value of hand is over 16, if not, draws a new card
playBankHelper :: Hand -> Hand -> (Hand, Hand) 
playBankHelper deck hand | value hand < 16 = playBankHelper smallerDeck biggerHand
                         | otherwise = (deck, hand)
  where (smallerDeck, biggerHand) = draw deck hand

-------------------------------------------------------------------------
-- B5
-------------------------------------------------------------------------
-- Here it gets a little messy, we use a helper function
-- Which uses some other functions described below
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g deck = shuffleHelper g deck Empty

shuffleHelper :: StdGen -> Hand -> Hand -> Hand
shuffleHelper g deck newDeck 
            | size deck == 0 = newDeck
            | otherwise = shuffleHelper g' deck' (Add looseCard newDeck)
            where (looseCard, deck') = removeNthCard n deck
                  (n, g') = randomR (0, size deck-1) g

-- Uses the helper function which produces a list of hands
-- to be able to use list operations
removeNthCard :: Int -> Hand -> (Card, Hand)
removeNthCard n hand | n < 0 = error "n is too low!"
removeNthCard n hand | n > size hand - 1 = error "n is too high!" 
removeNthCard n hand = removeNthCardHelper 
                        (hands !! n, take n hands ++ drop (n+1) hands)
  where hands = buildListOfHands hand -- Need this to index the correct card to be "shuffled"

-- Build a list of hands 
buildListOfHands :: Hand -> [Hand]
buildListOfHands Empty = []
buildListOfHands (Add card hand) = Add card Empty:buildListOfHands hand

-- Removes a layering, gives us (Card, Hand)
-- instead of (Hand, [Hand])
-- such that we don't get errors in function definitions later
-- Won't write error checks since we know we will get correct Hand, [Hand]
-- (it complained in the test checks on Fire)
removeNthCardHelper :: (Hand, [Hand]) -> (Card, Hand)
removeNthCardHelper (Add c1 Empty, hands) = (c1, largeHand) 
  where largeHand = deckHelper hands Empty


-- Just checking if the cards are still in the deck
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Needed helper functions
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Checks if the sizes differ before shuffling and after
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size (shuffleDeck g h) == size h 

-------------------------------------------------------------------------
-- B6
-------------------------------------------------------------------------
-- Doing as instructed in the assignment
implementation :: Interface
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = BlackJack.displayHand -- clashes with the other displayHand
  , iGameOver = gameOver              -- otherwise
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation