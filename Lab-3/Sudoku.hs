{- Lab 3A 
   Date: 2022-11-21
   Authors: Zozk Mohamed and Anton Sandberg
   Lab group: - 48
 -}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}
module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List.Split
import Control.Monad
import Data.List

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 5,n, n   ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
    Sudoku
      [ [j 5,j 6,j 1  ,j 1  ,j 7,j 1,j 5, j 3, j 9   ]
      , [j 3  ,j 5,j 9  ,j 9  ,j 9  ,j 9  ,j 1,j 8,j 9  ]
      , [j 9  ,j 9  ,j 9,j 2,j 9  ,j 4,j 7,j 9  ,j 9  ]
      , [j 9  ,j 9  ,j 9  ,j 9  ,j 1,j 3,j 9  ,j 2,j 8]
      , [j 4,j 9  ,j 9  ,j 5,j 9  ,j 2,j 9  ,j 9  ,j 9]
      , [j 2,j 7,j 9  ,j 4,j 6,j 9  ,j 9  ,j 9  ,j 9  ]
      , [j 9  ,j 9  ,j 5,j 3,j 9  ,j 8,j 9,j 9  ,j 9  ]
      , [j 9  ,j 8,j 3,j 9  ,j 9  ,j 9  ,j 9  ,j 6,j 9  ]
      , [j 9  ,j 9  ,j 7,j 6,j 9,j 9  ,j 9  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just


goodBlock :: Row
goodBlock =  [j 3,j 6,n  ,n  ,j 7,j 1,j 5,n, n, n]
  where
    n = Nothing
    j = Just
badBlock :: Row
badBlock =  [j 3,j 3,n  ,n  ,j 7,j 1,j 5,n, n   ]
  where
    n = Nothing
    j = Just
-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = isCorrectSize s && all isCorrectElem (concat $ rows s)

-----------------------------------------------------------------------
isCorrectSize :: Sudoku -> Bool
isCorrectSize s =  all isSizeNine (rows s) && isSizeNine (rows s)

isSizeNine :: [a] -> Bool
isSizeNine l = length l == 9
-----------------------------------------------------------------------
isCorrectElem :: Maybe Int -> Bool
isCorrectElem (Just x) = x `elem` [1..9]
isCorrectElem  Nothing = True

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = all isJust (concat $ rows s)

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = mapM_ putStrLn (parseSudoku s) where
  parseSudoku :: Sudoku -> [String]
  parseSudoku s = [parseRow x | x  <- rows s]
  -------------------------------------------
  parseRow :: [Cell] -> String
  parseRow ss = [parseElem s | s <- ss]
  -------------------------------------------
  parseElem :: Maybe Int -> Char
  parseElem (Just x) = intToDigit x
  parseElem Nothing = '.'

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku

readSudoku :: FilePath -> IO Sudoku
readSudoku f = do x <- readFile f
                  let sudokuList = lines x
                  let sudoku = Sudoku (map rowsToList sudokuList)
                  if isSudoku sudoku then return sudoku -- <- Doing the mandatory check
                  else error "Stop importing an invalid Sudoku!" where
                    --------------------------------------------------
                    rowsToList :: [Char] -> [Cell]
                    rowsToList = map charToCell
                    --------------------------------------------------
                    charToCell :: Char ->  Cell
                    charToCell '.'  = Nothing               -- if . return Nothing on it's place
                    charToCell  c   =  Just (digitToInt c)  -- Else return J
                                                            -- ust combined with a conversion of the cell
                              --The two functions below are just reverse of what we've done previously
                              --So for every Row in our s we map the charToCell function




------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku


-- hlint recommended fmap, and need to use return
-- since we only have one value for the other
-- Using choose from the lectures as well as
-- providing the 80/20 prob for the two different
-- "cell types"
cell :: Gen Cell
cell = frequency [(1, fmap Just (choose (1, 9))), (9, return Nothing)]


-- * C2
-- | an instance for generating Arbitrary Sudokus
-- Double use of vectorOf to get our 9x9 board of cells

instance Arbitrary Sudoku where
  arbitrary :: Gen Sudoku
  arbitrary = do s <- vectorOf 9 (vectorOf 9 cell)
                 return (Sudoku s)


-- * C3
-- Guess this is it, seems redundant however
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1
-- If we have no dups the whole anyJustDups will be false which we will get to true
-- If there is at least one dup we will get at least one true which we want to be false
-- Sort to be able to compare them next to each other
isOkayBlock :: Block -> Bool
isOkayBlock b = not (or (anyJustDups (sort b))) where
  ----------------------------------------------------------------------
  anyJustDups :: Block -> [Bool]
  anyJustDups (Nothing:Nothing:rest) = False : anyJustDups (Nothing:rest)
  anyJustDups (e1:e2:rest) = (e1 == e2) : anyJustDups (e2:rest)
  anyJustDups lastItem = [False] -- Will only get here if there is only one item left


-- * D2
-- had to import chunksOf from Data.List.Split (which I installed manually)
-- Just a mixture of the chunksOf function, concat and transpose
-- To get the elements in their desired spots
threeXthree :: Sudoku -> [Row]
threeXthree s = map concat $ chunksOf 3 $ concat $ transpose $ map (chunksOf 3) $ rows s

-- If we transpose the row of rows we get the columns as rows
-- Which means we don¨t have to create other new functions
columns :: Sudoku -> [Row]
columns s = transpose $ rows s

-- Adding all three versions together to get the 27 block "mega Sudoku"
blocks :: Sudoku -> [Block]
blocks s = rows s ++ columns s ++ threeXthree s

-- Just using the iscorrectSize together with some transposing and blocking
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = all isSizeNine $ blocks s

-- * D3
-- Just making use of our blocks function to create the 3*9 blocks and then checking
-- with help of our 9 length check function
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock $ blocks s


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

-- Gives a list of all postions were the sudoku is blank
-- Using a list comprehension and some simple math
-- to grab all the indices in tuples
blanks :: Sudoku -> [Pos]
blanks s = [(x `div` 9, x `mod` 9) | x <- elemIndices Nothing $ concat $ rows s]

-- Just checkign the length of it's a "whole" Sudoku
prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 9*9

-- * E2

(!!=) :: [a] -> (Int, a) -> [a]
(x:xs) !!= (0,y) = y:xs
(x:xs) !!= (i,y) = x: xs !!= (i-1,y)


prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct x pair = a && b where
  a = length (x !!= pair) == length x
  b =  ((x !!= pair) !! fst pair) == snd pair

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update s (row, col) c = Sudoku $ updateHelper (rows s) (row, col) c

updateHelper :: [Row] -> Pos -> Cell -> [Row]
updateHelper (r:rs) (0, col) c = (r !!= (col, c)) : rs
updateHelper (r:rs) (row, col) c = r : updateHelper rs (row-1, col) c


--prop_update_updated :: Sudoku -> (Int, Int) -> Cell -> (a, Int) -> Cell -> [Bool]
--prop_update_updated :: Sudoku -> (Int, Cell) -> t1 -> t2
prop_update_updated :: Sudoku -> (Int, Int) -> Cell -> Bool
prop_update_updated s (row, col) c = prop_bangBangEquals_correct (last (take (row+1) (rows (update s (row, col) c)))) (col, c)

------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve s | not (isOkay s) = Nothing  -- First checking if the Sudoku is okay
         | null (blanks s) = Just s -- Sudoku is already finished

         -- Otherwise fill the first available tile in all different ways and then continue from there
         -- catMaybe takes care of the Nothings (that might be produced) that will get produced and listToMaybe grabs
         -- the first of these solutions
         | otherwise = listToMaybe $ catMaybes [solve oneFilled | oneFilled <- allOneFilledSudokus] where
                       allOneFilledSudokus = [update s (head (blanks s)) (Just x) | x <- [1..9]]

-- * F2
-- produces instructions for reading the Sudoku from the given file, 
-- solving it, and printing the answer
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
                       sudoku <- readSudoku path
                       let solvedSudoku = solve sudoku
                       printSolvedSudoku solvedSudoku
                       where printSolvedSudoku :: Maybe Sudoku -> IO ()
                             printSolvedSudoku Nothing = putStrLn "No solution found"
                             printSolvedSudoku s       = printSudoku $ fromJust s

-- * F3
-- that checks, given two Sudokus, whether the first one is a solution 
-- (i.e. all blocks are okay, there are no blanks), and also whether the first one is a solution of the second one 
-- (i.e. all digits in the second sudoku are maintained in the first one).
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isOkay s1 && null (blanks s1) && all (== True) (isSolutionOfHelp (concat(rows s1)) (concat(rows s2)))
  where isSolutionOfHelp :: [Cell] -> [Cell] -> [Bool]
        isSolutionOfHelp [] [] = []
        isSolutionOfHelp (_:cs1) (Nothing:cs2) = True : isSolutionOfHelp cs1 cs2
        isSolutionOfHelp (c1:cs1) (c2:cs2) = (c1 == c2) : isSolutionOfHelp cs1 cs2

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s | isNothing (solve s) = property True -- if Sudoku can't be solved
                  | otherwise = property $ isSolutionOf (fromJust $ solve s) s -- isSolutionOf check that sudoku s is valid 
                                                                               -- and that solved sudoku solved s