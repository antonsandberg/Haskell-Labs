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
  where isSizeNine n = length n == 9
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


-- hlint recommmended fmap, and need to use return
-- since we only have one value for the other
-- Using choose from the lectures as well as
-- providing the 90/10 prob for the two different
-- "cell types"
cell :: Gen Cell
cell = frequency [(9, fmap Just (choose (1, 9))), (1, return Nothing)]


-- * C2

-- | an instance for generating Arbitrary Sudokus
-- Double use of vectorOf to get our 9x9 board of cells
instance Arbitrary Sudoku where  
  arbitrary :: Gen Sudoku
  arbitrary = do s <- vectorOf 9 (vectorOf 9 cell)
                 return (Sudoku s)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

-- Guess this is it, seems redundant however
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell
-- * D1

-- nubBy function to be able to remove duplicates but only
-- If none of them are empty
isOkayBlock :: Block -> Bool
isOkayBlock b = length (nubBy (\e1 e2 -> e1==e2 && isNothing e2) b) == 9
  

-- had to import chunksOf from Data.List.Split (which I installed manually)
-- Just a mixture of the chunksOf function, concat and transpose
-- To get the elements in their desired spots
-- * D2
blocks :: Sudoku -> [Block]
blocks s = map concat $ chunksOf 3 $ concat $ transpose $ map (chunksOf 3) $ rows s


-- Prob want to remove this since I got Data.List.Split to work
{- 
chunksOf' :: Int -> [a] -> [[a]] 
chunksOf' _ [] = []
chunksOf' i l | i > 0 = take i l : chunksOf' i (drop i l)
              | otherwise = error "chunk size is less than 0"

-- Wanted to use Split.chunkOf for this but couldnt import it
-- So just defining it on my own (this is messy as we sont really understand it)
build :: ((a1 -> [a1] -> [a1]) -> [a2] -> t) -> t
build g = g (:) []

chunksOf'' :: Int -> [a] -> [[a]]
chunksOf'' i ls = map (take i) (build (splitter ls)) where
                splitter [] _ n = n
                splitter l c n = l `c` splitter (drop i l) c n
-}

-- Just using the iscorrectSize function since I think that's what we want
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths =  isCorrectSize

-- * D3
-- Check if the regular sudoku is a good board
-- Check if a transposed board is a good board
-- Check if a blocked board is a good board
-- This gets quite ugly since we need to row the s and then re Sudoku it
isOkay :: Sudoku -> Bool
isOkay s = isSudoku s && cols' s && blocks' s where 
  cols'   s = isSudoku $ Sudoku $ transpose $ rows s
  blocks' s = isSudoku $ Sudoku $ blocks s 


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
