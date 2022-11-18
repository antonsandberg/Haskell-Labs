module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List
import Control.Monad



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
allBlankSudoku = Sudoku $ replicate 9 $ take 9 $ repeat Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = isCorrectSize s && and (map isCorrectElem (concat $ rows s))

isCorrectSize :: Sudoku -> Bool
isCorrectSize s =  all isSizeNine (rows s) && isSizeNine (rows s)
  where isSizeNine n = length n == 9

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
printSudoku s = mapM_ putStrLn (parseSudoku s)

parseSudoku :: Sudoku -> [String]
parseSudoku s = [parseRow x | x  <- rows s]

parseRow :: [Cell] -> String
parseRow ss = [parseElem s | s <- ss] 

parseElem :: Maybe Int -> Char
parseElem (Just x) = intToDigit x
parseElem Nothing = '.'

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku

readSudoku :: FilePath -> IO Sudoku
readSudoku f = do x <- readfile f
                  let sudokuList = lines x  
                  let sudoku = Sudoku {rows = map rowsToList sudokuList}
                  if (isSudoku sudoku) then return sudoku
                  else return error "This is not a valid SUDOKU!"

 rowsToList :: [Char] -> [Cell]
 rowsToList s = map charToCell s

 charToCell :: Char ->  Cell
 charToCell '.'  = Nothing
 charToCell  c   =  Just (digitToInt c)
 
      
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
{-
cell :: Gen (Cell)
cell = frequency [(9, liftM Nothing), (1, liftM Just choose (1, 9) )]
-}



-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = undefined

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple! (prob not this simple though)
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell
-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b = length (nubBy (\e1 e2 -> e1==e2 && isNothing e2) b) == 9
  

-- CHECK ON THIS DURING THE WEEKEND 
-- * D2
-- blocks :: Sudoku -> [Block]
-- blocks s = groupByThree $ concat $ transpose $ map groupByThree $ rows s

-- Need to fix the definition of this
groupByThree :: [Row] -> [[Row]]
groupByThree (e1:e2:e3:rest) = [e1, e2, e3] : groupByThree rest
groupByThree []         = []


-- Not sure if this is correct since this is an exact replica of 
-- what we've done previously and we might be suppose to do something else
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s =  all isSizeNine (rows s) && isSizeNine (rows s)
  where isSizeNine n = length n == 9

-- * D3
-- Check if the regular sudoku is a good board
-- Check if a transposed board is a good board
-- And check if a block made board is a good board <- STILL NEED TO DO THIS
isOkay :: Sudoku -> Bool
isOkay s = isSudoku s && isSudoku (Sudoku $ transpose $ rows s)




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