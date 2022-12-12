module Expr where

{- Lab 4
   Date: 2022-12-14
   Authors: Zozk Mohamed and Anton Sandberg
   Lab group: - 48
 -}
{-# LANGUAGE InstanceSigs #-}

--import Data.Aeson.Encoding (value)
import Parsing
import Data.Char
import Test.QuickCheck
import Data.Maybe
import Data.Functor


expr1 = Mul (Add (Num 3) (Num 4)) (Add (Add X (Num 3)) (Sin (Add (Num 3) (Num 4))))

expr2 = Add (Mul (Num 3) (Num 4)) (Add (Add X (Num 3)) (Cos (Add (Num 3) (Num 4))))

expr3 = Sin X


-------------------------------------------------------------
-- *A
-------------------------------------------------------------

-- Recursive datatype that represents expressions 
data Expr = Num Double
            | Add Expr Expr
            | Mul Expr Expr
            | Sin Expr
            | Cos Expr
            | X
    deriving Eq

-- In order that we can run tests 

x :: Expr
x = X

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Add
mul = Mul

sin,cos :: Expr -> Expr
sin = Sin
cos = Cos



-- Counts the number of functions and operators in the given expression
size :: Expr -> Int
size (Num _)      = 1
size X            = 1
size (Add e1 e2)  = 1 + size e1 + size e2
size (Mul e1 e2)  = 1 + size e1 + size e2
size (Sin e)      = 1 + size e
size (Cos e)      = 1 + size e

-------------------------------------------------------------
-- *B 
-------------------------------------------------------------

-- Converts any expression to a string
showExpr :: Expr -> String
showExpr (Num n)      = show n
showExpr (Sin e)      = "sin " ++ showFactorCosSin e
showExpr (Cos e)      = "cos " ++ showFactorCosSin e
showExpr X            = "x"
showExpr (Add e1 e2)  = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul a b)    = showFactor a ++ " * " ++ showFactor b
     where showFactor :: Expr -> String
           showFactor (Add a b) = "("++showExpr (Add a b)++")"
           showFactor e         = showExpr e

showFactorCosSin :: Expr -> String
showFactorCosSin (Num n)  = showExpr (Num n)
showFactorCosSin (Sin a)  = showExpr (Sin a)
showFactorCosSin (Cos a)  = showExpr (Cos a)
showFactorCosSin X        = showExpr X
showFactorCosSin e        = "(" ++ showExpr e ++ ")"

instance Show Expr where
  show = showExpr

-------------------------------------------------------------
-- *C
-------------------------------------------------------------

-- Given an expression, and the value for the variable x, calculates the value of the expression
eval :: Expr -> Double -> Double
eval (Num n) _      = n
eval X v            = v

eval (Add e1 e2) v  = eval e1 v + eval e2 v
eval (Mul e1 e2) v  = eval e1 v * eval e2 v
eval (Sin e) v      = Prelude.sin $ eval e v
eval (Cos e) v      = Prelude.cos $ eval e v

-------------------------------------------------------------
-- *D
-------------------------------------------------------------

readExpr :: String -> Maybe Expr
readExpr s = parsing $ parse expr $ removeSpaces s
  where parsing (Just (e,[]))         = Just e
        parsing (Just (_,noneEmpty))  = Nothing
        parsing Nothing               = Nothing

expr, term, factor, parseX, parseSin, parseCos :: Parser Expr
parseX    = char 'x' *> return X
parseSin  = char 's' *> char 'i' *> char 'n' *> (Sin <$> factor)
parseCos  = char 'c' *> char 'o' *> char 's' *> (Cos <$> factor)
expr      = foldl1 Add <$> chain term (char '+')
term      = foldl1 Mul <$> chain factor (char '*')
factor    = parseX <|> (Num <$> readsP) <|> char '(' *> expr <* char ')' <|> parseSin <|> parseCos

removeSpaces :: [Char] -> [Char]
removeSpaces = filter (not . isSpace)

-------------------------------------------------------------
-- *E
-------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- says that first showing and then reading should yield the same
-- as expression
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e | isNothing (readExpr (showExpr e)) = True
                    | otherwise                         = showExpr (fromJust (readExpr (showExpr e))) == showExpr e

arbExpr :: Int -> Gen Expr
--arbExpr i = frequency [(8, do Num <$> choose(0, 99)), (2, do return X), (1, do let e = arbExpr (i 'div' 2) return (Cos e) )]
arbExpr i = frequency [(3, do Num <$> choose(-100, 100)), (3, do return X), 
                      (i, genSin i), (i, genCos i), 
                      (i, genMul i), (i, genAdd i)]
  where 
    -- Either choose sin or cos and i `div` 2
    genSin j = do 
      e <- arbExpr (j `div` 2)
      return $ Sin e
    
    genCos j = do
      e <- arbExpr (j `div` 2)
      return $ Cos e

    genAdd j = do
      e1 <- arbExpr (j `div` 2)
      e2 <- arbExpr (j `div` 2)
      return $ Add e1 e2  
  
    genMul j = do
      e1 <- arbExpr (j `div` 2)
      e2 <- arbExpr (j `div` 2)
      return $ Mul e1 e2   

-------------------------------------------------------------
-- *F
-------------------------------------------------------------
-- Will just pattern match into oblivion and maybe do some
-- more general stuff later, not even sure that it's possible though

simplify :: Expr -> Expr
simplify e = do
  let simplified = simplifyHelper e
  if simplified == e then simplified else simplify simplified


simplifyHelper :: Expr -> Expr
-- Base cases (not sure if they are necessary)
simplifyHelper (Num x) = Num x
simplifyHelper X = X

-- Structure them in different operators for some structure
simplifyHelper (Add (Num x1) (Num 0.0)) = Num x1
simplifyHelper (Add (Num 0.0) (Num x2)) = Num x2
simplifyHelper (Add (Num x1) (Num x2))  = Num (x1+x2)
simplifyHelper (Add X (Num 0.0))        = X
simplifyHelper (Add e1 (Num 0.0))       = simplifyHelper e1
simplifyHelper (Add (Num 0.0) e2)       = simplifyHelper e2
-- Otherwise
simplifyHelper (Add e1 e2)              = Add (simplifyHelper e1) (simplifyHelper e2)

simplifyHelper (Mul (Num x1) (Num x2))  = Num (x1*x2)
simplifyHelper (Mul _ (Num 0.0))        = Num 0
simplifyHelper (Mul (Num 0.0) _)        = Num 0
simplifyHelper (Mul (Num 1.0) e2)       = simplifyHelper e2
simplifyHelper (Mul e1 (Num 1.0))       = simplifyHelper e1
-- Otherwise
simplifyHelper (Mul e1 e2)              = Mul (simplifyHelper e1) (simplifyHelper e2)

-- Don't think there is much to do for cos/sin
-- however still have to declare them otherwise
-- we won't be able to use the function
simplifyHelper (Sin e) = Sin (simplifyHelper e)
simplifyHelper (Cos e) = Cos (simplifyHelper e)

-------------------------------------------------------------
-- *G
-------------------------------------------------------------

-- Think we will just patter match this for all different 
-- variations starting with the simple ones and moving up to the 
-- "harder" ones (for example multiplication and sin/cos)
-- We also want to make use of the simplification
-- function before differentiate to be able to 
-- not work the function more than necessary
-- When we do it recursively we also take care 
-- of the inner derivative (we think lol)

-- Add the fact that 2x+4x = 6x etc if there is time

differentiate :: Expr -> Expr
differentiate e = simplify $ differentiateHelper e


-- Should be done
differentiateHelper :: Expr -> Expr
differentiateHelper (Num _) = Num 0.0
differentiateHelper X = Num 1.0
differentiateHelper (Add e1 e2) = Add (differentiateHelper e1) (differentiateHelper e2)

-- Multiplication rule
differentiateHelper (Mul e1 e2) = Add (Mul (differentiateHelper e1) e2) (Mul e1 (differentiateHelper e2))
-- Cos and sin rules
differentiateHelper (Sin e) = Mul (Cos e) (differentiateHelper e)
differentiateHelper (Cos e) = Mul (Num (-1.0)) (Mul (Sin e) (differentiateHelper e))
