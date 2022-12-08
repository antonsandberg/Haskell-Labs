{- Lab 4
   Date: 2022-12-14
   Authors: Zozk Mohamed and Anton Sandberg
   Lab group: - 48
 -}
{-# LANGUAGE InstanceSigs #-}

import Data.Aeson.Encoding (value)
import Parsing



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
size (Num _) = 1
size X = 1
size (Add e1 e2) = 1+size e1+size e2 
size (Mul e1 e2) = 1+size e1 +size e2
size (Sin e) = 1+size e
size (Cos e) = 1+size e 

-------------------------------------------------------------
-- *B 
-------------------------------------------------------------

-- Converts any expression to a string
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (Sin e) = "sin " ++ showFactorCosSin e
showExpr (Cos e) = "cos " ++ showFactorCosSin e
showExpr X = "x"
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
     where showFactor :: Expr -> String
           showFactor (Add a b) = "("++showExpr (Add a b)++")"
           showFactor e = showExpr e

showFactorCosSin :: Expr -> String
showFactorCosSin (Num n) = showExpr (Num n)
showFactorCosSin (Sin a) = showExpr (Sin a)
showFactorCosSin (Cos a) = showExpr (Cos a)
showFactorCosSin X = showExpr X
showFactorCosSin e = "(" ++ showExpr e ++ ")"

instance Show Expr where
  show :: Expr -> String
  show = showExpr

-------------------------------------------------------------
-- *C
-------------------------------------------------------------

-- Given an expression, and the value for the variable x, calculates the value of the expression
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval X v = v

eval (Add e1 e2) v = eval e1 v + eval e2 v
eval (Mul e1 e2) v = eval e1 v * eval e2 v
eval (Sin e) v = Prelude.sin $ eval e v
eval (Cos e) v = Prelude.cos $ eval e v

-------------------------------------------------------------
-- *D
-------------------------------------------------------------

readExpr :: String -> Maybe Expr
readExpr = undefined

number	::	Parser	Integer
number = read <$> oneOrMore digit

expr,	term,	factor, numPars	:: Parser	Expr

numPars	=	Num	<$>	number

expr	=	foldl1	Add	<$>	chain	term	(char	'+')	
term	=	foldl1	Mul<$>	chain	factor(char	'*')	
factor	= number <|> char '(' *> expr <* char ')'  

-------------------------------------------------------------
-- *E
-------------------------------------------------------------