{- Lab 4
   Date: 2022-12-14
   Authors: Zozk Mohamed and Anton Sandberg
   Lab group: - 48
 -}

import Data.Aeson.Encoding (value)


-------------------------------------------------------------
-- *A
-------------------------------------------------------------

-- Recursive datatype that represents expressions 
data Expr = Num Double 
            | Add Expr Expr 
            | Mul Expr Expr 
            | Sin Expr 
            | Cos Expr 
            | Var X
    deriving (Show, Eq)

type X = Char

-- In order that we can run tests 
x :: Expr
x = Var 'x'

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
size (Var _) = 1
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
showExpr (Sin e) = "sin " ++ showExpr e
showExpr (Cos e) = "cos " ++ showExpr e
showExpr (Var x) = show x
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
     where showFactor :: Expr -> String
           showFactor (Add a b) = "("++showExpr (Add a b)++")"
           showFactor e = showExpr e

-- Given an expression, and the value for the variable x, calculates the value of the expression
eval :: Expr -> Double -> Double
eval (Num n) _ = n
eval (Var x) v = v

eval (Add e1 e2) v = eval e1 v + eval e2 v
eval (Mul e1 e2) v = eval e1 v * eval e2 v
eval (Sin e) v = Prelude.sin $ eval e v
eval (Cos e) v = Prelude.cos $ eval e v

-------------------------------------------------------------
-- *C
-------------------------------------------------------------





-------------------------------------------------------------
-- *D
-------------------------------------------------------------






-------------------------------------------------------------
-- *E
-------------------------------------------------------------