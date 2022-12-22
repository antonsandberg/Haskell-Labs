module Expr where

{- Lab 4 V.2.0
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
import Data.List
import System.Random

{-
Questions to Matti:

1. DO you just want us to use a variable for determining the max/min values
assigned to Num or is there something more to it?


2. What do you feel is bad in our simplify, is it our main functions
or is that we go too in depth in some of the helper code?

3. How exactly do we prove that there is no junk?
Do you want us to create specific expressions and their corresponding simplify or
do you want us to provide a general formula? 

-}

-------------------------------------------------------------
-- Some test cases to test our code
-------------------------------------------------------------

{- expr1 = Mul (Add (Num 3) (Num 4)) (Add (Add X (Num 3)) (Sin (Add (Num 3) (Num 4))))

expr2 = Add (Mul (Num 3) (Num 4)) (Add (Add X (Num 3)) (Cos (Add (Num 3) (Num 4))))

expr3 = Sin X

expr4 =  fromJust $ readExpr $ "sin (x*x*x*x) + 4*3*2 + (3+2)"

expr5 = Add (Mul (Num 3) (Num 4)) (Add (Add (Num 4) (Num 3))  (Cos (Add (Add (Num 3) (Num 4)) (X))))

expr6 = fromJust $ readExpr "4 + 3 + 5 + 6 +sin(x) + cos(x)"

expr7 = Cos X -}


expr1 = Op Mul (Op Add (Num 3) (Num 4)) (Op Add (Op Add X (Num 3)) (Uni Sin (Op Add (Num 3) (Num 4))))

expr2 = Op Add (Op Mul (Num 3) (Num 4)) (Op Add (Op Add X (Num 3)) (Uni Cos (Op Add (Num 3) (Num 4))))

expr3 = Uni Sin X

expr4 =  fromJust $ readExpr $ "sin (x*x*x*x) + 4*3*2 + (3+2)"

expr5 = Op Add (Op Mul (Num 3) (Num 4)) (Op Add (Op Add (Num 4) (Num 3))  (Uni Cos (Op Add (Op Add (Num 3) (Num 4)) X)))

expr6 = fromJust $ readExpr "4 + 3 + 5 + 6 +sin(x) + cos(x)"

expr7 = Uni Cos X

-------------------------------------------------------------
-- *A
-------------------------------------------------------------
-- Recursive datatype that represents expressions 
{- data Expr = Num Double
            | Add Expr Expr
            | Mul Expr Expr
            | Sin Expr
            | Cos Expr
            | X
    deriving Eq
 -}

-- Adding the critisism here
data BinOp = Add | Mul
              deriving Eq

data UniOp = Sin | Cos
              deriving (Eq, Show)

data Expr = Num Double
              | Op BinOp Expr Expr
              | Uni UniOp Expr
              | X
              deriving Eq   

-- Need this to shorten our expressions
getOp :: BinOp -> Double -> Double -> Double
getOp Add = (+)
getOp Mul = (*) 

getUni :: UniOp -> Double -> Double
getUni Sin = Prelude.sin
getUni Cos = Prelude.cos

-- In order that we can run tests 

-- x :: Expr
-- x = X

-- num :: Double -> Expr
-- num = Num

-- add,mul :: Expr -> Expr -> Expr
-- add = Add
-- mul = Mul

-- sin,cos :: Expr -> Expr
-- sin = Sin
-- cos = Cos

-- -- x :: Expr
-- -- x = X

num :: Double -> Expr
num = Num

add,mul :: Expr -> Expr -> Expr
add = Op Add 
mul = Op Mul

sin,cos :: Expr -> Expr
sin = Uni Sin
cos = Uni Cos

-- -- Counts the number of functions and operators in the given expression
-- size :: Expr -> Int
-- size (Num _)      = 1
-- size X            = 1
-- size (Add e1 e2)  = 1 + size e1 + size e2
-- size (Mul e1 e2)  = 1 + size e1 + size e2
-- size (Sin e)      = 1 + size e
-- size (Cos e)      = 1 + size e

-- Counts the number of functions and operators in the given expression
size :: Expr -> Int
size (Num _)      = 1
size X            = 1
size (Op _ e1 e2) = 1 + size e1 + size e2
size (Uni _ e)    = 1 + size e



-- -------------------------------------------------------------
-- -- *B 
-- -------------------------------------------------------------

-- -- Converts any expression to a string
-- showExpr :: Expr -> String
-- showExpr (Num n)      = show n
-- showExpr (Sin e)      = "sin " ++ showFactorCosSin e
-- showExpr (Cos e)      = "cos " ++ showFactorCosSin e
-- showExpr X            = "x"
-- showExpr (Add e1 e2)  = showExpr e1 ++ " + " ++ showExpr e2
-- showExpr (Mul a b)    = showFactor a ++ " * " ++ showFactor b
--      where showFactor :: Expr -> String
--            showFactor (Add a b) = "("++showExpr (Add a b)++")"
--            showFactor e         = showExpr e

-- showFactorCosSin :: Expr -> String
-- showFactorCosSin (Num n)  = showExpr (Num n)
-- showFactorCosSin (Sin a)  = showExpr (Sin a)
-- showFactorCosSin (Cos a)  = showExpr (Cos a)
-- showFactorCosSin X        = showExpr X
-- showFactorCosSin e        = "(" ++ showExpr e ++ ")"


-- Converts any expression to a string
showExpr :: Expr -> String
showExpr (Num n)      = show n
-- showExpr (Uni Sin e)      = "sin " ++ showFactorCosSin e
-- showExpr (Uni Cos e)      = "cos " ++ showFactorCosSin e
showExpr (Uni sc e)     = lowSc ++ " " ++ showFactorCosSin e 
      where lowSc :: String
            lowSc = [toLower c | c <- show sc]          

showExpr X                 = "x"
showExpr (Op Add e1 e2)    = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Op Mul e1 e2)    = showFactor e1 ++ " * " ++ showFactor e2
      where showFactor :: Expr -> String
            showFactor (Op Add a b) = "("++showExpr (Op Add a b)++")"
            showFactor e            = showExpr e

showFactorCosSin :: Expr -> String
showFactorCosSin (Num n)     = showExpr (Num n)
showFactorCosSin (Uni sc a)  = showExpr (Uni sc a)
showFactorCosSin X           = showExpr X
showFactorCosSin e           = "(" ++ showExpr e ++ ")"


instance Show Expr where
  -- show :: Expr -> String
  show = showExpr


-- -------------------------------------------------------------
-- -- *C
-- -------------------------------------------------------------

-- -- Given an expression, and the value for the variable x, calculates the value of the expression
-- eval :: Expr -> Double -> Double
-- eval (Num n) _      = n
-- eval X v            = v

-- eval (Add e1 e2) v  = eval e1 v + eval e2 v
-- eval (Mul e1 e2) v  = eval e1 v * eval e2 v
-- eval (Sin e) v      = Prelude.sin $ eval e v
-- eval (Cos e) v      = Prelude.cos $ eval e v

eval :: Expr -> Double -> Double
eval (Num n) _        = n
eval X v              = v
eval (Op op e1 e2) v  = getOp op (eval e1 v)  (eval e2 v)
eval (Uni uni e) v    = getUni uni $ eval e v


-- -------------------------------------------------------------
-- -- *D
-- -------------------------------------------------------------

readExpr :: String -> Maybe Expr
readExpr s = parsing $ parse expr $ removeSpaces s
  where parsing (Just (e,[]))         = Just e
        parsing (Just (_,noneEmpty))  = Nothing
        parsing Nothing               = Nothing

expr, term, factor, parseX, parseSin, parseCos :: Parser Expr
parseX    = (char 'x' <|> char 'X') *> return X
parseSin  = (char 's' <|> char 'S') *> (char 'i' <|> char 'I') *> (char 'n' <|> char 'N') *> (Uni Sin <$> factor)
parseCos  = (char 'c' <|> char 'C') *> (char 'o' <|> char 'O') *> (char 's' <|> char 'S') *> (Uni Cos <$> factor)
expr      = foldl1 (Op Add) <$> chain term (char '+')
term      = foldl1 (Op Mul) <$> chain factor (char '*')
factor    = parseX <|> (Num <$> readsP) <|> char '(' *> expr <* char ')' <|> parseSin <|> parseCos

-- expr, term, factor, parseX, parseSin, parseCos :: Parser Expr
-- parseX    = char 'x' *> return X
-- parseSin  = char 's' *> char 'i' *> char 'n' *> (Sin <$> factor)
-- parseCos  = char 'c' *> char 'o' *> char 's' *> (Cos <$> factor)
-- expr      = foldl1 Add <$> chain term (char '+')
-- term      = foldl1 Mul <$> chain factor (char '*')
-- factor    = parseX <|> (Num <$> readsP) <|> char '(' *> expr <* char ')' <|> parseSin <|> parseCos

-- Helper function to remove whitespace
removeSpaces :: [Char] -> [Char]
removeSpaces = filter (not . isSpace)


-- -------------------------------------------------------------
-- -- *E
-- -------------------------------------------------------------
assoc :: Expr -> Expr
assoc (Op op1 (Op op2 e1 e2) e3) | op1 == op2 = assoc (Op op1 e1 (Op op2 e2 e3))
assoc (Op op e1 e2)                           = Op op (assoc e1) (assoc e2)
assoc (Uni op e)                              = Uni op (assoc e)
-- Otherwise
assoc e                                       = e  


-- -- assoc :: Expr -> Expr
-- -- assoc (Op Add (Op Add e1 e2) e3)  = assoc (Op Add e1 (Op Add e2 e3))
-- -- assoc (Op Add e1 e2)              = Op Add (assoc e1) (assoc e2)
-- -- assoc (Op Mul (Op Mul e1 e2) e3)  = assoc (Op Mul e1 (Op Mul e2 e3))
-- -- assoc (Op Mul e1 e2)              = Op Mul (assoc e1) (assoc e2)
-- -- assoc (Uni op e)                  = Uni op (assoc e)
-- -- -- Otherwise
-- -- assoc e                           = e  

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- We handle associtative expressions with the assoc function
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e | isNothing (readExpr (showExpr e)) = True
                    | otherwise                         = assoc (fromJust (readExpr (showExpr e))) == assoc e

-- arbExpr :: Int -> Gen Expr

-- arbExpr i = frequency [(3, do Num <$> arbitrary), 
--                       (3, do return X), 
--                       (i, genSinCos i),
--                       (i, genAddMul i)]
--   where 
--     -- Either choose sin, cos, add, mul and divide i by two
--     -- to choose it less often (to not get too long of expressions)
--     genSinCos j = do
--       trig <- oneof [do return Sin, do return Cos]
--       e <- arbExpr (j `div` 2)
--       return $ trig e
    
--     genAddMul j = do
--       e1 <- arbExpr (j `div` 2)
--       e2 <- arbExpr (j `div` 2)
--       op <- oneof [do return Mul, do return Add]
--       return $ op e1 e2



arbExpr :: Int -> Gen Expr

arbExpr i = frequency [(3, do Num <$> arbitrary), 
                      (3, do return X), 
                      (i, genSinCos i),
                      (i, genAddMul i)]
  where 
    -- Either choose sin, cos, add, mul and divide i by two
    -- to choose it less often (to not get too long of expressions)
    genSinCos j = do
      e <- arbExpr (j `div` 2)
      uni <- oneof [do return (Uni Sin), do return (Uni Cos)]
      return $ uni e
    
    genAddMul j = do
      e1 <- arbExpr (j `div` 2)
      e2 <- arbExpr (j `div` 2)
      op <- oneof [do return (Op Mul), do return (Op Add)]
      return $ op e1 e2
-- -- -------------------------------------------------------------
-- -- *F
-- ----------------------------------------------------------------

-- -- We want to simplify a number of times before it's all
-- -- as far is it can go, therefor do it recursively
-- simplify :: Expr -> Expr
-- simplify e = do
--   let simplified = simplifyHelper e
--   if simplified == e then simplified else simplify simplified

-- simplifyHelper :: Expr -> Expr
-- -- Base cases (not sure if they are necessary)
-- simplifyHelper (Num x) = Num x
-- simplifyHelper X = X

-- -- Structure them in different operators for some structure
-- -- The add part
-- simplifyHelper (Add (Num x1) (Num 0.0)) = Num x1
-- simplifyHelper (Add (Num 0.0) (Num x2)) = Num x2
-- simplifyHelper (Add (Num x1) (Num x2))  = Num (x1+x2)
-- simplifyHelper (Add X (Num 0.0))        = X
-- simplifyHelper (Add (Num 0.0) X)        = X
-- simplifyHelper (Add e1 (Num 0.0))       = simplifyHelper e1
-- simplifyHelper (Add (Num 0.0) e2)       = simplifyHelper e2
-- -- Otherwise
-- simplifyHelper (Add e1 e2)              = Add (simplifyHelper e1) (simplifyHelper e2)

-- -- The mul part
-- simplifyHelper (Mul (Num x1) (Num x2))  = Num (x1*x2)
-- simplifyHelper (Mul _ (Num 0.0))        = Num 0
-- simplifyHelper (Mul (Num 0.0) _)        = Num 0
-- simplifyHelper (Mul (Num 1.0) e2)       = simplifyHelper e2
-- simplifyHelper (Mul e1 (Num 1.0))       = simplifyHelper e1
-- -- Otherwise
-- simplifyHelper (Mul e1 e2)              = Mul (simplifyHelper e1) (simplifyHelper e2)

-- -- Don't think there is much to do for cos/sin
-- -- however still have to declare them otherwise
-- -- we won't be able to use the function
-- -- Just adding a eval when sin only contains a number
-- -- Cause read in slack that was something you should do
-- simplifyHelper (Sin (Num x)) = fromJust $ readExpr $ show $ eval (Sin (Num x)) 0
-- simplifyHelper (Cos (Num x)) = fromJust $ readExpr $ show $ eval (Cos (Num x)) 0
-- simplifyHelper (Sin e) = Sin (simplifyHelper e)
-- simplifyHelper (Cos e) = Cos (simplifyHelper e)



-- as far is it can go, therefor do it recursively
simplify :: Expr -> Expr
simplify e = do
  let simplified = simplifyHelper e
  if simplified == e then simplified else simplify simplified

simplifyHelper :: Expr -> Expr
-- Base cases
simplifyHelper (Num x) = Num x
simplifyHelper X = X

simplifyHelper (Op Add (Num x1) (Num 0.0)) = Num x1
simplifyHelper (Op Add (Num 0.0) (Num x2)) = Num x2
-- Covers both Add and Mul with the getOp function
simplifyHelper (Op op (Num x1) (Num x2))  = Num (getOp op x1 x2)

simplifyHelper (Op Add X (Num 0.0))        = X
simplifyHelper (Op Add (Num 0.0) X)        = X
simplifyHelper (Op Add e1 (Num 0.0))       = simplifyHelper e1
simplifyHelper (Op Add (Num 0.0) e2)       = simplifyHelper e2

simplifyHelper (Op Mul _ (Num 0.0))        = Num 0
simplifyHelper (Op Mul (Num 0.0) _)        = Num 0
simplifyHelper (Op Mul (Num 1.0) e2)       = simplifyHelper e2
simplifyHelper (Op Mul e1 (Num 1.0))       = simplifyHelper e1

-- Otherwise (covers both Add and Mul)
simplifyHelper (Op op e1 e2)               = Op op (simplifyHelper e1) (simplifyHelper e2)

-- Don't think there is much to do for cos/sin
-- however still have to declare them otherwise
-- we won't be able to use the function
-- Just adding a eval when sin only contains a number
-- Cause read in slack that was something you should do
simplifyHelper (Uni uni (Num x)) = fromJust $ readExpr $ show $ eval (Uni uni (Num x)) 0
-- simplifyHelper (Uni Sin e) = Uni Sin (simplifyHelper e)
-- simplifyHelper (Uni Cos e) = Uni Cos (simplifyHelper e)
simplifyHelper (Uni uni e) = Uni uni $ simplifyHelper e


-- Check both if the simplified value achieves the same value as well 
-- as that it contains no junk
prop_Simplify :: Expr -> Double -> Bool
prop_Simplify e n = prop_Simplify_sameValue e smpE n && prop_Simplify_noJunk smpE
       where smpE = simplify e

prop_Simplify_sameValue :: Expr -> Expr -> Double -> Bool  
prop_Simplify_sameValue e smpE n = eval e n == eval smpE n

-- Making sure there is no junk, returning False if it is
prop_Simplify_noJunk :: Expr -> Bool
prop_Simplify_noJunk (Num x) = True
prop_Simplify_noJunk X = True

prop_Simplify_noJunk (Op Add (Num x1) (Num 0.0)) = False
prop_Simplify_noJunk (Op Add (Num 0.0) (Num x2)) = False
-- Covers both Add and Mul
prop_Simplify_noJunk (Op op (Num x1) (Num x2))  = False

prop_Simplify_noJunk (Op Add X (Num 0.0))        = False
prop_Simplify_noJunk (Op Add (Num 0.0) X)        = False
prop_Simplify_noJunk (Op Add e1 (Num 0.0))       = False
prop_Simplify_noJunk (Op Add (Num 0.0) e2)       = False

prop_Simplify_noJunk (Op Mul _ (Num 0.0))        = False
prop_Simplify_noJunk (Op Mul (Num 0.0) _)        = False
prop_Simplify_noJunk (Op Mul (Num 1.0) e2)       = False
prop_Simplify_noJunk (Op Mul e1 (Num 1.0))       = False

-- Otherwise (covers both Add and Mul)
prop_Simplify_noJunk (Uni uni (Num x)) = False
prop_Simplify_noJunk (Uni uni e) = prop_Simplify_noJunk e

prop_Simplify_noJunk (Op op e1 e2)              = prop_Simplify_noJunk e1 && prop_Simplify_noJunk e2

-- -------------------------------------------------------------
-- -- *G
-- -------------------------------------------------------------

-- -- Think we will just patter match this for all different 
-- -- variations starting with the simple ones and moving up to the 
-- -- "harder" ones (for example multiplication and sin/cos)
-- -- We also want to make use of the simplification
-- -- function before differentiate to be able to 
-- -- not work the function more than necessary
-- -- When we do it recursively we also take care 
-- -- of the inner derivative (we think lol)

-- differentiate :: Expr -> Expr
-- differentiate e = simplify $ differentiateHelper e

-- -- Should be done
-- differentiateHelper :: Expr -> Expr
-- differentiateHelper (Num _) = Num 0.0
-- differentiateHelper X = Num 1.0
-- differentiateHelper (Add e1 e2) = Add (differentiateHelper e1) (differentiateHelper e2)

-- -- Multiplication rule
-- differentiateHelper (Mul e1 e2) = Add (Mul (differentiateHelper e1) e2) (Mul e1 (differentiateHelper e2))
-- -- Cos and sin rules
-- differentiateHelper (Sin e) = Mul (Cos e) (differentiateHelper e)
-- differentiateHelper (Cos e) = Mul (Num (-1.0)) (Mul (Sin e) (differentiateHelper e))


differentiate :: Expr -> Expr
differentiate e = simplify $ differentiateHelper e

-- Should be done
differentiateHelper :: Expr -> Expr
differentiateHelper (Num _) = Num 0.0
differentiateHelper X = Num 1.0
differentiateHelper (Op Add e1 e2) = Op Add (differentiateHelper e1) (differentiateHelper e2)
-- Multiplication rule
differentiateHelper (Op Mul e1 e2) = Op Add (Op Mul (differentiateHelper e1) e2) (Op Mul e1 (differentiateHelper e2))
-- Cos and sin rules
differentiateHelper (Uni Sin e) = Op Mul (Uni Cos e) (differentiateHelper e)
differentiateHelper (Uni Cos e) = Op Mul (Num (-1.0)) (Op Mul (Uni Sin e) (differentiateHelper e))
