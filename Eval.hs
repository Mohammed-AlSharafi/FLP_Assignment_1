module Eval (eval) where

import Data.Fixed (mod', div')
import Parse (Expr(..))

-- Stage 3: evaluate a parsed expression (without throwing exceptions).
eval :: Expr -> Either String Double
-- Fix 5: literals are checked too, because 'read' turns a huge number
-- such as 1 followed by 400 zeros into Infinity.
eval (Lit n) = finite n
eval (Positive e) = eval e
eval (Negative e) = negate <$> eval e
eval (Add a b) = binary (+) a b
eval (Sub a b) = binary (-) a b
eval (Mul a b) = binary (*) a b
eval (Pow a b) = binary (**) a b
eval (Div a b) = safeBinary (/) a b
eval (Mod a b) = safeBinary mod' a b
eval (FloorDiv a b) = safeBinary (\x y -> fromIntegral(div' x y)) a b

type Binary = (Double -> Double -> Double) -> Expr -> Expr -> Either String Double

safeBinary :: Binary
safeBinary op a b = do
  numerator <- eval a
  denominator <- eval b
  -- Fix 5: the old check "abs denominator < 1e-9" incorrectly returns error division by zero:
  -- such as 1 / 0.0000000001. Only an exact zero is an error now.
  if denominator == 0
     then Left "Division by zero"
     else finite (op numerator denominator)

binary :: Binary
binary op a b = do
  x <- eval a
  y <- eval b
  finite (op x y)

-- Fix 5: Proper error handling for doubles that silently produce NaN and Infinity (10 ** 400,
-- (-8) ** 0.5, 0 ** -1), and these used to be returned as successful
-- results. Every literal and every result now passes through here, so
-- they become errors instead.
finite :: Double -> Either String Double
finite x
  | isNaN x      = Left "Result is undefined (not a number)"
  | isInfinite x = Left "Result is too large to print"
  | otherwise    = Right x