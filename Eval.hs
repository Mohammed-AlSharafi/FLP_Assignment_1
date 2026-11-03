module Eval (eval) where

import Data.Fixed (mod', div')
import Parse (Expr(..))


-- Stage 3: evaluate a parsed expression (without throwing exceptions).
eval :: Expr -> Either String Double
eval (Lit n) = Right n
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
  if abs denominator < 1e-9
     then Left "Division by zero"
     else Right (op numerator denominator)

binary :: Binary
binary op a b = do
  x <- eval a
  y <- eval b
  Right (op x y)
