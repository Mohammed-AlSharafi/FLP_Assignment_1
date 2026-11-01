module Evaluator (evaluate) where

import Data.Char (isDigit, isSpace)

data Token
  = Number Double | Plus | Minus | Times | Divide | Power
  | FloorDivide | Modulo | Open | Close
  deriving (Eq, Show)

data Expr
  = Lit Double | Positive Expr | Negative Expr
  | Add Expr Expr | Sub Expr Expr
  | Mul Expr Expr | Div Expr Expr | Pow Expr Expr
  deriving (Eq, Show)

-- Stage 1: split a string into tokens.
tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (c:cs)
  | isSpace c = tokenize cs
  | isDigit c = do
      let (digits, rest) = span isDigit (c:cs)
          (spelling, remaining) = case rest of
            '.':d:ds | isDigit d ->
              let (fraction, after) = span isDigit (d:ds)
              in (digits ++ "." ++ fraction, after)
            _ -> (digits, rest)
      more <- tokenize remaining
      Right (Number (read spelling) : more)
  | otherwise = do
      let (tok, rest) = case (c, cs) of
            ('*', '*':xs) -> (Just Power, xs)
            ('/', '/':xs) -> (Just FloorDivide, xs)
            ('+', xs)     -> (Just Plus, xs)
            ('-', xs)     -> (Just Minus, xs)
            ('*', xs)     -> (Just Times, xs)
            ('/', xs)     -> (Just Divide, xs)
            ('%', xs)     -> (Just Modulo, xs)
            ('(', xs)     -> (Just Open, xs)
            (')', xs)     -> (Just Close, xs)
            _             -> (Nothing, cs)
      case tok of
        Nothing -> Left ("Unexpected character: " ++ [c])
        Just t  -> (t :) <$> tokenize rest

-- Stage 2: construct an expression tree.
-- Each parser returns the expression and tokens still available.
type Parser = [Token] -> Either String (Expr, [Token])

parseExpression :: Parser
parseExpression = parseSum

parseSum :: Parser
parseSum ts = do
  (first, rest) <- parseProduct ts
  continue first rest
  where
    continue lhs (Plus:ts) = do
      (rhs, rest) <- parseProduct ts
      continue (Add lhs rhs) rest
    continue lhs (Minus:ts) = do
      (rhs, rest) <- parseProduct ts
      continue (Sub lhs rhs) rest
    continue lhs rest = Right (lhs, rest)

parseProduct :: Parser
parseProduct ts = do
  (first, rest) <- parsePower ts
  continue first rest
  where
    continue lhs (Times:ts) = do
      (rhs, rest) <- parsePower ts
      continue (Mul lhs rhs) rest
    continue lhs (Divide:ts) = do
      (rhs, rest) <- parsePower ts
      continue (Div lhs rhs) rest
    continue lhs rest = Right (lhs, rest)

parsePower :: Parser
parsePower ts = do
  (first, rest) <- parseUnary ts
  continue first rest
  where
    continue lhs (Power:ts) = do
      (rhs, rest) <- parseUnary ts
      continue (Pow lhs rhs) rest
    continue lhs rest = Right (lhs, rest)

parseUnary :: Parser
parseUnary (Plus:ts) = do
  (expr, rest) <- parseUnary ts
  Right (Positive expr, rest)
parseUnary (Minus:ts) = do
  (expr, rest) <- parseUnary ts
  Right (Negative expr, rest)
parseUnary ts = parseAtom ts

parseAtom :: Parser
parseAtom (Number n:ts) = Right (Lit n, ts)
parseAtom (Open:ts) = do
  (expr, rest) <- parseExpression ts
  case rest of
    Close:remaining -> Right (expr, remaining)
    _               -> Left "Missing closing parenthesis"
parseAtom _ = Left "Expected a number or '('"

-- Stage 3: evaluate a parsed expression (without throwing exceptions).
eval :: Expr -> Either String Double
eval (Lit n) = Right n
eval (Positive e) = eval e
eval (Negative e) = negate <$> eval e
eval (Add a b) = binary (+) a b
eval (Sub a b) = binary (-) a b
eval (Mul a b) = binary (*) a b
eval (Pow a b) = binary (**) a b
eval (Div a b) = do
  numerator <- eval a
  denominator <- eval b
  if abs denominator < 1e-9
     then Left "Division by zero"
     else Right (numerator / denominator)

binary :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
binary op a b = do
  x <- eval a
  y <- eval b
  Right (op x y)

-- Public function: use this in GHCi or in your own experiments.
evaluate :: String -> Either String Double
evaluate input = do
  tokens <- tokenize input
  (expr, _unused) <- parseExpression tokens
  eval expr
