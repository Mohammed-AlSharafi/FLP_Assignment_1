module Parse ( Expr(..), parse) where

import Tokenize (Token(..))

data Expr
  = Lit Double | Positive Expr | Negative Expr
  | Add Expr Expr | Sub Expr Expr
  | Mul Expr Expr | Div Expr Expr | Pow Expr Expr
  | Mod Expr Expr | FloorDiv Expr Expr
  deriving (Eq, Show)

-- Stage 2: construct an expression tree.
-- Each parser returns the expression and tokens still available.
type Parser = [Token] -> Either String (Expr, [Token])

-- Fix 4: Leftover tokens used to be silently discarded, so "1 2" and
-- "(1+2))" returned a number. Parsing now succeeds only if every token
-- was consumed.
parse :: [Token] -> Either String Expr
parse ts = do
  (expr, rest) <- parseExpression ts
  case rest of
    []      -> Right expr
    (t : _) -> Left ("Unexpected token: " ++ show t)

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
    continue lhs (Modulo:ts) = do
      (rhs, rest) <- parsePower ts
      continue (Mod lhs rhs) rest
    continue lhs (FloorDivide:ts) = do
      (rhs, rest) <- parsePower ts
      continue (FloorDiv lhs rhs) rest
    continue lhs rest = Right (lhs, rest)

-- Fix 3: exponents used to be left-associative, making
-- 2 ** 2 ** 4 = (2 ** 2) ** 4 = 256 instead of 2 ** (2 ** 4) = 65536,
-- which is the correct (right-associative) answer.
parsePower :: Parser
parsePower ts = do
  (first, rest) <- parseUnary ts
  continue first rest
  where
    continue lhs (Power:ts) = do
      (rhs, rest) <- parsePower ts
      Right (Pow lhs rhs, rest)
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