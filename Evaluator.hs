module Evaluator (evaluate) where

import Tokenize (tokenize, Token(..))
import Parse (parseExpression, Expr(..))
import Eval (eval)

-- Public function: use this in GHCi or in your own experiments.
evaluate :: String -> Either String Double
evaluate input = do
  tokens <- tokenize input
  (expr, _unused) <- parseExpression tokens
  eval expr
