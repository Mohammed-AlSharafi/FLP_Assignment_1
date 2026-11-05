module Evaluator (evaluate) where

import Tokenize (tokenize, Token(..))
import Parse (parse, Expr(..))
import Eval (eval)

-- Public function: use this in GHCi or in your own experiments.
evaluate :: String -> Either String Double
evaluate input = do
  tokens <- tokenize input
  expr <- parse tokens
  eval expr
