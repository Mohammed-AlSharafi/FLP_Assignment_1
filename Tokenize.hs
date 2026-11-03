module Tokenize (tokenize, Token(..)) where

import Data.Char (isDigit, isSpace)

data Token
  = Number Double | Plus | Minus | Times | Divide | Power
  | FloorDivide | Modulo | Open | Close
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
