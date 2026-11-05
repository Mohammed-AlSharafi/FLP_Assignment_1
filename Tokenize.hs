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
    -- Fix 1: Numbers that start with '.' (such as ".1") are now
    -- accounted for by checking if the char after '.' is a digit
    | isDigit c || (c == '.' && any isDigit (take 1 cs)) = do
        let (digits, rest) = span isDigit (c:cs)
            (fraction, remaining) = case rest of
                -- Fix 2: Numbers that end in '.' are now properly handled
                -- the '.' is consumed such that spelling appends/prepends a '0'
                -- for proper handling
                '.':ds -> span isDigit ds
                _      -> ("", rest)
            spelling = orZero digits ++ "." ++ orZero fraction
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

orZero :: String -> String
orZero s = if null s then "0" else s