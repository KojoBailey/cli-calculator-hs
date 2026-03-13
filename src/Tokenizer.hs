module Tokenizer (HighOp(..), LowOp(..), Token(..), tokenize) where

import Data.Char ( isDigit )

data HighOp
  = Multiply
  | Divide
  deriving (Show, Eq)

data LowOp
  = Add
  | Subtract
  deriving (Show, Eq)

data Token
  = TNum Double
  | TLowOp LowOp
  | THighOp HighOp
  | TParenOpen
  | TParenClose
  | TEOF
  deriving (Show, Eq)

-- Throws on failure.
-- Consider result types down the line.
tokenize :: String -> [Token]
tokenize [] = [TEOF]
tokenize input =
  case c of
    ' ' -> rest
    '(' -> TParenOpen : rest
    ')' -> TParenClose : rest
    '+' -> TLowOp Add : rest
    '-' -> TLowOp Subtract : rest
    '*' -> THighOp Multiply : rest
    '/' -> THighOp Divide : rest
    _   | isDigit c -> let (num, remaining) = parseNum input in TNum num : tokenize remaining
    _   -> error "Invalid token."
  where
    c = head input
    rest = tokenize (tail input)

-- Does not check for invalid input.
parseNum :: String -> (Double, String)
parseNum input = (read (takeWhile isNumLiteral input) :: Double, dropWhile isNumLiteral input)
  where isNumLiteral = (`elem` '.' : ['0'..'9'])
