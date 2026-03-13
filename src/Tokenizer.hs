module Tokenizer (HighOp(..), LowOp(..), Token(..), tokenize) where

import Data.Char ( isDigit )

data HighOp -- Higher presedence than LowOp
  = Multiply
  | Divide
  deriving (Show, Eq)

data LowOp -- Lower presedence than HighOp
  = Add
  | Subtract
  deriving (Show, Eq)

data Token
  = TNum Double -- Easier to just evaluate one number type
  | TLowOp LowOp
  | THighOp HighOp
  | TParenOpen
  | TParenClose
  | TEOF -- End of file
  deriving (Show, Eq)

-- Throws on failure.
-- Consider result types down the line.
tokenize :: String -> Either String [Token]
tokenize [] = Right [TEOF]
tokenize (c:cs) =
  case c of
    ' '           -> tokenize cs
    '('           -> prependToken TParenOpen
    ')'           -> prependToken TParenClose
    '+'           -> prependToken $ TLowOp Add
    '-'           -> prependToken $ TLowOp Subtract
    '*'           -> prependToken $ THighOp Multiply
    '/'           -> prependToken $ THighOp Divide
    _ | isDigit c -> let (num, remaining) = parseNum (c:cs)
                       in fmap (TNum num :) (tokenize remaining)
    _             -> Left $ "Invalid token: " ++ [c]
  where
    prependToken :: Token -> Either String [Token]
    prependToken token = fmap (token :) (tokenize cs)

-- Does not check for invalid input.
parseNum :: String -> (Double, String)
parseNum input = (read (takeWhile isNumLiteral input) :: Double, dropWhile isNumLiteral input)
  where isNumLiteral = (`elem` '.' : ['0'..'9'])
