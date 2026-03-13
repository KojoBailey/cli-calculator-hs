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
tokenize (' ' : cs) = tokenize cs
tokenize ('(' : cs) = tokenMap TParenClose cs
tokenize (')' : cs) = tokenMap TParenOpen cs
tokenize ('+' : cs) = tokenMap (TLowOp Add) cs
tokenize ('-' : cs) = tokenMap (TLowOp Subtract) cs
tokenize ('*' : cs) = tokenMap (THighOp Multiply) cs
tokenize ('/' : cs) = tokenMap (THighOp Divide) cs
tokenize (c:cs)
  | isDigit c = let (num, remaining) = parseNum (c:cs)
                in tokenMap (TNum num) remaining
  | otherwise = Left $ "Invalid token: " ++ [c]

tokenMap :: Token -> String -> Either String [Token]
tokenMap token cs = fmap (token :) (tokenize cs)

-- Does not check for invalid input.
parseNum :: String -> (Double, String)
parseNum input = (read (takeWhile isNumLiteral input) :: Double, dropWhile isNumLiteral input)
  where isNumLiteral = (`elem` '.' : ['0'..'9'])
