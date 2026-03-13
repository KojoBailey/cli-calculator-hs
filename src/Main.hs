import Data.Char
import Data.Maybe ( fromJust, isJust )
import Data.Either ( fromLeft, fromRight )
import Control.Monad ( guard )

-- Challenge: No use of `read`.

data Operation = Subtraction | Addition | Division | Multiplication
  deriving (Show)

data BinaryTree a = Branch Operation (BinaryTree a) (BinaryTree a) | Leaf a
  deriving (Show)

main :: IO ()
main = do
  putStrLn "Enter your calculation to compute:"
  input <- getLine
  let
    cleanInput = filter (/= ' ') input
    parsedInput = parseString cleanInput
    binaryTree = toBinaryTree $ fromJust parsedInput
  print $ calculate binaryTree

charToInt :: Char -> Maybe Int
charToInt c = ord c - ord '0' <$ guard (isInteger c)

isInteger :: Char -> Bool
isInteger = (`elem` ['0'..'9'])

mapAndTakeWhile :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapAndTakeWhile _ _ [] = []
mapAndTakeWhile mapFunc condition (x:xs)
  | condition transX = transX : mapAndTakeWhile mapFunc condition xs
  | otherwise         = []
  where transX = mapFunc x

mapAndDropWhile :: (a -> b) -> (b -> Bool) -> [a] -> [a]
mapAndDropWhile _ _ [] = []
mapAndDropWhile mapFunc condition (x:xs)
  | condition transX = mapAndDropWhile mapFunc condition xs
  | otherwise         = x:xs
  where transX = mapFunc x

readInteger :: String -> Maybe Integer
readInteger str = list >>= \x -> pure $ conversion x
  where
    conversion x = toInteger $ foldl (\acc x -> x + acc * 10) 0 x
    list = map fromJust maybeList <$ guard (Nothing `notElem` maybeList)
    maybeList = map charToInt str

takeInteger :: String -> Maybe (Integer, String)
takeInteger str = integer >>= \left -> (left, right) <$ guard (list /= [])
  where
    list = takeWhile isInteger str
    integer = readInteger list
    right = dropWhile isInteger str

-- readFloat :: String -> Maybe (Float, String)
-- readFloat str = (left, right) <$ guard (not . null $ numberStr)
--   where
--     numberStr = takeWhile (\c -> isInteger c || c == '.') str
--     integer = fst <$> takeInteger numberStr
--     right = dropWhile 
--     isNumber = 

getOperation :: Char -> Maybe Operation
getOperation '+' = Just Addition
getOperation '-' = Just Subtraction
getOperation '*' = Just Multiplication
getOperation '/' = Just Division
getOperation  _  = Nothing

parseString :: String -> Maybe [Either Integer Operation]
parseString [] = Just []
parseString (c:cs)
  | Just operation <- getOperation c =
      parseString cs >>= \rest ->
      pure (Right operation : rest)
  | Just number <- takeInteger (c:cs)  =
      parseString (snd number) >>= \rest ->
      pure (Left (fst number) : rest)
  | otherwise = Nothing

toBinaryTree :: [Either Integer Operation] -> BinaryTree Integer
toBinaryTree [num] = Leaf $ fromLeft 0 num
toBinaryTree (num:op:rest) = Branch (fromRight Addition op) (Leaf $ fromLeft 0 num) (toBinaryTree rest)

operationToFunction :: Integral a => Operation -> (a -> a -> a)
operationToFunction Addition       = (+)
operationToFunction Subtraction    = (-)
operationToFunction Multiplication = (*)
operationToFunction Division       = div

calculate :: Integral a => BinaryTree a -> a
calculate (Leaf num)      = num
calculate (Branch op l r) = operationToFunction op (calculate l) (calculate r)
