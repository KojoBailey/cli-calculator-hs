import Data.Char
import Data.Maybe ( fromJust, isJust )
import Data.Either ( fromLeft, fromRight )
import Control.Monad ( guard )

-- Challenge: No use of `read`.

data Operation = Subtraction | Addition | Division | Multiplication
  deriving (Show)

data BinaryTree a = Branch Operation (BinaryTree a) (BinaryTree a) | Leaf a
  deriving (Show)

char_to_int :: Char -> Maybe Int
char_to_int c = ord c - ord '0' <$ guard (c `elem` ['0'..'9'])

map_and_take_while :: (a -> b) -> (b -> Bool) -> [a] -> [b]
map_and_take_while _ _ [] = []
map_and_take_while map_func condition (x:xs)
  | condition trans_x = trans_x : map_and_take_while map_func condition xs
  | otherwise         = []
  where trans_x = map_func x

map_and_drop_while :: (a -> b) -> (b -> Bool) -> [a] -> [a]
map_and_drop_while _ _ [] = []
map_and_drop_while map_func condition (x:xs)
  | condition trans_x = map_and_drop_while map_func condition xs
  | otherwise         = x:xs
  where trans_x = map_func x

get_number :: String -> Maybe (Integer, String)
get_number str = (left, right) <$ guard (not . null $ list)
  where
    list = map fromJust . map_and_take_while char_to_int isJust $ str
    left = toInteger $ foldl (\acc x -> x + acc * 10) 0 list
    right = map_and_drop_while char_to_int isJust str

get_operation :: Char -> Maybe Operation
get_operation '+' = Just Addition
get_operation '-' = Just Subtraction
get_operation '*' = Just Multiplication
get_operation '/' = Just Division
get_operation  _  = Nothing

parse_string :: String -> Maybe [Either Integer Operation]
parse_string [] = Just []
parse_string (c:cs)
  | Just operation <- get_operation c =
      parse_string cs >>= \rest ->
      pure (Right operation : rest)
  | Just number <- get_number (c:cs)  =
      parse_string (snd number) >>= \rest ->
      pure (Left (fst number) : rest)
  | otherwise = Nothing

to_binary_tree :: [Either Integer Operation] -> BinaryTree Integer
to_binary_tree [num] = Leaf $ fromLeft 0 num
to_binary_tree (num:op:rest) = Branch (fromRight Addition op) (Leaf $ fromLeft 0 num) (to_binary_tree rest)

operation_to_function :: Integral a => Operation -> (a -> a -> a)
operation_to_function Addition       = (+)
operation_to_function Subtraction    = (-)
operation_to_function Multiplication = (*)
operation_to_function Division       = div

calculate :: Integral a => BinaryTree a -> a
calculate (Leaf num)      = num
calculate (Branch op l r) = operation_to_function op (calculate l) (calculate r)

main :: IO ()
main =
  putStrLn "Enter your calculation to compute:" >>
  getLine >>= \input ->
  let
    clean_input = filter (/= ' ') input
    parsed_input = parse_string clean_input
    binary_tree = to_binary_tree $ fromJust parsed_input
  in print $ calculate binary_tree