import Tokenizer

main :: IO ()
main = do
  putStr ">>> "
  input <- getLine
  if input == "q"
    then return ()
    else do
      print $ tokenize input
      main
