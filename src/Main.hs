import Tokenizer
import Parser
import Evaluator

import System.Environment
import Control.Monad ( when )
import Control.Monad.Except ( ExceptT, runExceptT, liftEither )
import Control.Monad.IO.Class (liftIO)

type App a = ExceptT String IO a

trueValues = ["T", "t", "True", "true", "1", "y", "Y", "yes", "Yes"]

main :: IO ()
main = do
  args <- getArgs
  mainMain $ length args > 0 && head args `elem` trueValues

mainMain :: Bool -> IO ()
mainMain showVerbose = do
  putStr ">>> "
  input <- getLine
  when (input /= ":q") $ do
    runExceptT (step showVerbose input) >>= either printErr pure
    main

step :: Bool -> String -> App ()
step showVerbose input = do
    tokens <- liftEither (tokenize input)
    liftIO $ doIfVerbose $ do
      putStrLn "== Tokens =="
      print tokens

    ast <- liftEither (parse tokens)
    liftIO $ doIfVerbose $ do
      newLine
      putStrLn "== Abstract Syntax Tree =="
      print ast

    result <- liftEither (evaluate ast)
    liftIO $ do
      doIfVerbose $ do
        newLine
        putStrLn "== Evaluation =="
      print result
      newLine
  where
    doIfVerbose :: IO () -> IO ()
    doIfVerbose task = if showVerbose then task else pure ()

newLine :: IO ()
newLine = putStrLn ""

printErr :: String -> IO ()
printErr err = putStrLn $ "\nERROR: " ++ err ++ "\n"
