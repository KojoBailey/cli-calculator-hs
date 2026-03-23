# Command-Line Calculator - Haskell
This is a very simple command-line calculator that evaluates expressions, programmed in Haskell.

## Usage
By running the compiled app or calling `main`:

```
>>> 6 + 7
13.0

>>> 6.9 * -(7/3 + 2)
-29.900000000000006

>>> :q
```

`>>>` denotes user input.

`:q` exits the program.

### Verbose output
To see the output of both the tokeniser and parser, run the app with `True` as the first argument:

```
>>> 6.9 * -(7/3 + 2)
== Tokens ==
[TNumber 6.9,TOperator Asterisk,TOperator Minus,TParenthesisOpen,TNumber 7.0,TOperator Slash,TNumber 3.0,TOperator Plus,TNumber 2.0,TParenthesisClose]

== Abstract Syntax Tree ==
ExpBinOp (ExpNum 6.9) OpMultiply (ExpUnOp OpNegative (ExpBinOp (ExpBinOp (ExpNum 7.0) OpDivide (ExpNum 3.0)) OpAdd (ExpNum 2.0)))

== Evaluation ==
-29.900000000000006
```

Alternatively, in GHCi, you can run the following command:

```
:set args True
```

## Features
For my own sanity, this is extremely limited, so the only supported operations are:
- Addition (a + b)
- Subtraction (a - b)
- Multiplication (a * b)
- Division (a / b)

Additonally, negative numbers can be expressed (-a), and sub-expressions can be wrapped in parentheses.

This should all follow the rules of [PEDMAS](https://en.wikipedia.org/wiki/Order_of_operations). If you notice anything outputting something it shouldn't, please open an [issue](https://github.com/KojoBailey/command-line-calculator-hs/issues)!

You should also get errors if you try doing anything invalid, such as `5 +- 2 *` or dividing by zero. If you are able to get something to successfully evaluate that doesn't make sense, please also open an issue!

## Motivation
Originally, this started as a project to aid my learning of Haskell university. This resulted in a not-so-great implementation.

Now finished with the university module, I've become interesed in the implementation of tooling (compilers, interpreters) for programming languages, and figured that this would be the perfect place to start, vastly improving from my initial attempt.

Unfortunately, Haskell can be quite cumbersome to program in, particularly because of how unscoped and annoying-to-use records/structs are, so I do not intend to expand the functionality of this particular application. Instead, I've made a [similar project in OCaml](https://github.com/KojoBailey/cli-calculator-ocaml), and also want to take it even further in Rust someday.
