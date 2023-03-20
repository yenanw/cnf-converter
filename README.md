# CNF-converter

A simple converter that transforms logical formulas into its
corresponding CNF, written in Haskell.

## Usage

Download the `Converter` binary and run it with an argument, where the argument
is a logical formula of the following form:

 - Conjunction: &
 - Disjunction: |
 - Negation: ~
 - Implication: ->
 - Literals: Letter followed by any number of letters or digits

(Note: <- not supported for now)

For examples:
```bash
$ ./Converter "(A1 & B1) | (A2 & B2) | (A3 & B3)"
(A1 | A2 | A3) & (A1 | B2 | A3) & (B1 | A2 | A3) & (B1 | B2 | A3) & (A1 | A2 | B3) & (A1 | B2 | B3) & (B1 | A2 | B3) & (B1 | B2 | B3)

$ ./Converter "a -> b"
~a | b

$ ./Converter "0 -> b"
syntax error at line 1, column 1 due to lexer error
```

## TODO (soon<sup>TM</sup>)

 * Allow flags in arguments for more options, i.e. print out the conversion step by step
 * Add support for conversion to DNF
 * Add support for first-order logic
 * Fix the project structure
 * Fix the grammar file
