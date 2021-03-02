# IotaBF

Tools for making and executing Iota programs, including a BrainFuck interpreter.

## Building

Install cabal then run `cabal build`.

## Running

To convert Lambda Calculus to Iota, use `cabal exec LambdaToIota`, and use standard input/output.

To simplify Iota, use `cabal exec IotaEval`, and use standard input/output.

To simplify Iota in a way that is strict in free variables, use
`cabal exec StrictIotaEval`, and use standard input/output.
