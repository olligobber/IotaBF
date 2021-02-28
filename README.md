# IotaBF

Tools for making and executing Iota programs, including a BrainFuck interpreter.

## Building

Install cabal, run `cabal configure && cabal build`.

## Running

To convert lambda calculus to Iota, use `cabal exec LambdaToIota`, and use standard input/output.

To simplify Iota, use `cabal exec StrictIotaEval`, and use standard input/output.
