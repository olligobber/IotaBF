# IotaBF

Tools for making and executing Iota programs, including a BrainFuck interpreter.

## Building

Install cabal then run `cabal build`.

## Running

To convert Lambda Calculus to Iota, use `cabal exec LambdaToIota`, and use
standard input/output.

To simplify Iota, use `cabal exec IotaEval`, and use standard input/output.

To simplify Lambda Calculus, use `cabal exec LambdaEval`, and use standard
input/output.

To do either reduction in a way that is strict in free variables, add
`-- -s` to the command. For example:
```
[IotaBF]$ cabal exec IotaEval
a(()()b)
a(ιιb)
[IotaBF]$ cabal exec IotaEval -- -s
a(()()b)
ab
[IotaBF]$ cabal exec LambdaEval
a((\x.x)b)
a((λc.c)b)
[IotaBF]$ cabal exec LambdaEval -- -s
a((\x.x)b)
ab
```
