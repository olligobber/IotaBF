{-# LANGUAGE TemplateHaskell #-}

-- import Data.Map (Map)
-- import qualified Data.Map as M
import Prelude hiding
	( id, const, flip, and, or, not, show, curry, uncurry, maybe, succ, pred
	, subtract, div, mod
	)

import Functional.Iota.Free (renderIFree)
import Functional.Lambda (render)
import Functional.Lambda.Typed.Function
	(id, const, compose, flip, apply, pipe, fix, on)
import Functional.Lambda.Typed.Bool
	(and, or, not, bool, magicIf, magicElseIf, magicElse)
import Functional.Lambda.Typed.Tuple
import Functional.Lambda.Typed.Word (showNibble, getNibbles, showByte)
import Functional.Lambda.Typed.Maybe
	(nothing, just, mmap, maybe, isJust, isNothing, fromMaybe)
import Functional.Lambda.Typed.Natural
	(succ, isZero, pred, add, mult, pow, subtract, divmod, div, mod)
-- import Functional.Lambda.Typed.Render (show)
-- import Functional.Lambda.Typed.Eq (eq, neq)

import PrintLambdasTemplate

-- List of functions currently written
functions :: [FunctionEntry]
functions =
	[
	-- Function
	  $(makeEntry "id" "Returns its input")
	, $(makeEntry "const" "Returns first of two inputs")
	, $(makeEntry "compose" "Combines two functions into one")
	, $(makeEntry "flip" "Swaps the inputs to a function")
	, $(makeEntry "apply" "Apply a function to an input")
	, $(makeEntry "pipe" "Apply an input to a function")
	, $(makeEntry "fix" "Get the fixed point of a function")
	, $(makeEntry "on" "Apply a function to the inputs of a binary function")
	-- Bool
	, $(makeEntry "and" "Return true if both inputs are true")
	, $(makeEntry "or" "Return true if either input is true")
	, $(makeEntry "not" "Return true if the input is false")
	, $(makeEntry "bool"
		"If the boolean is true, return the first input, otherwise the second")
	, $(makeEntry "magicIf" "Used to start if-elseif-else blocks")
	, $(makeEntry "magicElseIf" "Used to continue if-elseif-else blocks")
	, $(makeEntry "magicElse" "Used to end if-elseif-else blocks")
	-- Tuple
		-- mkTupleN TODO
		-- getNofM TODO
	, $(makeEntry "curry"
		"Turn a function on a pair into a function with two inputs")
	, $(makeEntry "uncurry"
		"Turn a function with two inputs into a function on a pair")
	-- Word
	, $(makeEntry "showNibble" "Render a nibble as a hex digit")
	, $(makeEntry "getNibbles" "Get both nibbles from a byte")
	, $(makeEntry "showByte" "Render a byte as two hex digits")
	-- Maybe
	, $(makeEntry "nothing" "A maybe with no value")
	, $(makeEntry "just" "Put a value into a maybe")
	, $(makeEntry "mmap" "Apply a function to any value in a maybe")
	, $(makeEntry "maybe" $ "Replaces nothing with the first value and " <>
		"applies the second value to just")
	, $(makeEntry "isJust" "Check if a maybe has a value")
	, $(makeEntry "isNothing" "Check if a maybe has no value")
	, $(makeEntry "fromMaybe" $ "Extract the value from a maybe, returns " <>
		"the first input if no value exists")
	-- Natural
	, $(makeEntry "succ" "Increment a natural number")
	, $(makeEntry "isZero" "Check if a natural number is zero")
	, $(makeEntry "pred" "Decrement a natural number, fails on zero")
	, $(makeEntry "add" "Add two natural numbers")
	, $(makeEntry "mult" "Multiply two natural numbers")
	, $(makeEntry "pow" "Raise the first number to the power of the second")
	, $(makeEntry "subtract"
		"Subtract two numbers, fails if the result is negative")
	, $(makeEntry "divmod" $ "Divide the first number by the second and get " <>
		"the quotient and remainder")
	, $(makeEntry "div" "Divide the first number by the second")
	, $(makeEntry "mod" "Get the first number modulo the second")
	-- Render
		-- TODO
	-- Eq
		-- TODO
	-- Representable
		-- TODO
	]

-- Output all functions listed above
main :: IO ()
main = mapM_
	(\e -> do
		putStrLn ""
		putStrLn $ "Name: " <> name e
		putStrLn $ "Description: " <> description e
		putStrLn $ "Type: " <> ltype e
		putStrLn $ "Definition: " <> render renderIFree (lambda e)
	)
	functions
	>> putStrLn ""

-- Desired behaviours:
	-- Print functions from a list (lambda or iota)
	-- Print types from a list
	-- List all available
