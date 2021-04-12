{-# LANGUAGE TemplateHaskell #-}

-- import Data.Map (Map)
-- import qualified Data.Map as M
import Prelude hiding (id, const, flip, and, or, not, show, curry, uncurry)

import Functional.Iota.Free (renderIFree)
import Functional.Lambda (render)
import Functional.Lambda.Typed.Function
	(id, const, compose, flip, apply, pipe, fix, on)
import Functional.Lambda.Typed.Bool
	(and, or, not, bool, magicIf, magicElseIf, magicElse)
import Functional.Lambda.Typed.Tuple
import Functional.Lambda.Typed.Word (showNibble, getNibbles, showByte)
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
		-- TODO
	-- Natural
		-- TODO
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
