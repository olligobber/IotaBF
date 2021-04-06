{-# LANGUAGE TemplateHaskell #-}

-- import Data.Map (Map)
-- import qualified Data.Map as M
import Prelude hiding (id, const, flip, and)
import Data.Void (absurd)

import Functional.Lambda (render)
import Functional.Lambda.Typed.Function
	(id, const, compose, flip, apply, pipe, fix, on)
import Functional.Lambda.Typed.Bool (and)

import PrintLambdasTemplate

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
	]

main :: IO ()
main = mapM_
	(\e -> do
		putStrLn ""
		putStrLn $ "Name: " <> name e
		putStrLn $ "Description: " <> description e
		putStrLn $ "Type: " <> ltype e
		putStrLn $ "Definition: " <> render absurd (lambda e)
	)
	functions

-- Print functions from a list
-- Print types from a list
-- List all available
