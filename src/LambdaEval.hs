import Text.Parsec (ParseError, parse, many, noneOf)
import Data.Char (isSpace)
import System.Environment (getArgs)

import Functional.Lambda
	(Lambda, render, leftmostReduce, leftmostStrict, lambdaParser)
import Functional.Reducible (Var(..))

parseLambda :: String -> Either ParseError (Lambda (Var String))
parseLambda = parse
	(lambdaParser $ Var <$> many (noneOf "()\\λ"))
	"Standard Input"

printLambda :: Lambda (Var String) -> String
printLambda = render getVar

-- Evaluate lambda calculus to WHNF, acting strict on free variables if
-- the flag is enabled
main :: IO ()
main = do
	args <- getArgs
	reduction <- case args of
		[] -> pure leftmostReduce
		["--strict"] -> pure leftmostStrict
		["-s"] -> pure leftmostStrict
		[a] -> fail $ "Unrecognised argument: " <> a
		_ -> fail "Too many arguments"
	interact $
		(<>"\n") .
		either show (printLambda . reduction) .
		parseLambda .
		filter (not . isSpace)
