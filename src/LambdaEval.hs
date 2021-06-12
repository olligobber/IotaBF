import Text.Parsec (ParseError, parse, eof)
import Data.Char (isSpace)
import System.Environment (getArgs)

import Functional.Lambda
	(Lambda, render, leftmostReduce, leftmostStrict, lambdaParser, LambdaSafe)
import Functional.VChar (VChar, renderVChar, charParser)

parseLambda :: String -> Either ParseError (Lambda (VChar LambdaSafe))
parseLambda = parse (lambdaParser charParser <* eof) "Standard Input"

printLambda :: Lambda (VChar LambdaSafe) -> String
printLambda = render renderVChar

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
