import Text.Parsec (ParseError, parse, (<|>))
import Control.Monad ((<=<))
import Data.Char (isSpace)
import System.Environment (getArgs)

import Functional.BinaryTree
	(BinaryTree, renderL, treeParserL, leftmostReduce, leftmostStrict)
import Functional.Iota (Iota, iotaParser, renderIota, fromSKI, IotaSafe)
import Functional.Free (Free, charParser, renderFree)
import Functional.SKI (SKI)

parseIota ::
	String -> Either ParseError (BinaryTree (Either (Free IotaSafe) Iota))
parseIota = parse
	(treeParserL $ Left <$> charParser <|> Right <$> iotaParser)
	"Standard Input"

type Reductant = Either (Either (Free IotaSafe) Iota) SKI

reduceIota :: (BinaryTree Reductant -> BinaryTree Reductant) ->
	BinaryTree (Either (Free IotaSafe) Iota) ->
	BinaryTree (Either (Free IotaSafe) Iota)
reduceIota reduction =
	either pure (fmap Right . fromSKI) <=<
	reduction .
	fmap Left

printIota :: BinaryTree (Either (Free IotaSafe) Iota) -> String
printIota = renderL $ either renderFree renderIota

-- Evaluate input as iota into WHNF, acting strictly on free variables if
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
		either show (printIota . reduceIota reduction) .
		parseIota .
		filter (not . isSpace)
