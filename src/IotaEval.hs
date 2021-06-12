import Text.Parsec (ParseError, parse, (<|>), eof)
import Control.Monad ((<=<))
import Data.Char (isSpace)
import System.Environment (getArgs)

import Functional.BinaryTree
	(BinaryTree, renderL, treeParserL, leftmostReduce, leftmostStrict)
import Functional.Iota (Iota, iotaParser, renderIota, fromSKI, IotaSafe)
import Functional.VChar (VChar, charParser, renderVChar)
import Functional.SKI (SKI)

parseIota ::
	String -> Either ParseError (BinaryTree (Either (VChar IotaSafe) Iota))
parseIota = parse
	(treeParserL (Left <$> charParser <|> Right <$> iotaParser) <* eof)
	"Standard Input"

type Reductant = Either (Either (VChar IotaSafe) Iota) SKI

reduceIota :: (BinaryTree Reductant -> BinaryTree Reductant) ->
	BinaryTree (Either (VChar IotaSafe) Iota) ->
	BinaryTree (Either (VChar IotaSafe) Iota)
reduceIota reduction =
	either pure (fmap Right . fromSKI) <=<
	reduction .
	fmap Left

printIota :: BinaryTree (Either (VChar IotaSafe) Iota) -> String
printIota = renderL $ either renderVChar renderIota

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
