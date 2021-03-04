import Text.Parsec (ParseError, parse, (<|>))
import Control.Monad ((<=<))
import Data.Char (isSpace)
import System.Environment (getArgs)

import Functional.BinaryTree
	(BinaryTree, renderL, treeParserL, leftmostReduce, leftmostStrict)
import Functional.Iota (Iota, iotaParser, renderIota, fromSKI)
import Functional.Iota.Free (IFree, iFreeParser, renderIFree)
import Functional.SKI (SKI)

parseIota :: String -> Either ParseError (BinaryTree (Either IFree Iota))
parseIota = parse
	(treeParserL $ Left <$> iFreeParser <|> Right <$> iotaParser)
	"Standard Input"

type Reductant = Either (Either IFree Iota) SKI

reduceIota :: (BinaryTree Reductant -> BinaryTree Reductant) ->
	BinaryTree (Either IFree Iota) -> BinaryTree (Either IFree Iota)
reduceIota reduction =
	either pure (fmap Right . fromSKI) <=<
	reduction .
	fmap Left

printIota :: BinaryTree (Either IFree Iota) -> String
printIota = renderL $ either renderIFree renderIota

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
