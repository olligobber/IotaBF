import Text.Parsec (ParseError, parse, (<|>))
import Control.Monad ((<=<))
import Data.Char (isSpace)

import Functional.BinaryTree (BinaryTree, renderL, treeParserL, leftmostStrict)
import Functional.Iota (Iota, iotaParser, renderIota, fromSKI)
import Functional.Iota.Free (IFree, iFreeParser, renderIFree)

parseIota :: String -> Either ParseError (BinaryTree (Either IFree Iota))
parseIota = parse
	(treeParserL $ Left <$> iFreeParser <|> Right <$> iotaParser)
	"Standard Input"

reduceIota :: BinaryTree (Either IFree Iota) -> BinaryTree (Either IFree Iota)
reduceIota =
	either pure (fmap Right . fromSKI) <=<
	leftmostStrict .
	fmap Left

printIota :: BinaryTree (Either IFree Iota) -> String
printIota = renderL $ either renderIFree renderIota

main :: IO ()
main = interact $
	(<>"\n") .
	either show (printIota . reduceIota) .
	parseIota .
	filter (not . isSpace)
