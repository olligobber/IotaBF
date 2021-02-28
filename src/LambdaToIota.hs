import Control.Monad ((<=<))
import Text.Parsec (parse, ParseError)
import Data.Char (isSpace)

import Functional.Lambda (Lambda, lambdaParser)
import Functional.BinaryTree (BinaryTree, renderL)
import Functional.Iota (Iota, renderIota)
import Functional.Iota.Free (IFree, iFreeParser, renderIFree)
import qualified Functional.Iota as I
import qualified Functional.Lambda.SKI as LSKI

convert :: Lambda x -> BinaryTree (Either x Iota)
convert =
	either (pure . Left) (fmap Right . I.fromSKI) <=< LSKI.toSKI . fmap Left

parseLambda :: String -> Either ParseError (Lambda IFree)
parseLambda = parse (lambdaParser iFreeParser) "Standard Input"

showIota :: BinaryTree (Either IFree Iota) -> String
showIota = renderL (either renderIFree renderIota)

main :: IO ()
main = interact $
	(<>"\n") .
	either show (showIota . convert) .
	parseLambda .
	filter (not . isSpace)
