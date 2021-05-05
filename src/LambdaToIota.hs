import Control.Monad ((<=<))
import Text.Parsec (parse, ParseError)
import Data.Char (isSpace)

import Functional.Lambda (Lambda, lambdaParser)
import Functional.BinaryTree (BinaryTree, renderL)
import Functional.Iota (Iota, renderIota, IotaSafe)
import Functional.VChar (VChar, charParser, renderVChar)
import qualified Functional.Iota as I
import qualified Functional.Lambda.SKI as LSKI

convert :: Lambda x -> BinaryTree (Either x Iota)
convert =
	either (pure . Left) (fmap Right . I.fromSKI) <=< LSKI.toSKI . fmap Left

parseLambda :: String -> Either ParseError (Lambda (VChar IotaSafe))
parseLambda = parse (lambdaParser charParser) "Standard Input"

showIota :: BinaryTree (Either (VChar IotaSafe) Iota) -> String
showIota = renderL (either renderVChar renderIota)

-- Convert lambda calculus to iota
main :: IO ()
main = interact $
	(<>"\n") .
	either show (showIota . convert) .
	parseLambda .
	filter (not . isSpace)
