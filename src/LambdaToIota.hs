import Control.Monad ((<=<))
import Text.Parsec (parse, ParseError)
import Data.Char (isSpace)
import Data.Type.Set (Union)

import Functional.Lambda (Lambda, lambdaParser, LambdaSafe)
import Functional.BinaryTree (BinaryTree, renderL)
import Functional.Iota (Iota, renderIota, IotaSafe)
import Functional.VChar (VChar, charParser, renderVChar)
import qualified Functional.Iota as I
import qualified Functional.Lambda.SKI as LSKI

convert :: Lambda x -> BinaryTree (Either x Iota)
convert =
	either (pure . Left) (fmap Right . I.fromSKI) <=< LSKI.toSKI . fmap Left

type FreeVar = VChar (Union IotaSafe LambdaSafe)

parseLambda :: String -> Either ParseError (Lambda FreeVar)
parseLambda = parse (lambdaParser charParser) "Standard Input"

showIota :: BinaryTree (Either FreeVar Iota) -> String
showIota = renderL (either renderVChar renderIota)

-- Convert lambda calculus to iota
main :: IO ()
main = interact $
	(<>"\n") .
	either show (showIota . convert) .
	parseLambda .
	filter (not . isSpace)
