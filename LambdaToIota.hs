import Control.Monad ((<=<))
import qualified Text.Parsec as P
import Data.Char (isSpace)

import Functional.Lambda (Lambda, lambdaParser)
import Functional.BinaryTree (BinaryTree, renderL)
import Functional.Iota (Iota, renderIota)
import qualified Functional.Iota as I
import qualified Functional.Lambda.SKI as LSKI

convert :: Lambda String -> BinaryTree (Either String Iota)
convert =
	either (pure . Left) (fmap Right . I.fromSKI) <=< LSKI.toSKI . fmap Left

parseLambda :: String -> Either P.ParseError (Lambda String)
parseLambda = P.parse (lambdaParser $ P.many P.anyChar) ""

showIota :: BinaryTree (Either String Iota) -> String
showIota = renderL (either id renderIota)

main :: IO ()
main = interact $
	(<>"\n") .
	either show (showIota . convert) .
	parseLambda .
	filter (not . isSpace)
