import Functional.Lambda.Typed.Natural (add)
import Functional.Lambda.Typed (toLambda, ($$$), fromTyped)
import qualified Functional.Lambda.Typed.Render as LR
import qualified Functional.Lambda as L
import Functional.VChar (renderVChar)

-- TODO use a number parser written in lambda calculus rather than haskell

-- Given two natural numbers as input, output their sum
main :: IO ()
main = do
	[a,b] <- words <$> getLine
	putStrLn $ L.render renderVChar $ L.leftmostStrict $ fromTyped $
		LR.render $$$ (add $$$ toLambda (read a) $$$ toLambda (read b))
