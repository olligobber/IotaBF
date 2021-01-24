import Data.Void (Void)

import BinaryTree (BinaryTree(..), renderL)

data LambdaTerm v = Abstraction (Lambda v) | Bound Integer | Free v
	deriving (Eq, Ord, Show)

newtype Lambda v = Lambda { getTree :: BinaryTree (LambdaTerm v) }
	deriving (Eq, Ord, Show)

type LambdaCombinator = Lambda Void

ap :: Lambda v -> Lambda v -> Lambda v
ap (Lambda x) (Lambda y) = Lambda $ x :^: y

abstract :: Lambda (Maybe v) -> Lambda v
abstract (Lambda t) = Lambda $ Leaf $ Abstraction $ Lambda $ bindWith 1 <$> t
	where
		bindWith n (Abstraction (Lambda s)) =
			Abstraction $ Lambda $ bindWith (n+1) <$> s
		bindWith _ (Bound i) = Bound i
		bindWith _ (Free (Just v)) = Free v
		bindWith n (Free Nothing) = Bound n

variableNames :: [String]
variableNames = flip (:) <$> (flip replicate '\'' <$> [0..]) <*> ['a'..'z']

-- Todo render bound variables without DeBruijn indices
render :: (v -> String) -> Lambda v -> String
render renderVar (Lambda x) = renderL renderTerm x where
	renderTerm (Abstraction l) = "λ." <> render renderVar l
	renderTerm (Bound i) = show i
	renderTerm (Free v) = renderVar v

-- Todo substitution

-- Todo reducible instance

-- Todo parser 
