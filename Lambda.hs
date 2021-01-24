{-# LANGUAGE ScopedTypeVariables #-}

module Lambda
	( Lambda
	, LambdaCombinator
	, free
	, abstract
	, render
	) where

import Data.Void (Void)
import Data.List (genericIndex)

import BinaryTree (BinaryTree(..), renderL)
import Reducible (Appliable(..))

data LambdaTerm v = Abstraction (Lambda v) | Bound Integer | Free v
	deriving (Eq, Ord, Show) -- todo remove show

newtype Lambda v = Lambda { getTree :: BinaryTree (LambdaTerm v) }
	deriving (Eq, Ord, Show) -- todo remove show

type LambdaCombinator = Lambda Void

instance Appliable (Lambda v) where
	Lambda x $$ Lambda y = Lambda $ x :^: y

free :: v -> Lambda v
free = Lambda . Leaf . Free

abstract :: Lambda (Maybe v) -> Lambda v
abstract (Lambda t) = Lambda $ Leaf $ Abstraction $ Lambda $ bindWith 1 <$> t
	where
		bindWith :: Integer -> LambdaTerm (Maybe v) -> LambdaTerm v
		bindWith n (Abstraction (Lambda s)) =
			Abstraction $ Lambda $ bindWith (n+1) <$> s
		bindWith _ (Bound i) = Bound i
		bindWith _ (Free (Just v)) = Free v
		bindWith n (Free Nothing) = Bound n

variableNames :: [String]
variableNames = flip (:) <$> (flip replicate '\'' <$> [0..]) <*> ['a'..'z']

-- Uses lowercase letters to render free variables, and adds primes (') when
-- it runs out
render :: forall v. (v -> String) -> Lambda v -> String
render renderVar = renderDepth 0 where

	renderDepth :: Integer -> Lambda v -> String
	renderDepth d (Lambda x) = renderL (renderTerm d) x

	renderTerm :: Integer -> LambdaTerm v -> String
	-- todo collect nested abstractions and render together
	renderTerm d (Abstraction l) =
		"λ" <> genericIndex variableNames d <> "." <> renderDepth (d+1) l
	renderTerm d (Bound i) = genericIndex variableNames (d-i)
	renderTerm _ (Free v) = renderVar v

-- Todo substitution

-- Todo reducible instance

-- Todo parser
