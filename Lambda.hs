{-# LANGUAGE ScopedTypeVariables #-}

module Lambda
	( Lambda
	, LambdaCombinator
	, free
	, abstract
	, render
	) where

import Data.Void (Void)

import BinaryTree (BinaryTree(..), renderL)
import Reducible (Appliable(..))

data LambdaTerm v = Abstraction (Lambda v) | Bound Int | Free v
	deriving (Eq, Ord)

instance Functor LambdaTerm where
	fmap f (Abstraction l) = Abstraction $ f <$> l
	fmap _ (Bound i) = Bound i
	fmap f (Free x) = Free $ f x

instance Foldable LambdaTerm where
	foldMap f (Abstraction l) = foldMap f l
	foldMap _ (Bound _) = mempty
	foldMap f (Free x) = f x

instance Traversable LambdaTerm where
	sequenceA (Abstraction l) = Abstraction <$> sequenceA l
	sequenceA (Bound i) = pure $ Bound i
	sequenceA (Free x) = Free <$> x

newtype Lambda v = Lambda (BinaryTree (LambdaTerm v))
	deriving (Eq, Ord)

type LambdaCombinator = Lambda Void

instance Functor Lambda where
	fmap f (Lambda x) = Lambda $ fmap f <$> x

instance Foldable Lambda where
	foldMap f (Lambda x) = foldMap (foldMap f) x

instance Traversable Lambda where
	sequenceA (Lambda x) = Lambda <$> traverse sequenceA x

-- todo applicative and monad instances

instance Appliable (Lambda v) where
	Lambda x $$ Lambda y = Lambda $ x :^: y

free :: v -> Lambda v
free = Lambda . Leaf . Free

abstract :: Lambda (Maybe v) -> Lambda v
abstract (Lambda t) = Lambda $ Leaf $ Abstraction $ Lambda $ bindWith 1 <$> t
	where
		bindWith :: Int -> LambdaTerm (Maybe v) -> LambdaTerm v
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

	renderDepth :: Int -> Lambda v -> String
	renderDepth d (Lambda x) = renderL (renderTerm d) x

	renderTerm :: Int -> LambdaTerm v -> String
	-- todo collect nested abstractions and render together
	renderTerm d (Abstraction l) =
		"λ" <> variableNames !! d <> "." <> renderDepth (d+1) l
	renderTerm d (Bound i) = variableNames !! (d-i)
	renderTerm _ (Free v) = renderVar v

-- Todo substitution

-- Todo reducible instance

-- Todo parser
