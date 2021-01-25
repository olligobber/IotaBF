{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda
	( LambdaTerm(..)
	, Lambda(..)
	, LambdaCombinator
	, free
	, abstract
	, render
	) where

import Data.Void (Void)

import BinaryTree (BinaryTree(..), renderL)
import Reducible (Appliable(..))

-- A leaf in the application tree of lambda calculus
data LambdaTerm v = Abstraction (Lambda v) | Bound Int | Free v
	deriving (Eq, Ord, Show)

-- A term in lambda calculus
newtype Lambda v = Lambda { getTree :: BinaryTree (LambdaTerm v) }
	deriving (Eq, Ord)

-- Lambda term with no free variables
type LambdaCombinator = Lambda Void

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

instance Applicative LambdaTerm where
	pure = Free
	Abstraction (Lambda f) <*> x = Abstraction $ Lambda $ (<*> x) <$> f
	Bound i <*> _ = Bound i
	Free f <*> x = f <$> x

instance Monad LambdaTerm where
	Abstraction (Lambda x) >>= f = Abstraction $ Lambda $ (>>= f) <$> x
	Bound i >>= _ = Bound i
	Free x >>= f = f x

instance Functor Lambda where
	fmap f (Lambda x) = Lambda $ fmap f <$> x

instance Foldable Lambda where
	foldMap f (Lambda x) = foldMap (foldMap f) x

instance Traversable Lambda where
	sequenceA (Lambda x) = Lambda <$> traverse sequenceA x

instance Applicative Lambda where
	pure = free
	Lambda f <*> Lambda x = Lambda $ (<*>) <$> f <*> x

instance Monad Lambda where
	(>>=) :: forall a b. Lambda a -> (a -> Lambda b) -> Lambda b
	Lambda x >>= f = Lambda $ x >>= bindTerm where
		bindTerm :: LambdaTerm a -> BinaryTree (LambdaTerm b)
		bindTerm (Abstraction y) = pure $ Abstraction $ y >>= f
		bindTerm (Bound i) = pure $ Bound i
		bindTerm (Free y) = getTree $ f y

instance Show v => Show (Lambda v) where
	showsPrec d (Lambda x) = showParen (d>10) $
		showString "Lambda " . showsPrec 11 x

instance Appliable (Lambda v) where
	Lambda x $$ Lambda y = Lambda $ x :^: y

-- Simple free variable
free :: v -> Lambda v
free = Lambda . Leaf . Free

-- M[Nothing] => \x.M[x]
abstract :: Lambda (Maybe v) -> Lambda v
abstract (Lambda t) = Lambda $ Leaf $ Abstraction $ Lambda $ bindWith 1 <$> t
	where
		bindWith :: Int -> LambdaTerm (Maybe v) -> LambdaTerm v
		bindWith n (Abstraction (Lambda s)) =
			Abstraction $ Lambda $ bindWith (n+1) <$> s
		bindWith _ (Bound i) = Bound i
		bindWith _ (Free (Just v)) = Free v
		bindWith n (Free Nothing) = Bound n

-- Infinite list of variable names used for rendering bound variables
variableNames :: [String]
variableNames = flip (:) <$> (flip replicate '\'' <$> [0..]) <*> ['a'..'z']

-- Uses lowercase letters to render free variables, and adds primes (') when
-- it runs out
-- Todo remove free variables form list of available variable names
render :: forall v. (v -> String) -> Lambda v -> String
render renderVar = renderDepth 0 where

	renderDepth :: Int -> Lambda v -> String
	renderDepth d (Lambda x) = renderL (renderTerm d) x

	renderTerm :: Int -> LambdaTerm v -> String
	-- todo collect nested abstractions and render together
	renderTerm d (Abstraction l) =
		"(λ" <> variableNames !! d <> "." <> renderDepth (d+1) l <> ")"
	renderTerm d (Bound i) = variableNames !! (d-i)
	renderTerm _ (Free v) = renderVar v

-- Todo substitution

-- Todo reducible instance

-- Todo parser
