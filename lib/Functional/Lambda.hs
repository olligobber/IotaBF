{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}

module Functional.Lambda
	( LambdaTerm(..)
	, Lambda(..)
	, LambdaCombinator
	, free
	, abstract
	, render
	, substitute
	, leftmostStep
	, leftmostReduce
	, lambdaParser
	) where

import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Data.Map as M
import Control.Monad.Reader (ReaderT, runReaderT, asks, local, mapReaderT)
import Control.Monad.Trans (lift)
import Control.Applicative ((<|>))
import ValidLiterals (Lift)

import Functional.BinaryTree
	(BinaryTree(..), renderL, fromBinaryTree, treeParserL)
import qualified Functional.BinaryTree as BT
import Functional.Reducible (Appliable(..), Reducible(..))

-- A leaf in the application tree of lambda calculus
data LambdaTerm v = Abstraction (Lambda v) | Bound Int | Free v
	deriving (Eq, Ord, Show, Lift)

-- A term in lambda calculus
newtype Lambda v = Lambda { getTree :: BinaryTree (LambdaTerm v) }
	deriving (Eq, Ord, Lift)

-- Lambda term with no free variables
type LambdaCombinator = forall v. Lambda v

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

-- Uses lowercase letters to render bound variables, and adds primes (') when
-- it runs out, and skipping any names already used by free variables
render :: forall v. (v -> String) -> Lambda v -> String
render renderVar top = renderDepth 0 top where

	renderDepth :: Int -> Lambda v -> String
	renderDepth d (Lambda x) = renderL (renderTerm d) x

	renderTerm :: Int -> LambdaTerm v -> String
	renderTerm d (Abstraction l@(Lambda (Leaf (Abstraction _)))) =
		"(λ" <> availVariables !! d <> drop 2 (renderDepth (d+1) l)
	renderTerm d (Abstraction l) =
		"(λ" <> availVariables !! d <> "." <> renderDepth (d+1) l <> ")"
	renderTerm d (Bound i) = availVariables !! (d-i)
	renderTerm _ (Free v) = renderVar v

	availVariables :: [String]
	availVariables = filter (`notElem` freeVariables) variableNames

	freeVariables :: S.Set String
	freeVariables = foldMap (S.singleton . renderVar) top

-- Applies one lambda to another, assuming the first would be wrapped in an
-- abstraction, and neither contains variables bound beyond the current scope
substitute :: forall v. Lambda v -> Lambda v -> Lambda v
substitute (Lambda t) (Lambda x) = Lambda $ t >>= replaceBound 1
	where
		-- Replaces bound variables of a given index with x
		replaceBound :: Int -> LambdaTerm v -> BinaryTree (LambdaTerm v)
		replaceBound n (Abstraction (Lambda l)) =
			Leaf $ Abstraction $ Lambda $ l >>= replaceBound (n+1)
		replaceBound n (Bound i) | n == i = x
		replaceBound _ y = Leaf y

instance Reducible (BinaryTree (LambdaTerm v)) v =>
	Reducible (BinaryTree (LambdaTerm v)) (LambdaTerm v) where
	reducible (Abstraction l) = Just (1, \case
		[x] -> getTree $ substitute l $ Lambda x
		_ -> error "Wrong number of arguments"
		)
	reducible (Bound _) = Nothing
	reducible (Free x) = reducible x

-- Perform a step of leftmost reduction if possible
leftmostStep ::
	Reducible (BinaryTree (LambdaTerm v)) v => Lambda v -> Maybe (Lambda v)
leftmostStep (Lambda l) =
	fromBinaryTree . fmap (Lambda . Leaf) <$> BT.leftmostStep l

-- Perform a leftmost reduction
leftmostReduce ::
	Reducible (BinaryTree (LambdaTerm v)) v => Lambda v -> Lambda v
leftmostReduce l = maybe l leftmostReduce $ leftmostStep l

data BoundState = BoundState
	-- How many abstractions deep was a variable bound
	(M.Map String Int)
	-- How many abstractions deep are we now
	Int

-- Take in a free variable parser and make a lambda calculus parser
lambdaParser :: forall s u m a.
	P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (Lambda a)
lambdaParser freeParser = runReaderT lambdaParserR emptyBoundState where

	lambdaParserR :: ReaderT BoundState (P.ParsecT s u m) (Lambda a)
	lambdaParserR = Lambda <$> mapReaderT treeParserL lambdaTermParser

	fullAbstractionParser ::
		ReaderT BoundState (P.ParsecT s u m) (LambdaTerm a)
	fullAbstractionParser =
		lift (P.char '\\' <|> P.char 'λ') *> partAbstractionParser

	partAbstractionParser ::
		ReaderT BoundState (P.ParsecT s u m) (LambdaTerm a)
	partAbstractionParser = lift variableParser >>= \var ->
		fmap Abstraction $ local (updateBoundState var) $
			lift (P.char '.') *> lambdaParserR <|>
			Lambda . Leaf <$> partAbstractionParser

	boundVariableParser :: ReaderT BoundState (P.ParsecT s u m) Int
	boundVariableParser = mapReaderT P.try $ do
		var <- lift variableParser
		Just i <- asks $ getIndex var
		return i

	lambdaTermParser :: ReaderT BoundState (P.ParsecT s u m) (LambdaTerm a)
	lambdaTermParser =
		fullAbstractionParser <|>
		Bound <$> boundVariableParser <|>
		Free <$> lift freeParser

	variableParser :: P.ParsecT s u m String
	variableParser = (:) <$> P.oneOf ['a'..'z'] <*> P.many (P.char '\'')

	-- Initial state, nothing bound
	emptyBoundState :: BoundState
	emptyBoundState = BoundState M.empty 0

	-- Bind a new variable
	updateBoundState :: String -> BoundState -> BoundState
	updateBoundState newvar (BoundState m d) = BoundState
		(M.insert newvar d m)
		(d+1)

	-- Get the DeBruijn index of a variable if it is bound
	getIndex :: String -> BoundState -> Maybe Int
	getIndex var (BoundState m d) = (d-) <$> M.lookup var m
