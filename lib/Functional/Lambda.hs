{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda
	( LambdaTerm(..)
	, Lambda(..)
	, LambdaCombinator
	, abstract
	, render
	, substitute
	, leftmostStep
	, leftmostReduce
	, leftmostStrict
	, lambdaParser
	, LambdaSafe
	) where

import qualified Data.Set as S
import qualified Text.Parsec as P
import qualified Data.Map as M
import Control.Monad.Reader (ReaderT, runReaderT, asks, local, mapReaderT)
import Control.Monad.Trans (lift)
import Control.Applicative ((<|>))
import ValidLiterals (Lift)
import Data.Type.Set (AsSet)

import Functional.BinaryTree (BinaryTree(..), renderL, treeParserL)
import qualified Functional.BinaryTree as BT
import Functional.Reducible (Appliable(..), Reducible(..))
import NatTypes (S(S,Z))
import Functional.VChar (Restriction(..))

-- A leaf in the application tree of lambda calculus
data LambdaTerm v = Abstraction (Lambda v) | Bound Int | LambdaFree v
	deriving (Eq, Ord, Show, Lift)

-- A term in lambda calculus, parameterised by the type of free variables
newtype Lambda v = Lambda { getTree :: BinaryTree (LambdaTerm v) }
	deriving (Eq, Ord, Lift)

-- Lambda term with no free variables
type LambdaCombinator = forall v. Lambda v

instance Functor LambdaTerm where
	fmap f (Abstraction l) = Abstraction $ f <$> l
	fmap _ (Bound i) = Bound i
	fmap f (LambdaFree x) = LambdaFree $ f x

instance Foldable LambdaTerm where
	foldMap f (Abstraction l) = foldMap f l
	foldMap _ (Bound _) = mempty
	foldMap f (LambdaFree x) = f x

instance Traversable LambdaTerm where
	sequenceA (Abstraction l) = Abstraction <$> sequenceA l
	sequenceA (Bound i) = pure $ Bound i
	sequenceA (LambdaFree x) = LambdaFree <$> x

instance Applicative LambdaTerm where
	pure = LambdaFree
	Abstraction (Lambda f) <*> x = Abstraction $ Lambda $ (<*> x) <$> f
	Bound i <*> _ = Bound i
	LambdaFree f <*> x = f <$> x

instance Monad LambdaTerm where
	Abstraction (Lambda x) >>= f = Abstraction $ Lambda $ (>>= f) <$> x
	Bound i >>= _ = Bound i
	LambdaFree x >>= f = f x

instance Functor Lambda where
	fmap f (Lambda x) = Lambda $ fmap f <$> x

instance Foldable Lambda where
	foldMap f (Lambda x) = foldMap (foldMap f) x

instance Traversable Lambda where
	sequenceA (Lambda x) = Lambda <$> traverse sequenceA x

instance Applicative Lambda where
	pure = Lambda . Leaf . LambdaFree
	Lambda f <*> Lambda x = Lambda $ (<*>) <$> f <*> x

instance Monad Lambda where
	(>>=) :: forall a b. Lambda a -> (a -> Lambda b) -> Lambda b
	Lambda x >>= f = Lambda $ x >>= bindTerm where
		bindTerm :: LambdaTerm a -> BinaryTree (LambdaTerm b)
		bindTerm (Abstraction y) = pure $ Abstraction $ y >>= f
		bindTerm (Bound i) = pure $ Bound i
		bindTerm (LambdaFree y) = getTree $ f y

instance Show v => Show (Lambda v) where
	showsPrec d (Lambda x) = showParen (d>10) $
		showString "Lambda " . showsPrec 11 x

instance Appliable (Lambda v) where
	Lambda x $$ Lambda y = Lambda $ x :^: y

-- M[Z] => \x.M[x]
abstract :: Lambda (S v) -> Lambda v
abstract (Lambda t) = Lambda $ Leaf $ Abstraction $ Lambda $ bindWith 1 <$> t
	where
		bindWith :: Int -> LambdaTerm (S v) -> LambdaTerm v
		bindWith n (Abstraction (Lambda s)) =
			Abstraction $ Lambda $ bindWith (n+1) <$> s
		bindWith _ (Bound i) = Bound i
		bindWith _ (LambdaFree (S v)) = LambdaFree v
		bindWith n (LambdaFree Z) = Bound n

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
	renderTerm _ (LambdaFree v) = renderVar v

	availVariables :: [String]
	availVariables = filter (`notElem` freeVariables) variableNames

	freeVariables :: S.Set String
	freeVariables = foldMap (S.singleton . renderVar) top

-- Applies one lambda to another, assuming the first would be wrapped in an
-- abstraction, and neither contains variables bound beyond the current scope.
-- Suitable for WHNF reductions only.
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
	reducible (LambdaFree x) = reducible x

-- Perform a step of leftmost reduction if possible
leftmostStep ::
	Reducible (BinaryTree (LambdaTerm v)) v => Lambda v -> Maybe (Lambda v)
leftmostStep (Lambda l) = Lambda <$> BT.leftmostStep l

-- Perform a leftmost reduction
leftmostReduce ::
	Reducible (BinaryTree (LambdaTerm v)) v => Lambda v -> Lambda v
leftmostReduce (Lambda l) = Lambda $ BT.leftmostReduce l

{-
	Perform a leftmost reduction, and reduce the inputs of expressions made
	entirely of irreducible terms
-}
leftmostStrict :: Reducible (BinaryTree (LambdaTerm v)) v =>
	Lambda v -> Lambda v
leftmostStrict (Lambda l) = Lambda $ BT.leftmostStrict l

data BoundState = BoundState
	-- How many abstractions deep was a variable bound
	(M.Map String Int)
	-- How many abstractions deep are we now
	Int

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

-- Take in a free variable parser and make a lambda calculus parser
lambdaParser :: forall s u m a.
	P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (Lambda a)
lambdaParser freeParser = runReaderT lambdaParserR emptyBoundState where

	-- Reader based lambda parser
	lambdaParserR :: ReaderT BoundState (P.ParsecT s u m) (Lambda a)
	lambdaParserR = Lambda <$> mapReaderT treeParserL lambdaTermParser

	-- Parse an abstraction
	fullAbstractionParser ::
		ReaderT BoundState (P.ParsecT s u m) (LambdaTerm a)
	fullAbstractionParser =
		lift (P.char '\\' <|> P.char 'λ') *> partAbstractionParser

	-- Parse the abstraction minus the lambda and some variables
	partAbstractionParser ::
		ReaderT BoundState (P.ParsecT s u m) (LambdaTerm a)
	partAbstractionParser = lift variableParser >>= \var ->
		fmap Abstraction $ local (updateBoundState var) $
			lift (P.char '.') *> lambdaParserR <|>
			Lambda . Leaf <$> partAbstractionParser

	-- Parse a bound variable using info from the state
	-- Uses try to backtrack if the variable wasn't bound
	boundVariableParser :: ReaderT BoundState (P.ParsecT s u m) Int
	boundVariableParser = mapReaderT P.try $ do
		var <- lift variableParser
		Just i <- asks $ getIndex var
		return i

	lambdaTermParser :: ReaderT BoundState (P.ParsecT s u m) (LambdaTerm a)
	lambdaTermParser =
		fullAbstractionParser <|>
		Bound <$> boundVariableParser <|>
		LambdaFree <$> lift freeParser

	-- Parse a variable without using info from the state
	variableParser :: P.ParsecT s u m String
	variableParser = (:) <$> P.oneOf ['a'..'z'] <*> P.many (P.char '\'')

instance Restriction "NoLambda" where
	block = (`elem` "\\λ")

instance Restriction "NoPrime" where
	block = (== '\'')

type LambdaSafe = AsSet ["NoLambda", "NoPrime", "NoWhitespace", "NoParens"]
