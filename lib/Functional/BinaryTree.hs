{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeApplications #-}

module Functional.BinaryTree
	( BinaryTree(..)
	, fromBinaryTree
	, invert
	, render
	, renderL
	, renderR
	, treeParser
	, treeParserL
	, treeParserR
	, leftmostStep
	, leftmostReduce
	, leftmostStrict
	) where

import Data.Function (on)
import Control.Applicative (liftA2, (<|>))
import qualified Text.Parsec as P
import Data.Maybe (isNothing)
import ValidLiterals (Lift)

import Functional.Reducible (Appliable(..), Reducible(..))

-- Simple binary tree with values stored in leaves
data BinaryTree a = Leaf a | (:^:) (BinaryTree a) (BinaryTree a)
	deriving (Eq, Ord, Lift)

infix 5 :^:

instance Show a => Show (BinaryTree a) where
	showsPrec d (Leaf x) = showParen (d > 10) $
		showString "Leaf " . showsPrec 11 x
	showsPrec d (l :^: r) = showParen (d > 5) $
		showsPrec 6 l . showString " :^: " . showsPrec 6 r

instance Functor BinaryTree where
	fmap f (Leaf x) = Leaf $ f x
	fmap f (l :^: r) = ((:^:) `on` fmap f) l r

instance Foldable BinaryTree where
	foldMap f (Leaf x) = f x
	foldMap f (l :^: r) = ((<>) `on` foldMap f) l r

instance Traversable BinaryTree where
	sequenceA (Leaf x) = Leaf <$> x
	sequenceA (l :^: r) = (liftA2 (:^:) `on` sequenceA) l r

-- Nest later structures into earlier structures
instance Applicative BinaryTree where
	pure = Leaf
	Leaf f <*> t = f <$> t
	l :^: r <*> t = ((:^:) `on` (<*> t)) l r

instance Monad BinaryTree where
	Leaf x >>= f = f x
	l :^: r >>= f = ((:^:) `on` (>>= f)) l r

instance Appliable (BinaryTree x) where
	($$) = (:^:)

fromBinaryTree :: Appliable t => BinaryTree t -> t
fromBinaryTree (Leaf x) = x
fromBinaryTree (l :^: r) = (($$) `on` fromBinaryTree) l r

-- Switch left children to right children and vice versa
invert :: BinaryTree a -> BinaryTree a
invert (Leaf x) = Leaf x
invert (l :^: r) = ((:^:) `on` invert) r l

-- Apply a function to render leaves, then combine with brackets
render :: (a -> String) -> BinaryTree a -> String
render f (Leaf x) = f x
render f (Leaf l :^: Leaf r) = f l <> f r
render f (l :^: Leaf r) = "(" <> render f l <> ")" <> f r
render f (Leaf l :^: r) = f l <> "(" <> render f r <> ")"
render f (l :^: r) = "(" <> render f l <> ")" <> "(" <> render f r <> ")"

-- Render but assume left associativity
renderL :: (a -> String) -> BinaryTree a -> String
renderL f (Leaf x) = f x
renderL f (l :^: Leaf r) = renderL f l <> f r
renderL f (l :^: r) = renderL f l <> "(" <> renderL f r <> ")"

-- Render but assume right associativity
renderR :: (a -> String) -> BinaryTree a -> String
renderR f (Leaf x) = f x
renderR f (Leaf l :^: r) = f l <> renderR f r
renderR f (l :^: r) = "(" <> renderL f l <> ")" <> renderL f r

{-
LL(1) parsers without try are used, so leaf parsers must be defined carefully
to produce a valid tree parser.
In general, brackets are assumed to be part of the tree structure, and in
cases where a leaf node may or may not exist, it is assumed to not exist.
-}

-- Use a leaf parser and combine with brackets
treeParser :: forall s u m a.
	P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (BinaryTree a)
treeParser leafParser = foldl (:^:) <$> firstParser <*> secondParser where

	-- bracketed tree or leaf
	firstParser :: P.ParsecT s u m (BinaryTree a)
	firstParser =
		P.char '(' *> treeParser leafParser <* P.char ')' <|>
		Leaf <$> leafParser

	-- nothing or bracketed tree or leaf
	secondParser :: P.ParsecT s u m (Maybe (BinaryTree a))
	secondParser =
		-- Follow sets don't work by default
		Nothing <$ P.lookAhead (P.eof <|> () <$ P.char ')') <|>
		Just <$> firstParser <|>
		pure Nothing

-- Parse but assume left associativity
treeParserL :: forall s u m a.
	P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (BinaryTree a)
treeParserL leafParser = foldl1 (:^:) <$> manyParser where

	-- 1 or more children nodes
	manyParser :: P.ParsecT s u m [BinaryTree a]
	manyParser = (:) <$> childParser <*> someParser

	-- 0 or more children nodes
	someParser :: P.ParsecT s u m [BinaryTree a]
	someParser =
		[] <$ P.lookAhead (P.eof <|> () <$ P.char ')') <|>
		manyParser <|>
		pure []

	-- bracketed tree or leaf
	childParser :: P.ParsecT s u m (BinaryTree a)
	childParser =
		P.char '(' *> treeParserL leafParser <* P.char ')' <|>
		Leaf <$> leafParser

-- Parse but assume right associativity
treeParserR :: forall s u m a.
	P.Stream s m Char => P.ParsecT s u m a -> P.ParsecT s u m (BinaryTree a)
treeParserR leafParser = foldr1 (:^:) <$> manyParser where

	-- 1 or more children nodes
	manyParser :: P.ParsecT s u m [BinaryTree a]
	manyParser = (:) <$> childParser <*> someParser

	-- 0 or more children nodes
	someParser :: P.ParsecT s u m [BinaryTree a]
	someParser =
		[] <$ P.lookAhead (P.eof <|> () <$ P.char ')') <|>
		manyParser <|>
		pure []

	-- bracketed tree or leaf
	childParser :: P.ParsecT s u m (BinaryTree a)
	childParser =
		P.char '(' *> treeParserR leafParser <* P.char ')' <|>
		Leaf <$> leafParser

-- Get the leftmost element and its depth
leftEleDepth :: BinaryTree a -> (a, Integer)
leftEleDepth (Leaf x) = (x,0)
leftEleDepth (l :^: _) = succ <$> leftEleDepth l

-- Get the sequence of siblings of left descendents from bottom up
leftDescendantSiblings :: BinaryTree a -> [BinaryTree a]
leftDescendantSiblings = helper [] where
	helper :: [BinaryTree a] -> BinaryTree a -> [BinaryTree a]
	helper list (Leaf _) = list
	helper list (l :^: r) = helper (r : list) l

-- Perform a step of leftmost reduction if possible
leftmostStep ::
	Reducible (BinaryTree a) a => BinaryTree a -> Maybe (BinaryTree a)
leftmostStep (Leaf x) = case reducible x of
	Just (0, reduce) -> Just $ reduce []
	_ -> Nothing
leftmostStep t@(l :^: r) = case reducible leftEle of
	Nothing -> Nothing
	Just (i, _) | i > leftDepth -> Nothing
	Just (i, _) | i < leftDepth -> (:^: r) <$> leftmostStep l
	Just (_, reduce) -> Just $ reduce $ leftDescendantSiblings t
	where
		(leftEle, leftDepth) = leftEleDepth t

{-
	Perform a leftmost reduction, producing a term in
	weak head normal form (whnf)
-}
leftmostReduce :: Reducible (BinaryTree a) a => BinaryTree a -> BinaryTree a
leftmostReduce t = maybe t leftmostReduce $ leftmostStep t

{-
	Perform a leftmost reduction, and reduce the inputs of expressions made
	entirely of irreducible terms
-}
leftmostStrict :: forall a. Reducible (BinaryTree a) a =>
	BinaryTree a -> BinaryTree a
leftmostStrict = fst . strict . leftmostReduce where
	{-
		Reduce inputs to expressions made entirely of irreducible terms,
		and determine if the result is made entirely of irreducible terms
	-}
	strict :: BinaryTree a -> (BinaryTree a, Bool)
	strict (Leaf x) =
		( Leaf x
		, isNothing $ reducible @(BinaryTree a) x
		)
	strict (l :^: r) = case strict l of
		(nl, True) -> let (nr, x) = strict $ leftmostReduce r in (nl :^: nr, x)
		(nl, False) -> (nl :^: r, False)
