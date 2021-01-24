{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module BinaryTree
	( BinaryTree(..)
	, invert
	, render
	, renderL
	, renderR
	, treeParser
	, treeParserL
	, treeParserR
	) where

import Data.Function (on)
import Control.Applicative (liftA2, (<|>))
import qualified Text.Parsec as P
import Text.Parsec (ParsecT)
import Control.Monad.Trans (lift)

-- Simple binary tree with balues stored in leaves
data BinaryTree a = Leaf a | (:^:) (BinaryTree a) (BinaryTree a)
	deriving (Eq, Ord)

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
	P.Stream s m Char => ParsecT s u m a -> ParsecT s u m (BinaryTree a)
treeParser leafParser = foldl (:^:) <$> firstParser <*> secondParser where

	-- bracketed tree or leaf
	firstParser :: ParsecT s u m (BinaryTree a)
	firstParser =
		P.char '(' *> treeParser leafParser <* P.char ')' <|>
		Leaf <$> leafParser

	-- nothing or bracketed tree or leaf
	secondParser :: ParsecT s u m (Maybe (BinaryTree a))
	secondParser = do
		-- Follow sets don't work properly by default...
		rest <- P.getInput >>= lift . P.uncons
		case rest of
			Nothing -> pure Nothing
			Just (')',_) -> pure Nothing
			_ -> Just <$> firstParser

-- Parse but assume left associativity
treeParserL :: forall s u m a.
	P.Stream s m Char => ParsecT s u m a -> ParsecT s u m (BinaryTree a)
treeParserL leafParser = foldl1 (:^:) <$> manyParser where

	-- 1 or more children nodes
	manyParser :: ParsecT s u m [BinaryTree a]
	manyParser = (:) <$> childParser <*> someParser

	-- 0 or more children nodes
	someParser :: ParsecT s u m [BinaryTree a]
	someParser = do
		rest <- P.getInput >>= lift . P.uncons
		case rest of
			Nothing -> pure []
			Just (')',_) -> pure []
			_ -> manyParser

	-- bracketed tree or leaf
	childParser :: ParsecT s u m (BinaryTree a)
	childParser =
		P.char '(' *> treeParser leafParser <* P.char ')' <|>
		Leaf <$> leafParser

-- Parse but assume right associativity
treeParserR :: forall s u m a.
	P.Stream s m Char => ParsecT s u m a -> ParsecT s u m (BinaryTree a)
treeParserR leafParser = foldr1 (:^:) <$> manyParser where

	-- 1 or more children nodes
	manyParser :: ParsecT s u m [BinaryTree a]
	manyParser = (:) <$> childParser <*> someParser

	-- 0 or more children nodes
	someParser :: ParsecT s u m [BinaryTree a]
	someParser = do
		rest <- P.getInput >>= lift . P.uncons
		case rest of
			Nothing -> pure []
			Just (')',_) -> pure []
			_ -> manyParser

	-- bracketed tree or leaf
	childParser :: ParsecT s u m (BinaryTree a)
	childParser =
		P.char '(' *> treeParser leafParser <* P.char ')' <|>
		Leaf <$> leafParser
