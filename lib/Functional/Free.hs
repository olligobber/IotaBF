{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveLift #-}

module Functional.Free
	( Free(fromFree)
	, renderFree
	, Restriction(..)
	, charParser
	, release
	, toSet
	, NoParens
	, NoWhitespace
	) where

import ValidLiterals (Validate, fromLiteral, Lift)
import Data.Kind (Type)
import Data.Type.Set (Union, AsSet)
import Data.Char (isSpace)
import Text.Parsec (ParsecT, Stream, try)
import Text.Parsec.Char (anyChar)

import Functional.Reducible (Reducible(..))

-- Type for Chars that satisfy a list of requirements, used for free variables
newtype Free (r :: [Type]) = Free { fromFree :: Char } deriving (Eq, Ord, Lift)

renderFree :: Free r -> String
renderFree = pure . fromFree

instance Show (Free r) where
	showsPrec d (Free x) = showParen (d > 10) $
		showString "Free " . showsPrec 11 x

instance Reducible t (Free r) where
	reducible _ = Nothing

-- No requirements means any Char is valid
instance Validate Char (Free '[]) where
	fromLiteral = Just . Free

-- Class of requirements for free variables
class Restriction r where
	-- Returns true if a char is not allowed by the requirement
	block :: Char -> Bool

-- Add a requirement to a list
instance (Restriction r, Validate Char (Free rs)) =>
	Validate Char (Free (r:rs)) where
		fromLiteral c
			| (block @r) c = Nothing
			| otherwise = Free . fromFree <$> (fromLiteral c :: Maybe (Free rs))

-- Parser for any validated char
charParser :: (Validate Char t, Stream s m Char) => ParsecT s u m t
charParser = try $ do
	x <- anyChar
	case fromLiteral x of
		Just y -> pure y
		Nothing -> fail "Failed to validate char"

-- Loosen the requirements on a char. Using type applications to specify s and t
-- is recommended
release :: Free (Union s t) -> Free s
release = Free . fromFree

-- Convert a list of requirements to one without repeats, useful in combination
-- with release
toSet :: Free s -> Free (AsSet s)
toSet = Free . fromFree

-- Requirement that no parentheses are allowed
data NoParens

instance Restriction NoParens where
	block '(' = True
	block ')' = True
	block _ = False

-- Requirement that no whitespace is allowed
data NoWhitespace

instance Restriction NoWhitespace where
	block = isSpace
