{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}

module Functional.VChar
	( VChar(fromVChar)
	, renderVChar
	, Restriction(..)
	, charParser
	, release
	, toSet
	) where

import ValidLiterals (Validate, fromLiteral, Lift)
import Data.Type.Set (Union, AsSet, Cmp)
import Data.Char (isSpace)
import Text.Parsec (ParsecT, Stream, try)
import Text.Parsec.Char (anyChar)
import GHC.TypeLits (Symbol, CmpSymbol)

import Functional.Reducible (Reducible(..))

type instance Cmp (a :: Symbol) (b :: Symbol) = CmpSymbol a b

-- Type for Chars that satisfy a list of requirements, used for free variables
newtype VChar (r :: [Symbol]) = VChar { fromVChar :: Char }
	deriving (Eq, Ord, Lift)

renderVChar :: VChar r -> String
renderVChar = pure . fromVChar

instance Show (VChar r) where
	showsPrec d (VChar x) = showParen (d > 10) $
		showString "VChar " . showsPrec 11 x

instance Reducible t (VChar r) where
	reducible _ = Nothing

-- No requirements means any Char is valid
instance Validate Char (VChar '[]) where
	fromLiteral = Just . VChar

-- Class of requirements for free variables
class Restriction (r :: Symbol) where
	-- Returns true if a char is not allowed by the requirement
	block :: Char -> Bool

-- Add a requirement to a list
instance (Restriction r, Validate Char (VChar rs)) =>
	Validate Char (VChar (r:rs)) where
		fromLiteral c
			| (block @r) c = Nothing
			| otherwise =
				VChar . fromVChar <$> (fromLiteral c :: Maybe (VChar rs))

-- Parser for any validated char
charParser :: (Validate Char t, Stream s m Char) => ParsecT s u m t
charParser = try $ do
	x <- anyChar
	case fromLiteral x of
		Just y -> pure y
		Nothing -> fail "Failed to validate char"

-- Loosen the requirements on a char. Using type applications to specify s and t
-- is recommended
release :: VChar (Union s t) -> VChar s
release = VChar . fromVChar

-- Convert a list of requirements to one without repeats, useful in combination
-- with release
toSet :: VChar s -> VChar (AsSet s)
toSet = VChar . fromVChar

-- Requirement that no parentheses are allowed
instance Restriction "NoParens" where
	block '(' = True
	block ')' = True
	block _ = False

-- Requirement that no whitespace is allowed
instance Restriction "NoWhitespace" where
	block = isSpace
