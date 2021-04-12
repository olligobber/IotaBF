{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Functional.Iota.Free
	( IFree(fromIFree)
	, iFree
	, iFreeParser
	, renderIFree
	) where

import Data.Char (isSpace)
import ValidLiterals (Validate(..), Lift)
import Text.Parsec as P

import Functional.Reducible (Reducible(..))

-- TODO add other kinds of free variables for other languages

-- Free variables that can be safely be mixed with iota terms
newtype IFree = IFree { fromIFree :: Char } deriving (Eq, Ord, Show, Lift)

-- Checks if a char makes a valid free variable
iFree :: Char -> Maybe IFree
iFree '(' = Nothing -- Can't mix up free variables with brackets
iFree ')' = Nothing
iFree 'ι' = Nothing -- Can't mix up free variables with iota
iFree c | isSpace c = Nothing -- Whitespace is filtered out when parsing
iFree c = Just $ IFree c

-- Parser for free variables, to be used in making more complex parsers
iFreeParser :: P.Stream s m Char => P.ParsecT s u m IFree
iFreeParser = P.try $ do
	Just f <- iFree <$> anyChar
	pure f

-- Render a free variable as a string
renderIFree :: IFree -> String
renderIFree = pure . fromIFree

-- Allows free variables to be entered at compile time with template haskell
instance Validate Char IFree where
	fromLiteral = iFree

-- Free variables cannot be reduced
instance Reducible t IFree where
	reducible _ = Nothing
