{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Iota.Free
	( IFree
	, fromIFree
	, iFree
	, iFreeParser
	, renderIFree
	) where

import ValidLiterals (fromLiteral)
import Text.Parsec as P
import Data.Type.Set (AsSet)

import Functional.Free
	(Free, Restriction, fromFree, renderFree, block, charParser)

instance Restriction "NoIota" where
	block = (== 'ι')

type IotaSafe = AsSet '["NoIota", "NoWhitespace", "NoParens"]

type IFree = Free IotaSafe

fromIFree :: IFree -> Char
fromIFree = fromFree

-- Checks if a char makes a valid free variable
iFree :: Char -> Maybe IFree
iFree = fromLiteral

-- Parser for free variables, to be used in making more complex parsers
iFreeParser :: P.Stream s m Char => P.ParsecT s u m IFree
iFreeParser = charParser

-- Render a free variable as a string
renderIFree :: IFree -> String
renderIFree = renderFree
