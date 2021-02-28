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

-- Free variables that can be safely be mixed with iota terms
newtype IFree = IFree { fromIFree :: Char } deriving (Eq, Ord, Show, Lift)

iFree :: Char -> Maybe IFree
iFree '(' = Nothing
iFree ')' = Nothing
iFree 'ι' = Nothing
iFree c | isSpace c = Nothing
iFree c = Just $ IFree c

iFreeParser :: P.Stream s m Char => P.ParsecT s u m IFree
iFreeParser = P.try $ do
	Just f <- iFree <$> anyChar
	pure f

renderIFree :: IFree -> String
renderIFree = pure . fromIFree

instance Validate Char IFree where
	fromLiteral = iFree

instance Reducible t IFree where
	reducible _ = Nothing
