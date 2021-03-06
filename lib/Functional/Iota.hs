{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Functional.Iota
	( Iota(..)
	, toLambda
	, iotaParser
	, renderIota
	, fromSKI
	) where

import qualified Text.Parsec as P
import Control.Applicative ((<|>))

import Functional.SKI (SKI(..), HasSKI(..))
import Functional.Lambda (Lambda, abstract, free)
import Functional.Reducible (Reducible(..), Appliable(..))
import Functional.BinaryTree (BinaryTree)

-- Iota combinator \x.xSK
data Iota = Iota deriving (Eq, Ord, Show)

-- Representation in lambda calculus
toLambda :: HasSKI v => Iota -> Lambda v
toLambda Iota = abstract $ free Nothing $$ s $$ k

instance (Appliable t, HasSKI t) => Reducible t Iota where
	reducible Iota = Just (1, \case
		[x] -> x $$ s $$ k
		_ -> error "Wrong number of arguments"
		)

-- Parse ι or the empty string as Iota
iotaParser :: P.Stream s m Char => P.ParsecT s u m Iota
iotaParser = Iota <$ P.char 'ι' <|> pure Iota

renderIota :: Iota -> String
renderIota Iota = "ι"

-- Convert SKI calculus to Iota
fromSKI :: SKI -> BinaryTree Iota
fromSKI I = pure Iota $$ pure Iota
fromSKI K = pure Iota $$ (pure Iota $$ fromSKI I)
fromSKI S = pure Iota $$ fromSKI K
