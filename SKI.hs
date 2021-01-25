{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module SKI
	( SKI(..)
	, skiParser
	) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Reducible (Appliable(..), Reducible(..))

-- Combinators in SKI calculus
data SKI = S | K | I deriving (Eq, Ord, Show, Read)

instance Appliable t => Reducible t SKI where
	reducible S = Just (3,
		\case
			[x,y,z] -> x $$ z $$ (y $$ z)
			_ -> error "Wrong number of arguments"
		)
	reducible K = Just (2,
		\case
			[x,_] -> x
			_ -> error "Wrong number of arguments"
		)
	reducible I = Just (1,
		\case
			[x] -> x
			_ -> error "Wrong number of arguments"
		)

skiParser :: P.Stream s m Char => P.ParsecT s u m SKI
skiParser =
	S <$ P.char 'S' <|>
	K <$ P.char 'K' <|>
	I <$ P.char 'I'
