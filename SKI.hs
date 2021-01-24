{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module SKI
	( SKI(..)
	, skiParser
	) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import Reducible (Appliable(..), Reducible(..))

data SKI = S | K | I deriving (Eq, Ord, Show, Read)

instance Appliable t => Reducible t SKI where
	reducible S = Just (3,
		\l -> case l of
			[x,y,z] -> (x $$ z) $$ (y $$ z)
			_ -> error "Wrong number of arguments"
		)
	reducible K = Just (2,
		\l -> case l of
			[x,_] -> x
			_ -> error "Wrong number of arguments"
		)
	reducible I = Just (1,
		\l -> case l of
			[x] -> x
			_ -> error "Wrong number of arguments"
		)

skiParser :: P.Stream s m Char => P.ParsecT s u m SKI
skiParser =
	S <$ P.char 'S' <|>
	K <$ P.char 'K' <|>
	I <$ P.char 'I'
