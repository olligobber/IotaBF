{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module SKI
	( SKI(..)
	) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P

import BinaryTree (BinaryTree((:^:)))
import Reducible (Reducible(..))

data SKI = S | K | I deriving (Eq, Ord, Show, Read)

instance Reducible (BinaryTree SKI) SKI where
	reducible S = Just (3, \[x,y,z] -> (x :^: z) :^: (y :^: z))
	reducible K = Just (2, \[x,y] -> x)
	reducible I = Just (1, \[x] -> x)

skiParser :: P.Stream s m Char => P.ParsecT s u m SKI
skiParser =
	S <$ P.char 'S' <|>
	K <$ P.char 'K' <|>
	I <$ P.char 'I'
