{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Reducible
	( Reducible(..)
	, Var(..)
	) where

-- Types that can act as functions, taking in inputs (a) to produce a tree (t)
class Reducible t a | t -> a where
	-- Number of inputs to the function and the reduction function,
	-- or Nothing if it is irreducible
	-- The reduction function will fail if given the wrong number of inputs
	reducible :: a -> Maybe (Integer, [t] -> t)

-- A type that cannot be reduced
newtype Var x = Var { getVar :: x } deriving (Eq, Ord)

instance Show x => Show (Var x) where
	showsPrec d (Var x) = showParen (d > 10) $
		showString "Var " . showsPrec 11 x

instance Functor Var where
	fmap f (Var x) = Var $ f x

instance Foldable Var where
	foldMap f (Var x) = f x

instance Traversable Var where
	sequenceA (Var x) = Var <$> x

instance Applicative Var where
	pure = Var
	Var f <*> Var x = Var $ f x

instance Monad Var where
	Var x >>= f = f x

instance Reducible (t (Var x)) (Var x) where
	reducible _ = Nothing
