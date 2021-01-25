{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Reducible
	( Appliable(..)
	, Reducible(..)
	, Var(..)
	) where

import Data.Void (Void, absurd)

infixl 5 $$

-- Types that support some notion of application
-- Most instances of Reducible are also Appliable
class Appliable t where
	($$) :: t -> t -> t

-- Types that can act as functions over some other type
class Reducible t a where
	-- Number of inputs to the function and the reduction function,
	-- or Nothing if it is irreducible
	-- The reduction function should fail if given the wrong number of inputs
	reducible :: a -> Maybe (Integer, [t] -> t)

instance Reducible t Void where
	reducible = absurd

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

instance Reducible t (Var x) where
	reducible _ = Nothing

instance (Reducible t x, Reducible t y) => Reducible t (Either x y) where
	reducible (Left x) = reducible x
	reducible (Right x) = reducible x
