{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module NatTypes
	( S(..)
	, Z
	, Positive
	, largest
	, type (|->)
	, convert
	, type (<=)
	, increase
	) where

import GHC.TypeNats (Nat, type (-))
import Data.Void (Void, absurd)

-- Successor of a type, adds one more value called Z and adjusts existing
-- values by applying S
data S x = S x | Z deriving (Eq, Ord, Show)

-- Zero type, has no values
type Z = Void

-- A type made of n S's applied to a Z has n values, representing zero to n-1

instance Functor S where
	fmap f (S x) = S $ f x
	fmap _ Z = Z

instance Applicative S where
	pure = S
	S f <*> S x = S $ f x
	Z <*> _ = Z
	_ <*> Z = Z

instance Monad S where
	S x >>= f = f x
	Z >>= _ = Z

instance Foldable S where
	foldMap f (S x) = f x
	foldMap _ Z = mempty

instance Traversable S where
	sequenceA (S x) = S <$> x
	sequenceA Z = pure Z

-- Convert a GHC Nat to a Peano Nat using Z and S
type family Peano (n :: Nat) where
	Peano 0 = Z
	Peano n = S (Peano (n - 1))

-- Positive Peano Nats n, which have some largest value n-1
class Positive n where
	largest :: n

instance Positive (S Z) where
	largest = Z

instance Positive (S x) => Positive (S (S x)) where
	largest = S largest

-- Injection from Peano Nats to larger types, allowing conversion that won't
-- change the value
class n |-> m where
	convert :: n -> m

instance Z |-> t where
	convert = absurd

instance n |-> m => S n |-> S m where
	convert = fmap convert

-- Ordering on Peano Nats, allowing conversion that will change the value
class n <= m where
	increase :: n -> m

instance n <= n where
	increase = id

instance n <= m => n <= S m where
	increase = S . increase
