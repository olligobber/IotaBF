{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Nat (
	Peano,
	Positive(..),
	type (<=)(..)
	) where

import GHC.TypeNats (Nat, type (-))
import Data.Void (Void, absurd)

-- Convert a GHC Nat to a Peano Nat using Zero = Void and Succ n = Maybe n
type family Peano (n :: Nat) where
	Peano 0 = Void
	Peano n = Maybe (Peano (n - 1))

-- Positive Peano Nats
class Positive n where
	-- Member of type n with the most Justs possible
	maxmem :: n

instance Positive (Maybe Void) where
	maxmem = Nothing

instance Positive (Maybe n) => Positive (Maybe (Maybe n)) where
	maxmem = Just maxmem

-- Ordering on Peano Nats
class n <= m where
	-- Injection from one type to the other without changing its value
	generalise :: n -> m

instance Void <= m where
	generalise = absurd

instance n <= m => Maybe n <= Maybe m where
	generalise = fmap generalise
