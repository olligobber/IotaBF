{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.MaybeN
	( MaybeN(..)
	) where

-- N applications of a gives b
class MaybeN a b where
	-- N applications of Just
	justn :: a -> b

-- N = 0
instance MaybeN a a where
	justn = id

-- N = N+1
instance MaybeN a b => MaybeN a (Maybe b) where
	justn = Just . justn
