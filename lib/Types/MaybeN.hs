{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.MaybeN
	( MaybeN(..)
	) where

class MaybeN a b where
	justn :: a -> b

instance MaybeN a a where
	justn = id

instance MaybeN a b => MaybeN a (Maybe b) where
	justn = Just . justn
