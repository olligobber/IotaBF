module Functional.Lambda.Typed.Functor
	( LambdaFunctor(..)
	) where

import Prelude hiding (map, const)

import Functional.Lambda.Typed (TypedCombinator, ($$$))
import Functional.Lambda.Typed.Function (const, compose)

class LambdaFunctor f where
	map :: TypedCombinator ((a -> b) -> f a -> f b)
	replace :: TypedCombinator (a -> f b -> f a)
	replace = compose $$$ map $$$ const

instance LambdaFunctor ((->) a) where
	map = compose
	replace = compose $$$ const $$$ const
