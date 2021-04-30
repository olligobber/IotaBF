module Functional.Lambda.Typed.Semigroup
	( LambdaSemigroup
	, cat
	) where

import Functional.Lambda.Typed (TypedCombinator)

class LambdaSemigroup a where
	cat :: TypedCombinator (a -> a -> a)
