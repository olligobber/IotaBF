{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functional.Lambda.Typed.Eq
	( LambdaEq(..)
	, neq
	) where

import Prelude hiding (not)

import Functional.Lambda.Typed
	( TypedCombinator, TypedInput
	, ($$$), toLambda, input, abstract, liftInput, toCombinator
	)
import Functional.Lambda.Typed.Bool (toFBool, not)

class LambdaEq t where
	eq :: TypedCombinator (t -> t -> Bool)

neq :: forall t. LambdaEq t => TypedCombinator (t -> t -> Bool)
neq = toCombinator $ abstract $ abstract $
	not $$$
	(
		(eq :: TypedCombinator (t -> t -> Bool)) $$$
		(input :: TypedInput 2 t) $$$
		liftInput (input :: TypedInput 1 t)
	)

instance LambdaEq Bool where
	eq = toCombinator $ abstract $ abstract $
		toFBool (input :: TypedInput 2 Bool) $$$
		liftInput (input :: TypedInput 1 Bool) $$$
		(
			toFBool (liftInput (input :: TypedInput 1 Bool)) $$$
			(input :: TypedInput 2 Bool) $$$
			toLambda True
		)
