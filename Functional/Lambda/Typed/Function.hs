{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Functional.Lambda.Typed.Function
	( id
	, const
	, compose
	, flip
	, apply
	) where

import Prelude hiding (id, const, flip)

import Functional.Lambda.Typed
	(TypedCombinator, TypedInput, input, lift, abstract, toCombinator, ($$$))

id :: forall a. TypedCombinator (a -> a)
id = toCombinator $ abstract (input :: TypedInput 1 a)

const :: forall a b. TypedCombinator (a -> b -> a)
const = toCombinator $ abstract $ abstract (input :: TypedInput 2 a)

-- AKA (.)
compose :: forall a b c. TypedCombinator ((b -> c) -> (a -> b) -> a -> c)
compose = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 (b -> c)) $$$
	(
		lift (input :: TypedInput 2 (a -> b)) $$$
		lift (input :: TypedInput 1 a)
	)

flip :: forall a b c. TypedCombinator ((a -> b -> c) -> b -> a -> c)
flip = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 (a -> b -> c)) $$$
	lift (input :: TypedInput 1 a) $$$
	lift (input :: TypedInput 2 b)

-- AKA ($)
apply :: forall a b. TypedCombinator ((a -> b) -> a -> b)
apply = toCombinator $ abstract $ abstract $
	(input :: TypedInput 2 (a -> b)) $$$
	lift (input :: TypedInput 1 a)

-- todo pipe (AKA (&)), fix, on,
