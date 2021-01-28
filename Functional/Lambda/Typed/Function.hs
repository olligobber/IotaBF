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
	(TypedCombinator, TypedInput, free, abstract, ($$$))

id :: forall a. TypedCombinator (a -> a)
id = abstract (free Nothing :: TypedInput 1 a)

const :: forall a b. TypedCombinator (a -> b -> a)
const = abstract $ abstract (free $ Just Nothing :: TypedInput 2 a)

compose :: forall a b c. TypedCombinator ((b -> c) -> (a -> b) -> a -> c)
compose = abstract $ abstract $ abstract $
	(free $ Just $ Just Nothing :: TypedInput 3 (b -> c)) $$$ (
		(free $ Just Nothing :: TypedInput 2 (a -> b)) $$$
		(free Nothing :: TypedInput 1 a)
	)

flip :: forall a b c. TypedCombinator ((a -> b -> c) -> b -> a -> c)
flip = abstract $ abstract $ abstract $
	(free $ Just $ Just Nothing :: TypedInput 3 (a -> b -> c)) $$$
	(free Nothing :: TypedInput 1 a) $$$
	(free $ Just Nothing :: TypedInput 2 b)

apply :: forall a b. TypedCombinator ((a -> b) -> a -> b)
apply = abstract $ abstract $
	(free $ Just Nothing :: TypedInput 2 (a -> b)) $$$
	(free Nothing :: TypedInput 1 a)

-- todo pipe, fix, on,
