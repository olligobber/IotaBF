{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Functional.Lambda.Typed.Function
	( id
	, const
	, compose
	, flip
	, apply
	, pipe
	, fix
	, on
	) where

import Prelude hiding (id, const, flip)

import Functional.Lambda.Typed
	( TypedCombinator, TypedInput, TypedLambda
	, input, liftInput, abstract, toCombinator, ($$$), reType
	)

id :: forall a. TypedCombinator (a -> a)
id = toCombinator $ abstract (input :: TypedInput 1 a)

const :: forall a b. TypedCombinator (a -> b -> a)
const = toCombinator $ abstract $ abstract (input :: TypedInput 2 a)

-- AKA (.)
compose :: forall a b c. TypedCombinator ((b -> c) -> (a -> b) -> a -> c)
compose = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 (b -> c)) $$$
	(
		liftInput (input :: TypedInput 2 (a -> b)) $$$
		liftInput (input :: TypedInput 1 a)
	)

flip :: forall a b c. TypedCombinator ((a -> b -> c) -> b -> a -> c)
flip = toCombinator $ abstract $ abstract $ abstract $
	(input :: TypedInput 3 (a -> b -> c)) $$$
	liftInput (input :: TypedInput 1 a) $$$
	liftInput (input :: TypedInput 2 b)

-- AKA ($)
apply :: forall a b. TypedCombinator ((a -> b) -> a -> b)
apply = id

-- AKA (&)
pipe :: forall a b. TypedCombinator (a -> (a -> b) -> b)
pipe = toCombinator $ abstract $ abstract $
	liftInput (input :: TypedInput 1 (a -> b)) $$$
	(input :: TypedInput 2 a)

-- Add a recursive type so the Y combinator can type check
data F a -- = F a -> a
toF :: TypedLambda (F a -> a) v -> TypedLambda (F a) v
toF = reType
fromF :: TypedLambda (F a) v -> TypedLambda (F a -> a) v
fromF = reType

-- Y combinator
fix :: forall a. TypedCombinator ((a -> a) -> a)
fix = toCombinator $ abstract $
	abstract (
		fromF (liftInput (input :: TypedInput 1 (F a))) $$$
		liftInput (input :: TypedInput 1 (F a))
	) $$$
	toF (abstract $
		(input :: TypedInput 2 (a -> a)) $$$
		(
			fromF (liftInput (input :: TypedInput 1 (F a))) $$$
			liftInput (input :: TypedInput 1 (F a))
		)
	)

on :: forall a b c. TypedCombinator ((b -> b -> c) -> (a -> b) -> a -> a -> c)
on = toCombinator $ abstract $ abstract $ abstract $ abstract $
	(input :: TypedInput 4 (b -> b -> c)) $$$
	(
		liftInput (input :: TypedInput 3 (a -> b)) $$$
		liftInput (input :: TypedInput 2 a)
	) $$$
	(
		liftInput (input :: TypedInput 3 (a -> b)) $$$
		liftInput (input :: TypedInput 1 a)
	)
