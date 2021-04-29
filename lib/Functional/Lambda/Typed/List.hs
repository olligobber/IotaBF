{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Functional.Lambda.Typed.List
	( FList
	, toFList
	, fromFList
	, cons
	, empty
	, concat
	, head
	, last
	, uncons
	, tail
	, init
	, singleton
	, null
	, foldr

	-- , foldl
	-- , reverse
	-- , repeat
	-- , cycle
	-- , elem
	-- , filter
	-- , zip
	-- , zipWith
	) where

import Prelude hiding
	(const, maybe, map, uncurry, concat, head, last, tail, init, null, foldr)

import Functional.Lambda.Typed
	( TypedInput, TypedCombinator, TypedLambda
	, input, liftInput, ($$$), abstract, toCombinator, toLambda, reType
	)
import Functional.Lambda.Typed.Function (const, compose)
import Functional.Lambda.Typed.Maybe (just, nothing, maybe)
import Functional.Lambda.Typed.Tuple (mkTuple2, get2of2, uncurry)
import Functional.Lambda.Typed.Functor (map)

-- Functional equivalent of list type, basically does foldr
type FList a b = b -> (a -> b -> b) -> b

toFList :: TypedLambda [a] v -> TypedLambda (FList a b) v
toFList = reType

fromFList :: TypedLambda (FList a b) v -> TypedLambda [a] v
fromFList = reType

-- TODO instances

cons :: forall a. TypedCombinator (a -> [a] -> [a])
cons = reType consF where
	consF :: forall b. TypedCombinator (a -> FList a b -> FList a b)
	consF = toCombinator $ abstract $ abstract $ abstract $ abstract $
		liftInput (input :: TypedInput 1 (a -> b -> b)) $$$
		(input :: TypedInput 4 a) $$$ (
			liftInput (input :: TypedInput 3 (FList a b)) $$$
			liftInput (input :: TypedInput 2 b) $$$
			liftInput (input :: TypedInput 1 (a -> b -> b))
		)

empty :: forall a. TypedCombinator [a]
empty = fromFList emptyF where
	emptyF :: forall b c. TypedCombinator (b -> c -> b)
	emptyF = const

concat :: forall a. TypedCombinator ([a] -> [a] -> [a])
concat = reType concatF where
	concatF :: forall b. TypedCombinator (FList a b -> FList a b -> FList a b)
	concatF = toCombinator $ abstract $ abstract $ abstract $ abstract $
		(input :: TypedInput 4 (FList a b)) $$$ (
			liftInput (input :: TypedInput 3 (FList a b)) $$$
			liftInput (input :: TypedInput 2 b) $$$
			liftInput (input :: TypedInput 1 (a -> b -> b))
		) $$$
		liftInput (input :: TypedInput 1 (a -> b -> b))

head :: forall a. TypedCombinator ([a] -> Maybe a)
head = toCombinator $ abstract $
	toFList (input :: TypedInput 1 [a]) $$$
	nothing $$$
	(compose $$$ const $$$ just)

last :: forall a. TypedCombinator ([a] -> Maybe a)
last = toCombinator $ abstract $
	toFList (input :: TypedInput 1 [a]) $$$
	nothing $$$
	abstract (
		maybe $$$ (
			just $$$
			liftInput (input :: TypedInput 1 a)
		) $$$
		just
	)

uncons :: forall a. TypedCombinator ([a] -> Maybe (a, [a]))
uncons = toCombinator $ abstract $
	toFList (input :: TypedInput 1 [a]) $$$
	nothing $$$
	abstract (
		maybe $$$ (
			just $$$ (
				mkTuple2 $$$
				liftInput (input :: TypedInput 1 a) $$$
				empty
			)
		) $$$
		abstract (
			just $$$ (
				mkTuple2 $$$
				liftInput (input :: TypedInput 2 a) $$$ (
					uncurry $$$
					cons $$$
					liftInput (input :: TypedInput 1 (a,[a]))
				)
			)
		)
	)

tail :: forall a. TypedCombinator ([a] -> Maybe [a])
tail = compose $$$ (map $$$ get2of2) $$$ uncons

init :: forall a. TypedCombinator ([a] -> Maybe [a])
init = toCombinator $ abstract $
	toFList (input :: TypedInput 1 [a]) $$$
	nothing $$$
	abstract (
		maybe $$$ (
			just $$$ empty
		) $$$ (
			compose $$$
			just $$$ (
				cons $$$
				liftInput (input :: TypedInput 1 a)
			)
		)
	)

singleton :: forall a. TypedCombinator (a -> [a])
singleton = reType singletonF where
	singletonF :: forall b c. TypedCombinator (a -> b -> (a -> b -> c) -> c)
	singletonF = toCombinator $ abstract $ abstract $ abstract $
		liftInput (input :: TypedInput 1 (a -> b -> c)) $$$
		(input :: TypedInput 3 a) $$$
		liftInput (input :: TypedInput 2 b)

null :: forall a. TypedCombinator ([a] -> Bool)
null = toCombinator $ abstract $
	toFList (input :: TypedInput 1 [a]) $$$
	toLambda False $$$ (
		const $$$ (
			const $$$
			toLambda True
		)
	)

foldr :: forall a b. TypedCombinator ((a -> b -> b) -> b -> [a] -> b)
foldr = toCombinator $ abstract $ abstract $ abstract $
	toFList (liftInput (input :: TypedInput 1 [a])) $$$
	liftInput (input :: TypedInput 2 b) $$$
	(input :: TypedInput 3 (a -> b -> b))

-- todo reverse, foldl, and the rest
