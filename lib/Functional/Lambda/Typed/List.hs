{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Functional.Lambda.Typed.List
	( FList
	, toFList
	, fromFList
	, cons
	, empty
	, append
	, head
	, last
	, uncons
	, tail
	, init
	, singleton
	, null
	, foldr
	, foldl
	, reverse
	, repeat
	, cycle
	, elem
	, filter
	, zipWith
	, zip
	) where

import Prelude hiding
	( const, maybe, map, uncurry, head, last, tail, init, null, foldr, reverse
	, flip, id, foldl, repeat, cycle, elem, or, filter, zipWith, zip, and, show
	)
import qualified Prelude as P
import ValidLiterals (valid)

import Functional.Lambda.Typed
	( TypedInput, TypedCombinator, TypedLambda, Representable
	, input, liftInput, ($$$), abstract, toCombinator, toLambda, reType
	, fromTyped, liftFree
	)
import Functional.Lambda.Typed.Function (const, compose, flip, id, fix)
import Functional.Lambda.Typed.Maybe (just, nothing, maybe, toFMaybe)
import Functional.Lambda.Typed.Tuple (mkTuple2, get2of2, uncurry)
import Functional.Lambda.Typed.Functor (LambdaFunctor, map)
import Functional.Lambda.Typed.Eq (LambdaEq, eq)
import Functional.Lambda.Typed.Bool (or, toFBool, and)
import Functional.Decode (Decode(..))
import Functional.Reducible (($$))
import Functional.Lambda.Typed.Render (LambdaShow, show, TypedRenderS, RenderS)
import Functional.Lambda.Typed.Semigroup (LambdaSemigroup, cat)

-- Functional equivalent of list type, basically does foldr
type FList a b = b -> (a -> b -> b) -> b

toFList :: TypedLambda [a] v -> TypedLambda (FList a b) v
toFList = reType

fromFList :: TypedLambda (FList a b) v -> TypedLambda [a] v
fromFList = reType

instance LambdaEq a => LambdaEq [a] where
	eq = toCombinator $ fix $$$
		abstract (abstract $ abstract $
			toFMaybe (
				uncons $$$
				liftInput (input :: TypedInput 2 [a])
			) $$$ (
				null $$$
				liftInput (input :: TypedInput 1 [a])
			) $$$ (
				uncurry $$$
				abstract (abstract $
					toFMaybe (
						uncons $$$
						liftInput (input :: TypedInput 3 [a])
					) $$$
					toLambda False $$$ (
						uncurry $$$
						abstract (abstract $
							and $$$ (
								eq $$$
								liftInput (input :: TypedInput 4 a) $$$
								liftInput (input :: TypedInput 2 a)
							) $$$ (
								(input :: TypedInput 7 ([a] -> [a] -> Bool)) $$$
								liftInput (input :: TypedInput 3 [a]) $$$
								liftInput (input :: TypedInput 1 [a])
							)
						)
					)
				)
			)
		)

instance Representable a => Representable [a] where
	toLambda l = fromFList $ toCombinator $ abstract $ abstract $
		P.foldr
			(\a b ->
				liftInput (input :: TypedInput 1 (a -> b -> b)) $$$
				toLambda a $$$
				b
			)
			(input :: TypedInput 2 b)
			l

instance Decode a => Decode [a] where
	decodeLambda lambda = case
		decodeLambda (fromTyped uncons $$ lambda) :: Maybe (Maybe (a, [a]))
		of
			Nothing -> Nothing
			Just Nothing -> Just []
			Just (Just (x, xs)) -> Just $ x:xs

instance LambdaShow a => LambdaShow [a] where
	show = abstract $
		toFMaybe (
			toFList (liftInput (input :: TypedInput 1 [a])) $$$
			nothing $$$
			abstract (
				maybe $$$ (
					just $$$ (
						cat $$$ (
							liftFree show $$$
							liftInput (input :: TypedInput 1 a)
						) $$$
						liftFree ($$(valid "]") :: TypedRenderS)
					)
				) $$$
				abstract (
					just $$$ (
						cat $$$ (
							cat $$$ (
								liftFree show $$$
								liftInput (input :: TypedInput 2 a)
							) $$$
							liftFree ($$(valid ",") :: TypedRenderS)
						) $$$
						liftInput (input :: TypedInput 1 RenderS)
					)
				)
			)
		) $$$
		liftFree ($$(valid "List[]") :: TypedRenderS) $$$ (
			cat $$$
			liftFree ($$(valid "List[") :: TypedRenderS)
		)

instance LambdaFunctor [] where
	map :: forall a b. TypedCombinator ((a -> b) -> [a] -> [b])
	map = toCombinator $ abstract $ abstract $ fromFList $ abstract $ abstract $
		toFList (liftInput (input :: TypedInput 3 [a])) $$$
		liftInput (input :: TypedInput 2 c) $$$ (
			compose $$$
			liftInput (input :: TypedInput 1 (b -> c -> c)) $$$
			(input :: TypedInput 4 (a -> b))
		)

instance LambdaSemigroup [a] where
	cat = reType catF where
		catF:: forall b. TypedCombinator (FList a b -> FList a b -> FList a b)
		catF = toCombinator $ abstract $ abstract $ abstract $ abstract $
			(input :: TypedInput 4 (FList a b)) $$$ (
				liftInput (input :: TypedInput 3 (FList a b)) $$$
				liftInput (input :: TypedInput 2 b) $$$
				liftInput (input :: TypedInput 1 (a -> b -> b))
			) $$$
			liftInput (input :: TypedInput 1 (a -> b -> b))

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

append :: forall a. TypedCombinator ([a] -> a -> [a])
append = reType appendF where
	appendF :: forall b. TypedCombinator (FList a b -> a -> FList a b)
	appendF = toCombinator $ abstract $ abstract $ abstract $ abstract $
		(input :: TypedInput 4 (FList a b)) $$$ (
			liftInput (input :: TypedInput 1 (a -> b -> b)) $$$
			liftInput (input :: TypedInput 3 a) $$$
			liftInput (input :: TypedInput 2 b)
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

foldl :: forall a b. TypedCombinator ((b -> a -> b) -> b -> [a] -> b)
foldl = toCombinator $ abstract $ abstract $ abstract $
	toFList (liftInput (input :: TypedInput 1 [a])) $$$
	id $$$
	abstract ( abstract $ abstract $
		liftInput (input :: TypedInput 2 (b -> b)) $$$ (
			(input :: TypedInput 6 (b -> a -> b)) $$$
			liftInput (input :: TypedInput 1 b) $$$
			liftInput (input :: TypedInput 3 a)
		)
	) $$$
	liftInput (input :: TypedInput 2 b)

reverse :: forall a. TypedCombinator ([a] -> [a])
reverse = toCombinator $ abstract $
	toFList (input :: TypedInput 1 [a]) $$$
	empty $$$
	(flip $$$ append)

repeat :: forall a. TypedCombinator (a -> [a])
repeat = reType repeatF where
	repeatF :: forall b. TypedCombinator (a -> FList a b)
	repeatF = toCombinator $ abstract $ abstract $ abstract $
		fix $$$ (
			liftInput (input :: TypedInput 1 (a -> b -> b)) $$$
			(input :: TypedInput 3 a)
		)

cycle :: forall a. TypedCombinator ([a] -> [a])
cycle = reType cycleF where
	cycleF :: forall b. TypedCombinator (FList a b -> FList a b)
	cycleF = toCombinator $ abstract $ abstract $ abstract $
		fix $$$ (
			flip $$$
			(input :: TypedInput 3 (FList a b)) $$$
			liftInput (input :: TypedInput 1 (a -> b -> b))
		)

elem :: forall a. LambdaEq a => TypedCombinator (a -> [a] -> Bool)
elem = toCombinator $ abstract $ abstract $
	toFList (liftInput (input :: TypedInput 1 [a])) $$$
	toLambda False $$$
	abstract (abstract $
		or $$$
		liftInput (input :: TypedInput 1 Bool) $$$ (
			eq $$$
			(input :: TypedInput 4 a) $$$
			liftInput (input :: TypedInput 2 a)
		)
	)

filter :: forall a. TypedCombinator ((a -> Bool) -> [a] -> [a])
filter = reType filterF where
	filterF :: forall b. TypedCombinator ((a -> Bool) -> FList a b -> FList a b)
	filterF = toCombinator $ abstract $ abstract $ abstract $ abstract $
		liftInput (input :: TypedInput 3 (FList a b)) $$$
		liftInput (input :: TypedInput 1 b) $$$
		abstract (
			toFBool (
				(input :: TypedInput 5 (a -> Bool)) $$$
				liftInput (input :: TypedInput 1 a)
			) $$$ (
				liftInput (input :: TypedInput 3 (a -> b -> b)) $$$
				liftInput (input :: TypedInput 1 a)
			) $$$
			id
		)

-- TODO reimplement without fix
zipWith :: forall a b c. TypedCombinator ((a -> b -> c) -> [a] -> [b] -> [c])
zipWith = toCombinator $ abstract $
	fix $$$ abstract ( abstract $ abstract $
		toFMaybe (
			uncons $$$
			liftInput (input :: TypedInput 2 [a])
		) $$$
		empty $$$ (
			uncurry $$$
			abstract (abstract $
				toFMaybe (
					uncons $$$
					liftInput (input :: TypedInput 3 [b])
				) $$$
				empty $$$ (
					uncurry $$$
					abstract (abstract $
						cons $$$ (
							(input :: TypedInput 8 (a -> b -> c)) $$$
							liftInput (input :: TypedInput 4 a) $$$
							liftInput (input :: TypedInput 2 b)
						) $$$ (
							liftInput
								(input :: TypedInput 7 ([a] -> [b] -> [c])) $$$
							liftInput (input :: TypedInput 3 [a]) $$$
							liftInput (input :: TypedInput 1 [b])
						)
					)
				)
			)
		)
	)

zip :: forall a b. TypedCombinator ([a] -> [b] -> [(a,b)])
zip = zipWith $$$ mkTuple2
