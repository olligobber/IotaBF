{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Functional.Lambda.Typed.FixedList
	( FixedList
	, unFix
	, nil
	, cons
	, append
	, head
	, last
	, tail
	, init
	, uncons
	, pure
	, ap
	, foldr
	, foldl
	, reverse
	, concat
	) where

import Prelude hiding
	( head, last, tail, init, pure, foldr, foldl, reverse, concat, id, map
	, const
	)
import GHC.TypeNats (Nat, type (+), type(-), type (<=), KnownNat, natVal)
import Data.Proxy (Proxy(Proxy))

import Functional.Lambda.Typed
	( TypedCombinator, TypedInput, TypedLambda
	, reType, abstract, toCombinator, ($$$), input, toLambda
	)
import qualified Functional.Lambda.Typed.List as L
import Functional.Lambda.Typed.Function (id, compose, const)
import Functional.Lambda.Typed.Functor (LambdaFunctor, map)
import Functional.Lambda.Typed.Eq (LambdaEq, eq)
import Functional.Lambda.Typed.Semigroup (cat)
import Functional.Lambda.Typed.Tuple (mkTuple2)
import Functional.Lambda.Typed.Maybe (fromMaybe)

-- List with a type level fixed length
data FixedList (n :: Nat) a

unFix :: TypedLambda (FixedList n a) v -> TypedLambda [a] v
unFix = reType

instance LambdaEq a => LambdaEq (FixedList n a) where
	eq = reType (eq :: TypedCombinator ([a] -> [a] -> Bool))

instance LambdaFunctor (FixedList n) where
	map = reType (map :: TypedCombinator ((a -> b) -> [a] -> [b]))

nil :: TypedCombinator (FixedList 0 a)
nil = reType L.empty

cons :: TypedCombinator (a -> FixedList n a -> FixedList (n+1) a)
cons = reType L.cons

append :: TypedCombinator (FixedList n a -> a -> FixedList (n+1) a)
append = reType L.append

head :: forall a n. 1 <= n => TypedCombinator (FixedList n a -> a)
head = reType headL where
	headL :: TypedCombinator ([a] -> a)
	headL = toCombinator $ abstract $
		(L.toFList input :: TypedInput 1 (a -> (a -> a -> a) -> a)) $$$
		(reType id :: TypedCombinator a) $$$
		const

last :: forall a n. 1 <= n => TypedCombinator (FixedList n a -> a)
last = reType lastL where
	lastL :: TypedCombinator ([a] -> a)
	lastL = toCombinator $ abstract $
		(L.toFList input :: TypedInput 1 (a -> (a -> a -> a) -> a)) $$$
		(reType id :: TypedCombinator a) $$$
		(const $$$ id :: TypedCombinator (b -> a -> a))

tail :: forall a n. 1 <= n =>
	TypedCombinator (FixedList n a -> FixedList (n-1) a)
tail = reType tailL where
	tailL :: TypedCombinator ([a] -> [a])
	tailL = compose $$$
		(fromMaybe $$$ L.empty) $$$
		L.tail

init :: forall a n. 1 <= n =>
	TypedCombinator (FixedList n a -> FixedList (n-1) a)
init = reType initL where
	initL :: TypedCombinator ([a] -> [a])
	initL = compose $$$
		(fromMaybe $$$ L.empty) $$$
		L.init

uncons :: forall a n. 1 <= n =>
	TypedCombinator (FixedList n a -> (a, FixedList (n-1) a))
uncons = toCombinator $ abstract $
	mkTuple2 $$$ (
		head $$$
		(input :: TypedInput 1 (FixedList n a))
	) $$$ (
		tail $$$
		(input :: TypedInput 1 (FixedList n a))
	)

pure :: forall n a. KnownNat n => TypedCombinator (a -> FixedList n a)
pure = reType $ L.replicate $$$ toLambda (natVal (Proxy :: Proxy n))

ap :: TypedCombinator (FixedList n (a -> b) -> FixedList n a -> FixedList n b)
ap = reType $ L.zipWith $$$ id

foldr :: TypedCombinator ((a -> b -> b) -> b -> FixedList n a -> b)
foldr = reType L.foldr

foldl :: TypedCombinator ((b -> a -> b) -> b -> FixedList n a -> b)
foldl = reType L.foldl

reverse :: TypedCombinator (FixedList n a -> FixedList n a)
reverse = reType L.reverse

concat :: TypedCombinator (FixedList m a -> FixedList n a -> FixedList (m+n) a)
concat = reType (cat :: TypedCombinator ([a] -> [a] -> [a]))

-- If you need elem then make a foldable class
